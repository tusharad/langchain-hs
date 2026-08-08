{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Agents.Supervisor
Description : Central supervisor agent for multi-agent orchestration
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Central supervisor agent that orchestrates the multi-agent pipeline.
Routes tasks to appropriate worker agents based on pipeline phase,
manages overall workflow, and handles error escalation and retry decisions.
Integrates with langchain-hs-graph's supervisor pattern.
-}
module Aegis.Agents.Supervisor
  ( -- * Supervisor
    supervisorNode
  , routeNextAgent
  , SupervisorDecision (..)

    -- * Pipeline Control
  , shouldContinue
  , shouldRetry
  , isTerminalPhase
  , nextPhase

    -- * Report Generation
  , generateReport
  , commitNode
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Langchain.Core.Model.Types (Message, assistantMessage, systemMessage, extractMessageText)

import Aegis.Core.Types.Pipeline
import Aegis.Core.Types.Security
import Aegis.Middleware.Telemetry (TelemetrySystem, emitInfo, emitWarning, emitError, emitNodeStart, emitNodeEnd)
import Aegis.Tools.Git (runGitCommand)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Decision made by the supervisor for routing
data SupervisorDecision
  = RouteToIndexer
  -- ^ Route to codebase indexing
  | RouteToTriage
  -- ^ Route to vulnerability triage
  | RouteToRefactor
  -- ^ Route to patch generation
  | RouteToVerify
  -- ^ Route to verification/testing
  | RouteToHITL
  -- ^ Route to human review
  | RouteToCommit
  -- ^ Route to commit
  | RouteToReport
  -- ^ Route to final report generation
  | RouteComplete
  -- ^ Pipeline complete
  | RouteFailed Text
  -- ^ Pipeline failed
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Supervisor Node
-- ---------------------------------------------------------------------------

-- | Main supervisor node that determines the next step in the pipeline
supervisorNode
  :: Maybe TelemetrySystem
  -> AegisState
  -> IO AegisState
supervisorNode mbTelemetry state = do
  mapM_ (\ts -> emitNodeStart ts "Supervisor") mbTelemetry

  let decision = routeNextAgent state

  mapM_ (\ts -> emitInfo ts "Supervisor"
    ("Routing decision: " <> T.pack (show decision))) mbTelemetry

  let nextState = case decision of
        RouteToIndexer -> state { statePhase = PhaseIndexing }
        RouteToTriage -> state { statePhase = PhaseTriaging }
        RouteToRefactor -> state { statePhase = PhaseRefactoring }
        RouteToVerify -> state { statePhase = PhaseVerifying }
        RouteToHITL -> state { statePhase = PhaseAwaitingApproval }
        RouteToCommit -> state { statePhase = PhaseCommitting }
        RouteToReport -> state { statePhase = PhaseReporting }
        RouteComplete -> state { statePhase = PhaseCompleted }
        RouteFailed reason -> state { statePhase = PhaseFailed reason }

  let nextState' = nextState
        { stateMessages =
            [assistantMessage $ "[Supervisor] Routing to: " <> T.pack (show decision)]
        , stateEventLog = stateEventLog state ++
            [logEvent (statePhase nextState) "Supervisor"
              ("Routed to " <> T.pack (show decision)) EventInfo]
        , stateIterationCount = stateIterationCount state + 1
        }

  mapM_ (\ts -> emitNodeEnd ts "Supervisor" 0.0) mbTelemetry

  pure nextState'

-- ---------------------------------------------------------------------------
-- Routing Logic
-- ---------------------------------------------------------------------------

-- | Determine which agent to route to next based on current state
routeNextAgent :: AegisState -> SupervisorDecision
routeNextAgent state = case statePhase state of
  -- Initial state: start with indexing
  PhaseInitializing -> RouteToIndexer

  -- After indexing: triage
  PhaseIndexing -> RouteToTriage

  -- After triage: refactor if there are vulnerabilities
  PhaseTriaging
    | null (stateVulnerabilities state) -> RouteToReport
    | otherwise -> RouteToRefactor

  -- After refactoring: verify
  PhaseRefactoring
    | stateCurrentPatch state == Nothing -> RouteToReport  -- No patch generated
    | otherwise -> RouteToVerify

  -- After verification: HITL or next vulnerability
  PhaseVerifying
    | shouldRetry state -> RouteToRefactor
    | otherwise -> RouteToHITL

  -- After HITL approval: commit
  PhaseAwaitingApproval -> case stateApprovalStatus state of
    Approved _ -> RouteToCommit
    Rejected _ ->
      if shouldRetry state
      then RouteToRefactor
      else RouteToReport
    ModificationRequested _ -> RouteToRefactor
    _ -> RouteToHITL  -- Still pending

  -- After commit: check for more vulnerabilities or report
  PhaseCommitting
    | null (stateVulnerabilities state) -> RouteToReport
    | otherwise -> RouteToRefactor

  -- Report and done
  PhaseReporting -> RouteComplete
  PhaseCompleted -> RouteComplete
  PhaseFailed reason -> RouteFailed reason

-- | Check if the pipeline should continue processing
shouldContinue :: AegisState -> Bool
shouldContinue state = not (isTerminalPhase (statePhase state))
  && stateIterationCount state < 100  -- Safety limit

-- | Check if a retry is warranted
shouldRetry :: AegisState -> Bool
shouldRetry state =
  stateRetryCount state < stateMaxRetries state
  && case stateTestResults state of
       [] -> False
       results -> testOutcome (last results) /= TestPassed

-- | Check if a phase is terminal (no more processing)
isTerminalPhase :: PipelinePhase -> Bool
isTerminalPhase PhaseCompleted = True
isTerminalPhase (PhaseFailed _) = True
isTerminalPhase _ = False

-- | Get the logical next phase (for sequential pipelines)
nextPhase :: PipelinePhase -> PipelinePhase
nextPhase PhaseInitializing = PhaseIndexing
nextPhase PhaseIndexing = PhaseTriaging
nextPhase PhaseTriaging = PhaseRefactoring
nextPhase PhaseRefactoring = PhaseVerifying
nextPhase PhaseVerifying = PhaseAwaitingApproval
nextPhase PhaseAwaitingApproval = PhaseCommitting
nextPhase PhaseCommitting = PhaseReporting
nextPhase PhaseReporting = PhaseCompleted
nextPhase p = p

-- ---------------------------------------------------------------------------
-- Commit Node
-- ---------------------------------------------------------------------------

-- | StateGraph node that commits approved patches
commitNode
  :: Maybe TelemetrySystem
  -> AegisState
  -> IO AegisState
commitNode mbTelemetry state = do
  mapM_ (\ts -> emitNodeStart ts "CommitNode") mbTelemetry

  case stateCurrentPatch state of
    Nothing -> do
      mapM_ (\ts -> emitWarning ts "CommitNode" "No patch to commit") mbTelemetry
      pure state
        { statePhase = PhaseReporting
        , stateEventLog = stateEventLog state ++
            [logEvent PhaseCommitting "CommitNode" "No patch to commit" EventWarning]
        }

    Just patch -> do
      let repoPath = stateRepoPath state
          commitMsg = "aegis: " <> patchDescription patch
            <> maybe "" (\vid -> " [" <> vid <> "]") (patchVulnerabilityId patch)

      -- Stage all changes
      stageResult <- runGitCommand repoPath ["add", "-A"]
      case stageResult of
        Left err -> do
          mapM_ (\ts -> emitError ts "CommitNode" ("Staging failed: " <> err)) mbTelemetry
          pure state
            { statePhase = PhaseFailed ("Commit staging failed: " <> err)
            , stateErrors = stateErrors state ++ ["Commit staging failed: " <> err]
            }

        Right _ -> do
          -- Commit
          commitResult <- runGitCommand repoPath ["commit", "-m", T.unpack commitMsg]
          case commitResult of
            Left err -> do
              mapM_ (\ts -> emitError ts "CommitNode" ("Commit failed: " <> err)) mbTelemetry
              pure state
                { statePhase = PhaseFailed ("Commit failed: " <> err)
                , stateErrors = stateErrors state ++ ["Commit failed: " <> err]
                }

            Right output -> do
              mapM_ (\ts -> emitInfo ts "CommitNode" ("Committed: " <> commitMsg)) mbTelemetry

              -- Update finding status
              let updatedFindings = map (\f ->
                    if findingId f == maybe "" id (patchVulnerabilityId patch)
                    then f { findingStatus = FindingCommitted }
                    else f
                    ) (stateFindings state)

              pure state
                { statePhase = if null (stateVulnerabilities state) then PhaseReporting else PhaseRefactoring
                , stateCurrentPatch = Nothing
                , stateCurrentVulnerability = Nothing
                , stateFindings = updatedFindings
                , stateMessages =
                    [assistantMessage $ "Committed: " <> commitMsg <> "\n" <> output]
                , stateEventLog = stateEventLog state ++
                    [logEvent PhaseCommitting "CommitNode" ("Committed: " <> commitMsg) EventInfo]
                , stateIterationCount = stateIterationCount state + 1
                }

-- ---------------------------------------------------------------------------
-- Report Generation
-- ---------------------------------------------------------------------------

-- | Generate the final analysis report
generateReport
  :: Maybe TelemetrySystem
  -> AegisState
  -> IO AegisState
generateReport mbTelemetry state = do
  mapM_ (\ts -> emitNodeStart ts "ReportGenerator") mbTelemetry
  now <- getCurrentTime

  let totalFindings = length (stateFindings state)
      committed = length [f | f <- stateFindings state, findingStatus f == FindingCommitted]
      failed = length [f | f <- stateFindings state, isFailed (findingStatus f)]
      pending = totalFindings - committed - failed

      report = AnalysisReport
        { reportId = stateScanId state
        , reportTitle = "AegisCode AI Security Analysis Report"
        , reportSummary = T.unlines
            [ "Scan completed for repository: " <> T.pack (stateRepoPath state)
            , "Total findings: " <> T.pack (show totalFindings)
            , "Remediated: " <> T.pack (show committed)
            , "Failed: " <> T.pack (show failed)
            , "Pending: " <> T.pack (show pending)
            ]
        , reportSections =
            [ ReportSection "Overview" (formatOverview state) 1
            , ReportSection "Findings Detail" (formatFindingsDetail state) 2
            , ReportSection "Patches Applied" (formatPatchesApplied state) 3
            , ReportSection "Errors" (formatErrors state) 4
            ]
        , reportTotalFindings = totalFindings
        , reportRemediatedCount = committed
        , reportPendingCount = pending
        , reportFailedCount = failed
        , reportTotalTokensUsed = stateTotalTokensUsed state
        , reportDurationSeconds = 0.0
        , reportGeneratedAt = Just now
        }

  mapM_ (\ts -> emitInfo ts "ReportGenerator"
    ("Report generated: " <> T.pack (show totalFindings) <> " findings, "
     <> T.pack (show committed) <> " remediated")) mbTelemetry

  mapM_ (\ts -> emitNodeEnd ts "ReportGenerator" 0.0) mbTelemetry

  pure state
    { statePhase = PhaseCompleted
    , stateReport = Just report
    , stateMessages =
        [assistantMessage $ "=== Report ===\n" <> reportSummary report]
    , stateEventLog = stateEventLog state ++
        [logEvent PhaseReporting "ReportGenerator" "Report generated" EventInfo]
    }

-- ---------------------------------------------------------------------------
-- Report Formatting Helpers
-- ---------------------------------------------------------------------------

isFailed :: FindingStatus -> Bool
isFailed (FindingFailed _) = True
isFailed FindingRejected = True
isFailed _ = False

formatOverview :: AegisState -> Text
formatOverview state = T.unlines
  [ "Repository: " <> T.pack (stateRepoPath state)
  , "Thread: " <> stateThreadId state
  , "Scan: " <> stateScanId state
  , "Iterations: " <> T.pack (show (stateIterationCount state))
  , "Tokens used: " <> T.pack (show (stateTotalTokensUsed state))
  ]

formatFindingsDetail :: AegisState -> Text
formatFindingsDetail state = T.unlines $
  concatMap (\f ->
    [ "- [" <> T.pack (show (findingStatus f)) <> "] " <> vtTitle (findingTarget f)
    , "  Severity: " <> severityToText (vtSeverity (findingTarget f))
    , "  File: " <> T.pack (locFilePath (vtLocation (findingTarget f)))
    , ""
    ]) (stateFindings state)

formatPatchesApplied :: AegisState -> Text
formatPatchesApplied state = T.unlines $
  concatMap (\p ->
    [ "- " <> T.pack (patchFilePath p) <> ": " <> patchDescription p
    , "  +" <> T.pack (show (patchLinesAdded p)) <> " / -" <> T.pack (show (patchLinesRemoved p))
    , ""
    ]) (statePatches state)

formatErrors :: AegisState -> Text
formatErrors state =
  if null (stateErrors state)
  then "No errors."
  else T.unlines (map ("- " <>) (stateErrors state))

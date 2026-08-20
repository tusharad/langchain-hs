{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Agents.HITLAgent
Description : Human-in-the-Loop gatekeeper agent
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Human-in-the-loop gatekeeper that suspends pipeline execution for human review.
Uses the HITL interrupt mechanism from langchain-hs-graph to pause the StateGraph,
saves complete state via Checkpointer for external review, and exposes a structured
diff summary for the reviewer. Resume logic via 'resumeGraph' with human modifications.
-}
module Aegis.Agents.HITLAgent
  ( -- * Agent
    hitlReviewNode
  , processApproval
  , processRejection

    -- * Review Formatting
  , formatReviewSummary
  , formatPatchForReview

    -- * Types
  , HITLReviewRequest (..)
  , HITLReviewResponse (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime)
import GHC.Generics (Generic)

import Langchain.Core.Model.Types (Message, assistantMessage)

import Aegis.Core.Types.Pipeline
import Aegis.Core.Types.Security
import Aegis.Middleware.Telemetry (TelemetrySystem, emitInfo, emitWarning)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A structured review request sent to the human reviewer
data HITLReviewRequest = HITLReviewRequest
  { reviewThreadId :: Text
  -- ^ Thread ID for this pipeline run
  , reviewScanId :: Text
  -- ^ Scan ID
  , reviewVulnerability :: Maybe VulnerabilityTarget
  -- ^ The vulnerability being fixed
  , reviewPatch :: Maybe PatchDiff
  -- ^ The proposed patch
  , reviewTestResult :: Maybe TestResult
  -- ^ Test results for the patch
  , reviewSummary :: Text
  -- ^ Human-readable review summary
  , reviewCreatedAt :: Maybe UTCTime
  -- ^ When the review was requested
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Human reviewer's response
data HITLReviewResponse = HITLReviewResponse
  { responseApproved :: Bool
  -- ^ Whether the patch is approved
  , responseNotes :: Text
  -- ^ Reviewer's notes/comments
  , responseModifications :: Maybe Text
  -- ^ Optional modified patch (if reviewer made changes)
  , responseReviewedAt :: Maybe UTCTime
  -- ^ When the review was completed
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- HITL Review Node
-- ---------------------------------------------------------------------------

-- | StateGraph node that pauses execution for human review.
-- In a real deployment, this would use langchain-hs-graph's HITL interrupt
-- mechanism. Here we prepare the review state and set the approval status
-- to Pending.
hitlReviewNode
  :: Maybe TelemetrySystem
  -> AegisState
  -> IO AegisState
hitlReviewNode mbTelemetry state = do
  mapM_ (\ts -> emitInfo ts "HITLAgent" "Entering human-in-the-loop review...") mbTelemetry
  now <- getCurrentTime

  let reviewRequest = HITLReviewRequest
        { reviewThreadId = stateThreadId state
        , reviewScanId = stateScanId state
        , reviewVulnerability = stateCurrentVulnerability state
        , reviewPatch = stateCurrentPatch state
        , reviewTestResult = case stateTestResults state of
            [] -> Nothing
            results -> Just (last results)
        , reviewSummary = formatReviewSummary state
        , reviewCreatedAt = Just now
        }

  let reviewMsg = assistantMessage $ T.unlines
        [ "=== HUMAN REVIEW REQUIRED ==="
        , ""
        , reviewSummary reviewRequest
        , ""
        , "The pipeline is paused awaiting your review."
        , "Use the API to approve or reject:"
        , "  POST /api/approve/" <> stateThreadId state
        , "  POST /api/reject/" <> stateThreadId state
        ]

  mapM_ (\ts -> emitWarning ts "HITLAgent" "Pipeline paused for human review") mbTelemetry

  pure state
    { statePhase = PhaseAwaitingApproval
    , stateApprovalStatus = Pending
    , stateMessages = [reviewMsg]
    , stateEventLog = stateEventLog state ++
        [logEvent PhaseAwaitingApproval "HITLAgent" "Awaiting human approval" EventInfo]
    }

-- ---------------------------------------------------------------------------
-- Approval Processing
-- ---------------------------------------------------------------------------

-- | Process a human approval response
processApproval :: Maybe TelemetrySystem -> Text -> AegisState -> IO AegisState
processApproval mbTelemetry notes state = do
  mapM_ (\ts -> emitInfo ts "HITLAgent" ("Patch APPROVED: " <> notes)) mbTelemetry
  now <- getCurrentTime

  pure state
    { statePhase = PhaseCommitting
    , stateApprovalStatus = Approved notes
    , stateReviewerNotes = Just notes
    , stateMessages = [assistantMessage $ "Patch approved by reviewer: " <> notes]
    , stateEventLog = stateEventLog state ++
        [logEvent PhaseAwaitingApproval "HITLAgent"
          ("Patch approved: " <> notes) EventInfo]
    }

-- | Process a human rejection response
processRejection :: Maybe TelemetrySystem -> Text -> AegisState -> IO AegisState
processRejection mbTelemetry reason state = do
  mapM_ (\ts -> emitWarning ts "HITLAgent" ("Patch REJECTED: " <> reason)) mbTelemetry

  let retries = stateRetryCount state + 1
      maxRetries = stateMaxRetries state

  if retries >= maxRetries
    then pure state
      { statePhase = PhaseFailed ("Patch rejected by reviewer after " <> T.pack (show retries) <> " attempts: " <> reason)
      , stateApprovalStatus = Rejected reason
      , stateReviewerNotes = Just reason
      , stateErrors = stateErrors state ++ ["Rejected: " <> reason]
      , stateEventLog = stateEventLog state ++
          [logEvent PhaseAwaitingApproval "HITLAgent"
            ("Patch rejected, max retries exceeded: " <> reason) EventError]
      }
    else pure state
      { statePhase = PhaseRefactoring  -- Route back to refactor agent
      , stateApprovalStatus = Rejected reason
      , stateReviewerNotes = Just reason
      , stateRetryCount = retries
      , stateCurrentPatch = Nothing
      , stateMessages =
          [assistantMessage $ T.unlines
            [ "Patch rejected by reviewer. Reason: " <> reason
            , "Retry " <> T.pack (show retries) <> "/" <> T.pack (show maxRetries)
            , "Please generate a new patch addressing the reviewer's feedback."
            ]]
      , stateEventLog = stateEventLog state ++
          [logEvent PhaseAwaitingApproval "HITLAgent"
            ("Patch rejected, retrying: " <> reason) EventWarning]
      }

-- ---------------------------------------------------------------------------
-- Review Formatting
-- ---------------------------------------------------------------------------

-- | Format a comprehensive review summary for the human reviewer
formatReviewSummary :: AegisState -> Text
formatReviewSummary state = T.unlines $
  [ "=== AegisCode AI — Review Summary ==="
  , "Thread: " <> stateThreadId state
  , "Scan: " <> stateScanId state
  , "Repository: " <> T.pack (stateRepoPath state)
  , ""
  ]
  ++ vulnerabilitySection
  ++ patchSection
  ++ testSection
  ++ statsSection
  where
    vulnerabilitySection = case stateCurrentVulnerability state of
      Nothing -> ["No active vulnerability."]
      Just vuln ->
        [ "--- Vulnerability ---"
        , "Title: " <> vtTitle vuln
        , "Severity: " <> severityToText (vtSeverity vuln)
        , "Category: " <> T.pack (show (vtCategory vuln))
        , "File: " <> T.pack (locFilePath (vtLocation vuln))
        , "Description: " <> vtDescription vuln
        , ""
        ]

    patchSection = case stateCurrentPatch state of
      Nothing -> ["No patch generated."]
      Just patch ->
        [ "--- Proposed Patch ---"
        , "File: " <> T.pack (patchFilePath patch)
        , "Lines added: " <> T.pack (show (patchLinesAdded patch))
        , "Lines removed: " <> T.pack (show (patchLinesRemoved patch))
        , "Description: " <> patchDescription patch
        , ""
        , "```diff"
        , patchRawDiff patch
        , "```"
        , ""
        ]

    testSection = case stateTestResults state of
      [] -> ["No test results available.", ""]
      results ->
        let latest = last results
        in [ "--- Test Results ---"
           , "Outcome: " <> T.pack (show (testOutcome latest))
           , "Exit Code: " <> T.pack (show (testExitCode latest))
           , "Pass: " <> T.pack (show (testPassCount latest))
           , "Fail: " <> T.pack (show (testFailCount latest))
           , ""
           ]

    statsSection =
      [ "--- Pipeline Stats ---"
      , "Total findings: " <> T.pack (show (length (stateFindings state)))
      , "Patches generated: " <> T.pack (show (length (statePatches state)))
      , "Retry count: " <> T.pack (show (stateRetryCount state)) <> "/" <> T.pack (show (stateMaxRetries state))
      , "Total tokens: " <> T.pack (show (stateTotalTokensUsed state))
      , "Iteration: " <> T.pack (show (stateIterationCount state))
      ]

-- | Format a patch for human review with line numbers
formatPatchForReview :: PatchDiff -> Text
formatPatchForReview patch = T.unlines
  [ "=== Patch: " <> T.pack (patchFilePath patch) <> " ==="
  , "Description: " <> patchDescription patch
  , "Lines: +" <> T.pack (show (patchLinesAdded patch))
    <> " / -" <> T.pack (show (patchLinesRemoved patch))
  , ""
  , patchRawDiff patch
  ]

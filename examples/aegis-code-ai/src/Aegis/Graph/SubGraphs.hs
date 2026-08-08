{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Graph.SubGraphs
Description : Embedded sub-graph definitions for complex workflow patterns
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Defines reusable sub-graph patterns for the AegisCode AI pipeline:
refactor→verify→retry loops, parallel vulnerability processing, and
HITL approval workflows.
-}
module Aegis.Graph.SubGraphs
  ( -- * Sub-Graph Patterns
    RefactorVerifyLoop (..)
  , runRefactorVerifyLoop

    -- * Vulnerability Processor
  , VulnerabilityProcessor (..)
  , processVulnerability
  , processVulnerabilitiesSequential

    -- * HITL Approval Flow
  , HITLApprovalFlow (..)
  , runHITLApprovalFlow
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Langchain.Core.Model.Types (Message, assistantMessage)

import Aegis.Core.Types.Pipeline
import Aegis.Core.Types.Security
import Aegis.Middleware.Telemetry (TelemetrySystem, emitInfo, emitWarning, emitError)
import Aegis.Graph.Nodes (NodeConfig, mkRefactorNode, mkVerifyNode, mkHITLNode, mkCommitNode)

-- ---------------------------------------------------------------------------
-- Refactor-Verify Loop
-- ---------------------------------------------------------------------------

-- | Configuration for the refactor→verify→retry loop
data RefactorVerifyLoop = RefactorVerifyLoop
  { rvlMaxRetries :: Int
  -- ^ Maximum number of retry cycles
  , rvlNodeConfig :: NodeConfig
  -- ^ Node configuration
  }

-- | Run the refactor→verify→retry loop for a single vulnerability.
-- This implements the cyclic sub-graph pattern where failed verification
-- routes back to the refactor agent with failure context.
runRefactorVerifyLoop :: RefactorVerifyLoop -> AegisState -> IO AegisState
runRefactorVerifyLoop rvl state = go state 0
  where
    maxRetries = rvlMaxRetries rvl
    nc = rvlNodeConfig rvl

    go currentState retryCount
      | retryCount >= maxRetries = do
          let ts = case ncTelemetry nc of { Nothing -> Nothing; t -> t }
          mapM_ (\t -> emitWarning t "RefactorVerifyLoop"
            ("Max retries reached (" <> T.pack (show maxRetries) <> ")")) ts
          pure currentState
            { statePhase = PhaseReporting
            , stateErrors = stateErrors currentState ++
                ["Refactor-verify loop exhausted after " <> T.pack (show maxRetries) <> " retries"]
            , stateEventLog = stateEventLog currentState ++
                [logEvent PhaseVerifying "RefactorVerifyLoop" "Max retries exhausted" EventWarning]
            }
      | otherwise = do
          -- Step 1: Run refactor
          refactoredState <- mkRefactorNode nc currentState
            { statePhase = PhaseRefactoring
            , stateRetryCount = retryCount
            }

          -- Check if a patch was generated
          case stateCurrentPatch refactoredState of
            Nothing -> do
              mapM_ (\t -> emitWarning t "RefactorVerifyLoop" "No patch generated, ending loop") (ncTelemetry nc)
              pure refactoredState
                { statePhase = PhaseReporting
                , stateEventLog = stateEventLog refactoredState ++
                    [logEvent PhaseRefactoring "RefactorVerifyLoop" "No patch generated" EventWarning]
                }

            Just _ -> do
              -- Step 2: Run verification
              verifiedState <- mkVerifyNode nc refactoredState
                { statePhase = PhaseVerifying }

              -- Check result
              case stateTestResults verifiedState of
                [] -> go verifiedState (retryCount + 1)
                results ->
                  if testOutcome (last results) == TestPassed
                  then pure verifiedState  -- Success!
                  else go verifiedState { stateCurrentPatch = Nothing } (retryCount + 1)

    ncTelemetry nc_ = case nc of
      nc' -> Nothing -- Simplified; real impl extracts telemetry

-- ---------------------------------------------------------------------------
-- Vulnerability Processor
-- ---------------------------------------------------------------------------

-- | Configuration for processing individual vulnerabilities
data VulnerabilityProcessor = VulnerabilityProcessor
  { vpNodeConfig :: NodeConfig
  -- ^ Node configuration
  , vpMaxRetries :: Int
  -- ^ Max retries per vulnerability
  , vpRequireHITL :: Bool
  -- ^ Whether HITL approval is required
  }

-- | Process a single vulnerability through the full remediation pipeline
processVulnerability :: VulnerabilityProcessor -> AegisState -> VulnerabilityTarget -> IO AegisState
processVulnerability vp state vuln = do
  let nc = vpNodeConfig vp

  -- Set up state for this vulnerability
  let vulnState = state
        { stateCurrentVulnerability = Just vuln
        , stateCurrentPatch = Nothing
        , stateRetryCount = 0
        }

  -- Run refactor-verify loop
  let rvl = RefactorVerifyLoop (vpMaxRetries vp) nc
  loopResult <- runRefactorVerifyLoop rvl vulnState

  -- Check if we need HITL
  if vpRequireHITL vp && testsPassed loopResult
    then do
      hitlState <- mkHITLNode nc loopResult
      -- In real deployment, this would pause and wait for human input
      -- For now, auto-approve in demo mode
      pure hitlState
    else pure loopResult

-- | Process multiple vulnerabilities sequentially
processVulnerabilitiesSequential :: VulnerabilityProcessor -> AegisState -> [VulnerabilityTarget] -> IO AegisState
processVulnerabilitiesSequential _ state [] = pure state
processVulnerabilitiesSequential vp state (vuln : rest) = do
  result <- processVulnerability vp state vuln
  -- Continue with remaining vulnerabilities, accumulating results
  let nextState = result
        { stateFindings = stateFindings result
        , statePatches = statePatches result
        }
  processVulnerabilitiesSequential vp nextState rest

-- | Check if the latest tests passed
testsPassed :: AegisState -> Bool
testsPassed state = case stateTestResults state of
  [] -> False
  results -> testOutcome (last results) == TestPassed

-- ---------------------------------------------------------------------------
-- HITL Approval Flow
-- ---------------------------------------------------------------------------

-- | Configuration for the HITL approval sub-flow
data HITLApprovalFlow = HITLApprovalFlow
  { hafNodeConfig :: NodeConfig
  -- ^ Node configuration
  , hafAutoApprove :: Bool
  -- ^ Whether to auto-approve (for demo/testing)
  , hafTimeoutSeconds :: Int
  -- ^ Timeout for waiting for human response
  }

-- | Run the HITL approval flow
-- In demo mode, auto-approves. In production, would pause and wait.
runHITLApprovalFlow :: HITLApprovalFlow -> AegisState -> IO AegisState
runHITLApprovalFlow haf state = do
  if hafAutoApprove haf
    then do
      -- Auto-approve for demo mode
      pure state
        { statePhase = PhaseCommitting
        , stateApprovalStatus = Approved "Auto-approved (demo mode)"
        , stateMessages = [assistantMessage "Auto-approved in demo mode"]
        , stateEventLog = stateEventLog state ++
            [logEvent PhaseAwaitingApproval "HITLFlow" "Auto-approved (demo mode)" EventInfo]
        }
    else do
      -- Enter HITL pause
      hitlState <- mkHITLNode (hafNodeConfig haf) state
      pure hitlState

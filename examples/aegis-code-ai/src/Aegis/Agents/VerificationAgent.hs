{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Agents.VerificationAgent
Description : Verification and sandbox testing agent
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Verification agent that applies generated patches, runs test suites in a sandbox,
evaluates pass/fail outcomes, and updates AegisState with TestResult. On failure,
increments retry counter and appends failure logs for re-routing to the refactor agent.
-}
module Aegis.Agents.VerificationAgent
  ( -- * Agent
    runVerificationAgent
  , verificationNode

    -- * Configuration
  , VerificationConfig (..)
  , defaultVerificationConfig

    -- * Test Execution
  , applyAndTest
  , rollbackPatch
  , interpretTestResults
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Langchain.Core.Model.Types (Message, assistantMessage)

import Aegis.Core.Types.Pipeline
import Aegis.Core.Types.Security (VulnerabilityTarget (..), severityToText)
import Aegis.Middleware.Telemetry (TelemetrySystem, emitAgentStart, emitAgentEnd, emitInfo, emitWarning, emitError)
import Aegis.Tools.Docker (runProcessWithTimeout)
import Aegis.Tools.Git (runGitCommand)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the verification agent
data VerificationConfig = VerificationConfig
  { vcTestCommand :: Text
  -- ^ Command to run tests (e.g., "cabal test", "stack test")
  , vcBuildCommand :: Text
  -- ^ Command to build/compile (e.g., "cabal build")
  , vcTimeoutSeconds :: Int
  -- ^ Timeout for test execution
  , vcRequireBuild :: Bool
  -- ^ Whether to require successful build before testing
  , vcRollbackOnFailure :: Bool
  -- ^ Whether to rollback the patch on test failure
  }
  deriving (Eq, Show)

-- | Default verification configuration for Haskell projects
defaultVerificationConfig :: VerificationConfig
defaultVerificationConfig = VerificationConfig
  { vcTestCommand = "cabal test 2>&1"
  , vcBuildCommand = "cabal build 2>&1"
  , vcTimeoutSeconds = 300
  , vcRequireBuild = True
  , vcRollbackOnFailure = True
  }

-- ---------------------------------------------------------------------------
-- Agent Execution
-- ---------------------------------------------------------------------------

-- | Run the verification agent on a patch
runVerificationAgent
  :: VerificationConfig
  -> Maybe TelemetrySystem
  -> FilePath           -- ^ Repository path
  -> PatchDiff          -- ^ Patch to verify
  -> IO (TestResult, [Message])
runVerificationAgent config mbTelemetry repoPath patch = do
  mapM_ (\ts -> emitAgentStart ts "VerificationAgent") mbTelemetry

  -- Step 1: Apply the patch
  mapM_ (\ts -> emitInfo ts "VerificationAgent" ("Applying patch to " <> T.pack (patchFilePath patch))) mbTelemetry

  applyResult <- applyPatch repoPath patch
  case applyResult of
    Left err -> do
      mapM_ (\ts -> emitError ts "VerificationAgent" ("Patch application failed: " <> err)) mbTelemetry
      let result = emptyTestResult
            { testOutcome = TestError
            , testStderr = "Patch application failed: " <> err
            , testExitCode = 1
            , testCommand = "git apply"
            , testPatchId = patchVulnerabilityId patch
            }
      pure (result, [assistantMessage $ "Patch application FAILED: " <> err])

    Right () -> do
      -- Step 2: Build check (if required)
      buildOk <- if vcRequireBuild config
        then do
          mapM_ (\ts -> emitInfo ts "VerificationAgent" "Running build check...") mbTelemetry
          buildResult <- runProcessWithTimeout repoPath (T.unpack (vcBuildCommand config)) (vcTimeoutSeconds config)
          case buildResult of
            Left err -> do
              mapM_ (\ts -> emitError ts "VerificationAgent" ("Build failed: " <> err)) mbTelemetry
              when (vcRollbackOnFailure config) $ rollbackPatch repoPath
              pure $ Left err
            Right (stdout, stderr, code) ->
              if code == 0
              then pure $ Right ()
              else do
                mapM_ (\ts -> emitError ts "VerificationAgent" ("Build failed with exit " <> T.pack (show code))) mbTelemetry
                when (vcRollbackOnFailure config) $ rollbackPatch repoPath
                pure $ Left $ "Build failed (exit " <> T.pack (show code) <> "): " <> stderr
        else pure $ Right ()

      case buildOk of
        Left buildErr -> do
          let result = emptyTestResult
                { testOutcome = TestError
                , testStderr = buildErr
                , testExitCode = 1
                , testCommand = vcBuildCommand config
                , testPatchId = patchVulnerabilityId patch
                }
          pure (result, [assistantMessage $ "Build FAILED: " <> buildErr])

        Right () -> do
          -- Step 3: Run tests
          mapM_ (\ts -> emitInfo ts "VerificationAgent" "Running test suite...") mbTelemetry
          testResult <- runProcessWithTimeout repoPath (T.unpack (vcTestCommand config)) (vcTimeoutSeconds config)
          case testResult of
            Left err -> do
              mapM_ (\ts -> emitError ts "VerificationAgent" ("Test execution failed: " <> err)) mbTelemetry
              when (vcRollbackOnFailure config) $ rollbackPatch repoPath
              let result = emptyTestResult
                    { testOutcome = TestTimeout
                    , testStderr = err
                    , testExitCode = 124
                    , testCommand = vcTestCommand config
                    , testPatchId = patchVulnerabilityId patch
                    }
              pure (result, [assistantMessage $ "Tests TIMED OUT: " <> err])

            Right (stdout, stderr, code) -> do
              let outcome = interpretTestResults code stdout stderr
                  result = TestResult
                    { testOutcome = outcome
                    , testStdout = stdout
                    , testStderr = stderr
                    , testExitCode = code
                    , testDurationSeconds = 0.0  -- Placeholder
                    , testPassCount = countTestResults "pass" stdout
                    , testFailCount = countTestResults "fail" stdout
                    , testSkipCount = countTestResults "skip" stdout
                    , testCommand = vcTestCommand config
                    , testPatchId = patchVulnerabilityId patch
                    }
              case outcome of
                TestPassed -> do
                  mapM_ (\ts -> emitInfo ts "VerificationAgent" "All tests PASSED!") mbTelemetry
                  pure (result, [assistantMessage "All tests PASSED. Patch is verified."])
                TestFailed -> do
                  mapM_ (\ts -> emitWarning ts "VerificationAgent" "Tests FAILED.") mbTelemetry
                  when (vcRollbackOnFailure config) $ rollbackPatch repoPath
                  pure (result, [assistantMessage $ "Tests FAILED:\n" <> stderr])
                _ -> do
                  when (vcRollbackOnFailure config) $ rollbackPatch repoPath
                  pure (result, [assistantMessage $ "Test execution error:\n" <> stderr])

-- | StateGraph node wrapper for the verification agent
verificationNode
  :: VerificationConfig
  -> Maybe TelemetrySystem
  -> AegisState
  -> IO AegisState
verificationNode config mbTelemetry state = do
  mapM_ (\ts -> emitInfo ts "VerificationAgent" "Starting patch verification...") mbTelemetry

  case stateCurrentPatch state of
    Nothing -> pure state
      { statePhase = PhaseReporting
      , stateEventLog = stateEventLog state ++
          [logEvent PhaseVerifying "VerificationAgent" "No patch to verify" EventWarning]
      }

    Just patch -> do
      (testResult, newMsgs) <- runVerificationAgent config mbTelemetry (stateRepoPath state) patch

      case testOutcome testResult of
        TestPassed -> pure state
          { statePhase = if scanRequiresHITL state then PhaseAwaitingApproval else PhaseCommitting
          , stateTestResults = stateTestResults state ++ [testResult]
          , stateMessages = newMsgs
          , stateRetryCount = 0
          , stateEventLog = stateEventLog state ++
              [logEvent PhaseVerifying "VerificationAgent" "Patch verified successfully" EventInfo]
          , stateIterationCount = stateIterationCount state + 1
          }

        _ ->
          let retries = stateRetryCount state + 1
              maxRetries = stateMaxRetries state
          in if retries >= maxRetries
             then pure state
               { statePhase = PhaseFailed "Max retries exceeded for patch verification"
               , stateTestResults = stateTestResults state ++ [testResult]
               , stateMessages = newMsgs
               , stateRetryCount = retries
               , stateErrors = stateErrors state ++ ["Verification failed after " <> T.pack (show maxRetries) <> " retries"]
               , stateEventLog = stateEventLog state ++
                   [logEvent PhaseVerifying "VerificationAgent" "Max retries exceeded" EventError]
               , stateIterationCount = stateIterationCount state + 1
               }
             else pure state
               { statePhase = PhaseRefactoring  -- Route back to refactor agent
               , stateTestResults = stateTestResults state ++ [testResult]
               , stateMessages = newMsgs ++
                   [assistantMessage $ "Retry " <> T.pack (show retries) <> "/" <> T.pack (show maxRetries)
                     <> ". Previous failure:\n" <> testStderr testResult]
               , stateRetryCount = retries
               , stateCurrentPatch = Nothing  -- Clear current patch for regeneration
               , stateEventLog = stateEventLog state ++
                   [logEvent PhaseVerifying "VerificationAgent"
                     ("Verification failed, retry " <> T.pack (show retries) <> "/" <> T.pack (show maxRetries)) EventWarning]
               , stateIterationCount = stateIterationCount state + 1
               }

-- ---------------------------------------------------------------------------
-- Patch Application
-- ---------------------------------------------------------------------------

-- | Apply a patch to the repository
applyPatch :: FilePath -> PatchDiff -> IO (Either Text ())
applyPatch repoPath patch = do
  let patchContent = patchRawDiff patch
  if T.null patchContent
    then pure $ Left "Empty patch content"
    else do
      -- Write patch to temp file
      let patchFile = repoPath ++ "/.aegis-verify-patch.tmp"
      T.writeFile patchFile patchContent
      result <- runGitCommand repoPath ["apply", "--check", patchFile]
      case result of
        Left err -> pure $ Left $ "Patch check failed: " <> err
        Right _ -> do
          applyResult <- runGitCommand repoPath ["apply", patchFile]
          case applyResult of
            Left err -> pure $ Left $ "Patch apply failed: " <> err
            Right _ -> pure $ Right ()

-- | Rollback the working tree to HEAD
rollbackPatch :: FilePath -> IO ()
rollbackPatch repoPath = do
  _ <- runGitCommand repoPath ["checkout", "--", "."]
  _ <- runGitCommand repoPath ["clean", "-fd"]
  pure ()

-- ---------------------------------------------------------------------------
-- Test Result Interpretation
-- ---------------------------------------------------------------------------

-- | Interpret test results from exit code and output
interpretTestResults :: Int -> Text -> Text -> TestOutcome
interpretTestResults exitCode stdout stderr
  | exitCode == 0 = TestPassed
  | exitCode == 124 = TestTimeout
  | "error" `T.isInfixOf` T.toLower stderr && "build" `T.isInfixOf` T.toLower stderr = TestError
  | otherwise = TestFailed

-- | Count test result occurrences in output
countTestResults :: Text -> Text -> Int
countTestResults keyword output =
  length $ filter (T.isInfixOf (T.toLower keyword)) (T.lines (T.toLower output))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Check if HITL is required based on state
scanRequiresHITL :: AegisState -> Bool
scanRequiresHITL _ = True  -- Default: always require HITL

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()

T.writeFile :: FilePath -> Text -> IO ()
T.writeFile = T.writeFile -- This is Data.Text.IO.writeFile

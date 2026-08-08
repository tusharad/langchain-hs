{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Core.Types.Pipeline
Description : Unified pipeline state types for AegisCode AI graph orchestration
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Defines the central 'AegisState' record used as the state type in the StateGraph,
along with patch diff representations, test results, analysis reports, and
pipeline phase tracking. This is the single source of truth for all data
flowing through the multi-agent orchestration pipeline.
-}
module Aegis.Core.Types.Pipeline
  ( -- * Pipeline Phase
    PipelinePhase (..)
  , phaseToText

    -- * Approval Status
  , ApprovalStatus (..)

    -- * Patch Diff
  , PatchDiff (..)
  , DiffHunk (..)
  , emptyPatchDiff

    -- * Test Result
  , TestResult (..)
  , TestOutcome (..)
  , emptyTestResult

    -- * Analysis Report
  , AnalysisReport (..)
  , ReportSection (..)
  , emptyAnalysisReport

    -- * Central Pipeline State
  , AegisState (..)
  , initialAegisState
  , aegisStateReducer

    -- * Pipeline Event Log
  , PipelineEvent (..)
  , EventSeverity (..)
  , logEvent
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Langchain.Core.Model.Types (Message)
import Langchain.Graph.StateGraph (StateReducer)

import Aegis.Core.Types.AST (CodeChunk, SymbolGraph, emptySymbolGraph)
import Aegis.Core.Types.Security
  ( SecurityFinding
  , VulnerabilityTarget
  )

-- ---------------------------------------------------------------------------
-- Pipeline Phase
-- ---------------------------------------------------------------------------

-- | Current phase of the AegisCode AI pipeline
data PipelinePhase
  = PhaseInitializing
  -- ^ System initializing, loading configuration
  | PhaseIndexing
  -- ^ Codebase indexing and RAG construction
  | PhaseTriaging
  -- ^ Vulnerability triage and prioritization
  | PhaseRefactoring
  -- ^ Patch generation and code refactoring
  | PhaseVerifying
  -- ^ Sandbox verification and testing
  | PhaseAwaitingApproval
  -- ^ Paused for human-in-the-loop review
  | PhaseCommitting
  -- ^ Committing approved patches
  | PhaseReporting
  -- ^ Generating final analysis report
  | PhaseCompleted
  -- ^ Pipeline completed successfully
  | PhaseFailed Text
  -- ^ Pipeline failed with reason
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Convert pipeline phase to display text
phaseToText :: PipelinePhase -> Text
phaseToText PhaseInitializing    = "Initializing"
phaseToText PhaseIndexing        = "Indexing Codebase"
phaseToText PhaseTriaging        = "Triaging Vulnerabilities"
phaseToText PhaseRefactoring     = "Generating Patches"
phaseToText PhaseVerifying       = "Verifying Patches"
phaseToText PhaseAwaitingApproval = "Awaiting Human Approval"
phaseToText PhaseCommitting      = "Committing Changes"
phaseToText PhaseReporting       = "Generating Report"
phaseToText PhaseCompleted       = "Completed"
phaseToText (PhaseFailed reason) = "Failed: " <> reason

-- ---------------------------------------------------------------------------
-- Approval Status
-- ---------------------------------------------------------------------------

-- | Human approval status for HITL workflow
data ApprovalStatus
  = NotSubmitted
  -- ^ Not yet submitted for review
  | Pending
  -- ^ Submitted, awaiting human decision
  | Approved Text
  -- ^ Approved with optional notes
  | Rejected Text
  -- ^ Rejected with reason
  | ModificationRequested Text
  -- ^ Human requested modifications to the patch
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Patch Diff
-- ---------------------------------------------------------------------------

-- | A single hunk within a unified diff
data DiffHunk = DiffHunk
  { hunkOldStart :: Int
  -- ^ Start line in original file
  , hunkOldCount :: Int
  -- ^ Number of lines from original
  , hunkNewStart :: Int
  -- ^ Start line in modified file
  , hunkNewCount :: Int
  -- ^ Number of lines in modified
  , hunkContent :: Text
  -- ^ The actual diff content (with +/- prefixes)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A unified diff patch for a single file
data PatchDiff = PatchDiff
  { patchFilePath :: FilePath
  -- ^ File being patched
  , patchOldPath :: Maybe FilePath
  -- ^ Old file path (for renames)
  , patchHunks :: [DiffHunk]
  -- ^ Diff hunks
  , patchRawDiff :: Text
  -- ^ Raw unified diff text
  , patchDescription :: Text
  -- ^ Human-readable description of the change
  , patchVulnerabilityId :: Maybe Text
  -- ^ Linked vulnerability target ID
  , patchLinesAdded :: Int
  -- ^ Total lines added
  , patchLinesRemoved :: Int
  -- ^ Total lines removed
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create an empty patch diff
emptyPatchDiff :: FilePath -> PatchDiff
emptyPatchDiff fp =
  PatchDiff
    { patchFilePath = fp
    , patchOldPath = Nothing
    , patchHunks = []
    , patchRawDiff = ""
    , patchDescription = ""
    , patchVulnerabilityId = Nothing
    , patchLinesAdded = 0
    , patchLinesRemoved = 0
    }

-- ---------------------------------------------------------------------------
-- Test Result
-- ---------------------------------------------------------------------------

-- | Outcome of a test suite execution
data TestOutcome
  = TestPassed
  -- ^ All tests passed
  | TestFailed
  -- ^ One or more tests failed
  | TestError
  -- ^ Test execution errored (e.g., compilation failure)
  | TestTimeout
  -- ^ Test execution timed out
  | TestSkipped
  -- ^ Tests were skipped (e.g., sandbox not available)
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Result of running tests in the verification sandbox
data TestResult = TestResult
  { testOutcome :: TestOutcome
  -- ^ Overall test outcome
  , testStdout :: Text
  -- ^ Standard output from test execution
  , testStderr :: Text
  -- ^ Standard error from test execution
  , testExitCode :: Int
  -- ^ Process exit code
  , testDurationSeconds :: Double
  -- ^ Execution duration in seconds
  , testPassCount :: Int
  -- ^ Number of tests that passed
  , testFailCount :: Int
  -- ^ Number of tests that failed
  , testSkipCount :: Int
  -- ^ Number of tests that were skipped
  , testCommand :: Text
  -- ^ Command that was executed
  , testPatchId :: Maybe Text
  -- ^ ID of the patch being tested
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create an empty test result
emptyTestResult :: TestResult
emptyTestResult =
  TestResult
    { testOutcome = TestSkipped
    , testStdout = ""
    , testStderr = ""
    , testExitCode = 0
    , testDurationSeconds = 0.0
    , testPassCount = 0
    , testFailCount = 0
    , testSkipCount = 0
    , testCommand = ""
    , testPatchId = Nothing
    }

-- ---------------------------------------------------------------------------
-- Analysis Report
-- ---------------------------------------------------------------------------

-- | A section within the analysis report
data ReportSection = ReportSection
  { sectionTitle :: Text
  -- ^ Section title
  , sectionContent :: Text
  -- ^ Section content (markdown)
  , sectionOrder :: Int
  -- ^ Display order
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Final analysis report generated at the end of a pipeline run
data AnalysisReport = AnalysisReport
  { reportId :: Text
  -- ^ Unique report identifier
  , reportTitle :: Text
  -- ^ Report title
  , reportSummary :: Text
  -- ^ Executive summary
  , reportSections :: [ReportSection]
  -- ^ Detailed report sections
  , reportTotalFindings :: Int
  -- ^ Total number of findings
  , reportRemediatedCount :: Int
  -- ^ Number of findings successfully remediated
  , reportPendingCount :: Int
  -- ^ Number of findings pending
  , reportFailedCount :: Int
  -- ^ Number of findings that failed remediation
  , reportTotalTokensUsed :: Int
  -- ^ Total LLM tokens consumed
  , reportDurationSeconds :: Double
  -- ^ Total pipeline duration
  , reportGeneratedAt :: Maybe UTCTime
  -- ^ When the report was generated
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create an empty analysis report
emptyAnalysisReport :: Text -> AnalysisReport
emptyAnalysisReport rid =
  AnalysisReport
    { reportId = rid
    , reportTitle = "AegisCode AI Security Analysis Report"
    , reportSummary = ""
    , reportSections = []
    , reportTotalFindings = 0
    , reportRemediatedCount = 0
    , reportPendingCount = 0
    , reportFailedCount = 0
    , reportTotalTokensUsed = 0
    , reportDurationSeconds = 0.0
    , reportGeneratedAt = Nothing
    }

-- ---------------------------------------------------------------------------
-- Pipeline Event Log
-- ---------------------------------------------------------------------------

-- | Severity level for pipeline events
data EventSeverity
  = EventInfo
  | EventWarning
  | EventError
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A timestamped event in the pipeline execution log
data PipelineEvent = PipelineEvent
  { eventTimestamp :: Maybe UTCTime
  -- ^ When the event occurred
  , eventSeverity :: EventSeverity
  -- ^ Severity level
  , eventPhase :: PipelinePhase
  -- ^ Pipeline phase when event occurred
  , eventAgent :: Text
  -- ^ Which agent or component generated the event
  , eventMessage :: Text
  -- ^ Human-readable event message
  , eventMetadata :: Map Text Text
  -- ^ Additional structured metadata
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create a pipeline event (without timestamp — caller should set it)
logEvent :: PipelinePhase -> Text -> Text -> EventSeverity -> PipelineEvent
logEvent phase agent msg sev =
  PipelineEvent
    { eventTimestamp = Nothing
    , eventSeverity = sev
    , eventPhase = phase
    , eventAgent = agent
    , eventMessage = msg
    , eventMetadata = Map.empty
    }

-- ---------------------------------------------------------------------------
-- Central Pipeline State
-- ---------------------------------------------------------------------------

-- | The central state record flowing through the StateGraph pipeline.
-- Every node in the graph reads and updates this state.
data AegisState = AegisState
  { -- | Conversation message history (for LLM context)
    stateMessages :: [Message]
    -- | Current pipeline phase
  , statePhase :: PipelinePhase
    -- | Unique thread identifier for this pipeline run
  , stateThreadId :: Text
    -- | Unique scan/run identifier
  , stateScanId :: Text
    -- | Target repository path
  , stateRepoPath :: FilePath

    -- Indexing Phase Outputs
    -- | Code chunks generated by RAG indexer
  , stateCodeChunks :: [CodeChunk]
    -- | Symbol dependency graph
  , stateSymbolGraph :: SymbolGraph

    -- Triage Phase Outputs
    -- | Identified vulnerability targets
  , stateVulnerabilities :: [VulnerabilityTarget]
    -- | Security findings with lifecycle tracking
  , stateFindings :: [SecurityFinding]

    -- Refactoring Phase Outputs
    -- | Generated patches
  , statePatches :: [PatchDiff]
    -- | Current vulnerability being worked on
  , stateCurrentVulnerability :: Maybe VulnerabilityTarget
    -- | Current patch being tested
  , stateCurrentPatch :: Maybe PatchDiff

    -- Verification Phase Outputs
    -- | Test results from verification sandbox
  , stateTestResults :: [TestResult]
    -- | Current retry count for refactor-verify loop
  , stateRetryCount :: Int
    -- | Maximum retries allowed
  , stateMaxRetries :: Int

    -- HITL Phase
    -- | Current approval status
  , stateApprovalStatus :: ApprovalStatus
    -- | Human reviewer notes
  , stateReviewerNotes :: Maybe Text

    -- Reporting
    -- | Final analysis report
  , stateReport :: Maybe AnalysisReport

    -- Telemetry
    -- | Pipeline event log
  , stateEventLog :: [PipelineEvent]
    -- | Total tokens consumed so far
  , stateTotalTokensUsed :: Int
    -- | Errors encountered
  , stateErrors :: [Text]
    -- | Iteration counter (for tracking overall progress)
  , stateIterationCount :: Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create an initial AegisState for a new pipeline run
initialAegisState :: Text -> Text -> FilePath -> AegisState
initialAegisState threadId scanId repoPath =
  AegisState
    { stateMessages = []
    , statePhase = PhaseInitializing
    , stateThreadId = threadId
    , stateScanId = scanId
    , stateRepoPath = repoPath
    , stateCodeChunks = []
    , stateSymbolGraph = emptySymbolGraph
    , stateVulnerabilities = []
    , stateFindings = []
    , statePatches = []
    , stateCurrentVulnerability = Nothing
    , stateCurrentPatch = Nothing
    , stateTestResults = []
    , stateRetryCount = 0
    , stateMaxRetries = 3
    , stateApprovalStatus = NotSubmitted
    , stateReviewerNotes = Nothing
    , stateReport = Nothing
    , stateEventLog = []
    , stateTotalTokensUsed = 0
    , stateErrors = []
    , stateIterationCount = 0
    }

-- | Pure StateReducer for AegisState — merges old and new state intelligently.
--
-- Strategy: keep accumulated lists (messages, findings, events), take newer
-- scalar values (phase, current vulnerability, etc.), accumulate counters.
aegisStateReducer :: StateReducer AegisState
aegisStateReducer old new =
  AegisState
    { stateMessages = stateMessages old ++ stateMessages new
    , statePhase = if statePhase new == PhaseInitializing then statePhase old else statePhase new
    , stateThreadId = stateThreadId old
    , stateScanId = stateScanId old
    , stateRepoPath = stateRepoPath old
    , stateCodeChunks = if null (stateCodeChunks new) then stateCodeChunks old else stateCodeChunks new
    , stateSymbolGraph = if stateSymbolGraph new == emptySymbolGraph then stateSymbolGraph old else stateSymbolGraph new
    , stateVulnerabilities = if null (stateVulnerabilities new) then stateVulnerabilities old else stateVulnerabilities new
    , stateFindings = stateFindings old ++ stateFindings new
    , statePatches = statePatches old ++ statePatches new
    , stateCurrentVulnerability = case stateCurrentVulnerability new of
        Nothing -> stateCurrentVulnerability old
        justVt  -> justVt
    , stateCurrentPatch = case stateCurrentPatch new of
        Nothing -> stateCurrentPatch old
        justP   -> justP
    , stateTestResults = stateTestResults old ++ stateTestResults new
    , stateRetryCount = if stateRetryCount new > 0 then stateRetryCount new else stateRetryCount old
    , stateMaxRetries = stateMaxRetries old
    , stateApprovalStatus = case stateApprovalStatus new of
        NotSubmitted -> stateApprovalStatus old
        status       -> status
    , stateReviewerNotes = case stateReviewerNotes new of
        Nothing -> stateReviewerNotes old
        justN   -> justN
    , stateReport = case stateReport new of
        Nothing -> stateReport old
        justR   -> justR
    , stateEventLog = stateEventLog old ++ stateEventLog new
    , stateTotalTokensUsed = stateTotalTokensUsed old + stateTotalTokensUsed new
    , stateErrors = stateErrors old ++ stateErrors new
    , stateIterationCount = stateIterationCount old + stateIterationCount new
    }

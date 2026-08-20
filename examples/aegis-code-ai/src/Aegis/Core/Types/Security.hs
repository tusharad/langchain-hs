{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Core.Types.Security
Description : SARIF v2.1.0 data structures, CVE/CWE identifiers, and vulnerability models
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Domain models for security analysis including SARIF (Static Analysis Results Interchange Format)
result representations, CVE/CWE classification, severity levels, and structured vulnerability
target records used throughout the AegisCode AI pipeline.
-}
module Aegis.Core.Types.Security
  ( -- * Severity Classification
    Severity (..)
  , severityToText
  , textToSeverity

    -- * CVE / CWE Identifiers
  , CVEID (..)
  , CWEID (..)

    -- * Code Location
  , CodeLocation (..)
  , CodeRange (..)

    -- * Vulnerability Target
  , VulnerabilityTarget (..)
  , VulnerabilityCategory (..)
  , RemediationStrategy (..)
  , RemediationHint (..)

    -- * SARIF Types
  , SARIFResult (..)
  , SARIFRun (..)
  , SARIFReport (..)

    -- * Analysis Report
  , SecurityFinding (..)
  , FindingStatus (..)
  , TriageDecision (..)

    -- * Helpers
  , isHighOrCritical
  , sortBySeverity
  , groupByFile
  , countBySeverity
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (sortBy, groupBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Severity Classification
-- ---------------------------------------------------------------------------

-- | Security vulnerability severity levels following CVSS-aligned classification
data Severity
  = Critical
  | High
  | Medium
  | Low
  | Info
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, ToJSON, FromJSON)

-- | Convert severity to display text
severityToText :: Severity -> Text
severityToText Critical = "CRITICAL"
severityToText High     = "HIGH"
severityToText Medium   = "MEDIUM"
severityToText Low      = "LOW"
severityToText Info     = "INFO"

-- | Parse severity from text (case-insensitive)
textToSeverity :: Text -> Maybe Severity
textToSeverity t = case T.toUpper (T.strip t) of
  "CRITICAL" -> Just Critical
  "HIGH"     -> Just High
  "MEDIUM"   -> Just Medium
  "LOW"      -> Just Low
  "INFO"     -> Just Info
  _          -> Nothing

-- | Check whether a severity is High or Critical (needs immediate attention)
isHighOrCritical :: Severity -> Bool
isHighOrCritical Critical = True
isHighOrCritical High     = True
isHighOrCritical _        = False

-- ---------------------------------------------------------------------------
-- CVE / CWE Identifiers
-- ---------------------------------------------------------------------------

-- | CVE (Common Vulnerabilities and Exposures) identifier
newtype CVEID = CVEID { unCVEID :: Text }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | CWE (Common Weakness Enumeration) identifier
newtype CWEID = CWEID { unCWEID :: Text }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Code Location
-- ---------------------------------------------------------------------------

-- | Precise location within a source file
data CodeRange = CodeRange
  { rangeStartLine :: Int
  -- ^ 1-indexed start line
  , rangeStartColumn :: Int
  -- ^ 1-indexed start column
  , rangeEndLine :: Int
  -- ^ 1-indexed end line
  , rangeEndColumn :: Int
  -- ^ 1-indexed end column
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Location of a code element within the repository
data CodeLocation = CodeLocation
  { locFilePath :: FilePath
  -- ^ Relative path from repository root
  , locRange :: Maybe CodeRange
  -- ^ Optional precise range within the file
  , locModuleName :: Maybe Text
  -- ^ Optional module name (for Haskell, Python, etc.)
  , locFunctionName :: Maybe Text
  -- ^ Optional enclosing function/method name
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Vulnerability Category
-- ---------------------------------------------------------------------------

-- | Categories of security vulnerabilities and code quality issues
data VulnerabilityCategory
  = InjectionFlaw
  -- ^ SQL injection, command injection, XSS, etc.
  | AuthenticationIssue
  -- ^ Broken authentication, weak credentials
  | AuthorizationIssue
  -- ^ Broken access control, privilege escalation
  | CryptographicWeakness
  -- ^ Weak algorithms, hardcoded keys, poor random generation
  | DataExposure
  -- ^ Sensitive data leaks, improper logging
  | InsecureDeserialization
  -- ^ Unsafe deserialization of untrusted data
  | DependencyVulnerability
  -- ^ Known vulnerable dependencies
  | CodeSmell
  -- ^ Dead code, unused imports, complexity issues
  | TypeSafetyGap
  -- ^ Partial functions, unsafe coercions, incomplete patterns
  | ResourceLeak
  -- ^ Unclosed handles, memory leaks, thread leaks
  | ConcurrencyIssue
  -- ^ Race conditions, deadlocks, unsafe shared state
  | ErrorHandlingGap
  -- ^ Missing error handling, swallowed exceptions
  | ConfigurationIssue
  -- ^ Insecure defaults, exposed debug endpoints
  | OtherCategory Text
  -- ^ Custom category with description
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Remediation
-- ---------------------------------------------------------------------------

-- | Strategy for remediating a vulnerability
data RemediationStrategy
  = AutoFix
  -- ^ Fully automated fix via patch generation
  | SemiAutoFix
  -- ^ Automated fix with human review required
  | ManualFix
  -- ^ Requires manual human intervention
  | DependencyUpgrade Text
  -- ^ Upgrade dependency to specified version
  | Suppress
  -- ^ Suppress finding (false positive or accepted risk)
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Structured hint for remediation
data RemediationHint = RemediationHint
  { hintDescription :: Text
  -- ^ Human-readable description of the fix
  , hintCodeSuggestion :: Maybe Text
  -- ^ Optional code snippet suggestion
  , hintReferences :: [Text]
  -- ^ Links to relevant documentation or advisories
  , hintEstimatedComplexity :: Text
  -- ^ Estimated fix complexity (e.g., "trivial", "moderate", "complex")
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Vulnerability Target
-- ---------------------------------------------------------------------------

-- | A specific vulnerability or code quality issue identified for remediation
data VulnerabilityTarget = VulnerabilityTarget
  { vtId :: Text
  -- ^ Unique identifier for this vulnerability target
  , vtTitle :: Text
  -- ^ Short human-readable title
  , vtDescription :: Text
  -- ^ Detailed description of the vulnerability
  , vtSeverity :: Severity
  -- ^ Severity classification
  , vtCategory :: VulnerabilityCategory
  -- ^ Category of the issue
  , vtLocation :: CodeLocation
  -- ^ Where the issue is located in the codebase
  , vtCWE :: Maybe CWEID
  -- ^ Optional CWE classification
  , vtCVE :: Maybe CVEID
  -- ^ Optional CVE identifier (for known vulnerabilities)
  , vtRemediation :: RemediationStrategy
  -- ^ Recommended remediation approach
  , vtHints :: [RemediationHint]
  -- ^ Hints for remediation
  , vtConfidence :: Double
  -- ^ Confidence score (0.0 to 1.0)
  , vtAffectedSymbols :: [Text]
  -- ^ List of affected function/type names
  , vtDependencies :: [Text]
  -- ^ List of modules/files that depend on the affected code
  , vtRawSource :: Maybe Text
  -- ^ Optional raw source snippet of the vulnerable code
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- SARIF Types (Simplified v2.1.0)
-- ---------------------------------------------------------------------------

-- | A single result from a SARIF static analysis run
data SARIFResult = SARIFResult
  { sarifRuleId :: Text
  -- ^ Identifier of the rule that produced this result
  , sarifMessage :: Text
  -- ^ Human-readable message
  , sarifLevel :: Text
  -- ^ Severity level ("error", "warning", "note", "none")
  , sarifLocations :: [CodeLocation]
  -- ^ Where the result was found
  , sarifFingerprints :: Map Text Text
  -- ^ Fingerprints for deduplication
  , sarifProperties :: Map Text Text
  -- ^ Additional properties
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A single run of a static analysis tool
data SARIFRun = SARIFRun
  { sarifToolName :: Text
  -- ^ Name of the analysis tool
  , sarifToolVersion :: Text
  -- ^ Version of the analysis tool
  , sarifResults :: [SARIFResult]
  -- ^ Results produced by this run
  , sarifInvocations :: [Text]
  -- ^ Command-line invocations used
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Top-level SARIF report containing one or more runs
data SARIFReport = SARIFReport
  { sarifVersion :: Text
  -- ^ SARIF format version (e.g., "2.1.0")
  , sarifRuns :: [SARIFRun]
  -- ^ Analysis runs
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Analysis Results
-- ---------------------------------------------------------------------------

-- | Status of a security finding through the pipeline
data FindingStatus
  = FindingNew
  -- ^ Newly identified, not yet triaged
  | FindingTriaged
  -- ^ Triaged and prioritized
  | FindingInProgress
  -- ^ Patch generation in progress
  | FindingPatchGenerated
  -- ^ Patch generated, awaiting verification
  | FindingVerified
  -- ^ Patch passed verification tests
  | FindingAwaitingApproval
  -- ^ Awaiting human approval
  | FindingApproved
  -- ^ Approved by human reviewer
  | FindingRejected
  -- ^ Rejected by human reviewer
  | FindingCommitted
  -- ^ Patch committed to repository
  | FindingFailed Text
  -- ^ Failed with reason
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Triage decision made by the TriageAgent
data TriageDecision
  = TriageRemediate
  -- ^ Proceed with automated remediation
  | TriageEscalate
  -- ^ Escalate to human for manual review
  | TriageIgnore Text
  -- ^ Ignore with justification
  | TriageDefer
  -- ^ Defer to a later scan
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A security finding tracked through the entire pipeline lifecycle
data SecurityFinding = SecurityFinding
  { findingId :: Text
  -- ^ Unique finding identifier
  , findingTarget :: VulnerabilityTarget
  -- ^ The underlying vulnerability target
  , findingStatus :: FindingStatus
  -- ^ Current status in the pipeline
  , findingTriageDecision :: Maybe TriageDecision
  -- ^ Triage decision (if triaged)
  , findingPatchDiff :: Maybe Text
  -- ^ Generated unified diff (if patch was generated)
  , findingTestOutput :: Maybe Text
  -- ^ Test execution output (if verified)
  , findingApprovalNotes :: Maybe Text
  -- ^ Human approval notes (if reviewed)
  , findingRetryCount :: Int
  -- ^ Number of refactor→verify retry cycles
  , findingCreatedAt :: Maybe UTCTime
  -- ^ When this finding was first identified
  , findingUpdatedAt :: Maybe UTCTime
  -- ^ When this finding was last updated
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Utility Functions
-- ---------------------------------------------------------------------------

-- | Sort vulnerabilities by severity (Critical first)
sortBySeverity :: [VulnerabilityTarget] -> [VulnerabilityTarget]
sortBySeverity = sortBy (comparing (Down . vtSeverity))

-- | Group vulnerability targets by their file path
groupByFile :: [VulnerabilityTarget] -> [[VulnerabilityTarget]]
groupByFile =
  groupBy (\a b -> locFilePath (vtLocation a) == locFilePath (vtLocation b))
    . sortBy (comparing (locFilePath . vtLocation))

-- | Count vulnerabilities by severity level
countBySeverity :: [VulnerabilityTarget] -> Map Severity Int
countBySeverity = foldl (\acc vt -> Map.insertWith (+) (vtSeverity vt) 1 acc) Map.empty

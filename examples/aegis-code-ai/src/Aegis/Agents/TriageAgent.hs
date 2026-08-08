{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Agents.TriageAgent
Description : Vulnerability & architecture triage agent using ReAct pattern
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Triage agent that analyzes SARIF reports, linter output, and code structure
to identify vulnerability targets, classify severity, and determine remediation
strategy. Uses the ReAct reasoning loop with linter and code search tools.
Wrapped as a StateGraph Node that updates AegisState with triage results.
-}
module Aegis.Agents.TriageAgent
  ( -- * Agent
    runTriageAgent
  , triageNode

    -- * Configuration
  , TriageAgentConfig (..)
  , defaultTriageConfig

    -- * Parsing
  , parseTriageOutput
  , formatTriageResults
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Langchain.Core.Model.Types (Message, systemMessage, userMessage, assistantMessage, extractMessageText)

import Aegis.Core.Types.Pipeline
import Aegis.Core.Types.Security
import Aegis.Middleware.Telemetry (TelemetrySystem, emitAgentStart, emitAgentEnd, emitInfo)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the triage agent
data TriageAgentConfig = TriageAgentConfig
  { taMaxFindings :: Int
  -- ^ Maximum number of findings to process
  , taMinSeverity :: Severity
  -- ^ Minimum severity to include
  , taAutoClassify :: Bool
  -- ^ Whether to auto-classify using heuristics before LLM
  , taSystemPrompt :: Text
  -- ^ System prompt for the triage LLM
  }
  deriving (Eq, Show)

-- | Default triage configuration
defaultTriageConfig :: TriageAgentConfig
defaultTriageConfig = TriageAgentConfig
  { taMaxFindings = 20
  , taMinSeverity = Low
  , taAutoClassify = True
  , taSystemPrompt = triageSystemPrompt
  }

-- ---------------------------------------------------------------------------
-- System Prompt
-- ---------------------------------------------------------------------------

triageSystemPrompt :: Text
triageSystemPrompt = T.unlines
  [ "You are the AegisCode AI Triage Agent, an expert security analyst."
  , "Your role is to analyze source code and static analysis findings to:"
  , ""
  , "1. IDENTIFY security vulnerabilities, code quality issues, and architectural problems"
  , "2. CLASSIFY each finding by severity (CRITICAL, HIGH, MEDIUM, LOW, INFO)"
  , "3. CATEGORIZE each finding (injection, auth, crypto, code-smell, type-safety, resource-leak, etc.)"
  , "4. DETERMINE remediation strategy (auto-fix, semi-auto, manual, suppress)"
  , "5. PRIORITIZE findings for the refactoring pipeline"
  , ""
  , "For each finding, output a structured block:"
  , "```"
  , "FINDING: <title>"
  , "SEVERITY: <CRITICAL|HIGH|MEDIUM|LOW|INFO>"
  , "CATEGORY: <category>"
  , "FILE: <file path>"
  , "LINES: <start>-<end>"
  , "DESCRIPTION: <detailed description>"
  , "REMEDIATION: <AUTO|SEMI_AUTO|MANUAL|SUPPRESS>"
  , "HINT: <suggested fix>"
  , "CONFIDENCE: <0.0-1.0>"
  , "```"
  , ""
  , "Focus on Haskell-specific issues: partial functions, unsafe IO, incomplete patterns,"
  , "unlocked TVars, space leaks, and type-safety gaps."
  ]

-- ---------------------------------------------------------------------------
-- Agent Execution
-- ---------------------------------------------------------------------------

-- | Run the triage agent on the current codebase state
-- Takes messages containing code/linter output, produces vulnerability targets
runTriageAgent
  :: TriageAgentConfig
  -> Maybe TelemetrySystem
  -> [Message]        -- ^ Context messages (code, linter output, etc.)
  -> (Text -> IO Text) -- ^ LLM invocation function
  -> IO ([VulnerabilityTarget], [Message])
runTriageAgent config mbTelemetry contextMsgs invokeLLM = do
  -- Emit telemetry
  mapM_ (\ts -> emitAgentStart ts "TriageAgent") mbTelemetry

  startTime <- getCurrentTime

  -- Step 1: Prepare the triage prompt
  let systemMsg = systemMessage (taSystemPrompt config)
      analysisPrompt = userMessage $ T.unlines
        [ "Analyze the following code and static analysis output for security vulnerabilities"
        , "and code quality issues. Produce structured findings."
        , ""
        , "=== CONTEXT ==="
        , T.unlines (map extractMessageText contextMsgs)
        ]
      fullMessages = [systemMsg, analysisPrompt]

  -- Step 2: Invoke LLM for triage analysis
  llmResponse <- invokeLLM (T.unlines (map extractMessageText fullMessages))

  -- Step 3: Parse the LLM output into structured findings
  let vulnerabilities = parseTriageOutput llmResponse
      filtered = filterBySeverity (taMinSeverity config) vulnerabilities
      capped = take (taMaxFindings config) (sortBySeverity filtered)

  -- Step 4: Auto-classify using heuristics if enabled
  enriched <- if taAutoClassify config
              then pure $ map autoEnrich capped
              else pure capped

  endTime <- getCurrentTime
  let duration = 0.0 -- Placeholder for real time diff
  mapM_ (\ts -> emitAgentEnd ts "TriageAgent" duration) mbTelemetry

  let responseMsg = assistantMessage $ formatTriageResults enriched
  pure (enriched, [responseMsg])

-- | StateGraph node wrapper for the triage agent
triageNode
  :: TriageAgentConfig
  -> Maybe TelemetrySystem
  -> (Text -> IO Text) -- ^ LLM invocation function
  -> AegisState
  -> IO AegisState
triageNode config mbTelemetry invokeLLM state = do
  mapM_ (\ts -> emitInfo ts "TriageAgent" "Starting vulnerability triage...") mbTelemetry

  -- Build context from indexed code chunks
  let contextMsgs = stateMessages state
  (vulnerabilities, newMsgs) <- runTriageAgent config mbTelemetry contextMsgs invokeLLM

  -- Create security findings from vulnerability targets
  let findings = map mkFinding vulnerabilities

  pure state
    { statePhase = PhaseTriaging
    , stateVulnerabilities = vulnerabilities
    , stateFindings = findings
    , stateMessages = newMsgs
    , stateEventLog = stateEventLog state ++
        [logEvent PhaseTriaging "TriageAgent"
          ("Identified " <> T.pack (show (length vulnerabilities)) <> " vulnerabilities") EventInfo]
    , stateIterationCount = stateIterationCount state + 1
    }

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse LLM triage output into structured VulnerabilityTargets
parseTriageOutput :: Text -> [VulnerabilityTarget]
parseTriageOutput output =
  let blocks = T.splitOn "FINDING:" output
      parsed = map parseBlock (drop 1 blocks)  -- Skip text before first FINDING
  in [vt | Just vt <- parsed]

-- | Parse a single finding block
parseBlock :: Text -> Maybe VulnerabilityTarget
parseBlock block =
  let ls = T.lines block
      fields = Map.fromList [(T.strip key, T.strip val)
                            | l <- ls
                            , let parts = T.breakOn ":" l
                            , not (T.null (snd parts))
                            , let key = fst parts
                            , let val = T.drop 1 (snd parts)
                            , not (T.null (T.strip key))
                            ]
      title = T.strip $ T.takeWhile (/= '\n') (T.strip block)
  in if T.null title then Nothing else Just VulnerabilityTarget
       { vtId = "triage-" <> T.take 8 (T.filter (/= ' ') title)
       , vtTitle = title
       , vtDescription = Map.findWithDefault "" "DESCRIPTION" fields
       , vtSeverity = maybe Medium id (textToSeverity (Map.findWithDefault "MEDIUM" "SEVERITY" fields))
       , vtCategory = parseCategory (Map.findWithDefault "code-smell" "CATEGORY" fields)
       , vtLocation = CodeLocation
           { locFilePath = T.unpack (Map.findWithDefault "<unknown>" "FILE" fields)
           , locRange = parseLineRange (Map.findWithDefault "" "LINES" fields)
           , locModuleName = Nothing
           , locFunctionName = Nothing
           }
       , vtCWE = Nothing
       , vtCVE = Nothing
       , vtRemediation = parseRemediationStrategy (Map.findWithDefault "SEMI_AUTO" "REMEDIATION" fields)
       , vtHints = case Map.lookup "HINT" fields of
           Nothing -> []
           Just h -> [RemediationHint h Nothing [] "moderate"]
       , vtConfidence = parseConfidence (Map.findWithDefault "0.7" "CONFIDENCE" fields)
       , vtAffectedSymbols = []
       , vtDependencies = []
       , vtRawSource = Nothing
       }

-- | Parse a line range like "10-25"
parseLineRange :: Text -> Maybe CodeRange
parseLineRange t =
  case T.splitOn "-" (T.strip t) of
    [startT, endT] ->
      let start = readInt startT
          end = readInt endT
      in if start > 0 && end > 0
         then Just $ CodeRange start 1 end 1
         else Nothing
    _ -> Nothing
  where
    readInt :: Text -> Int
    readInt txt = case reads (T.unpack txt) :: [(Int, String)] of
      [(n, _)] -> n
      _ -> 0

-- | Parse vulnerability category
parseCategory :: Text -> VulnerabilityCategory
parseCategory t = case T.toLower (T.strip t) of
  "injection"        -> InjectionFlaw
  "authentication"   -> AuthenticationIssue
  "authorization"    -> AuthorizationIssue
  "crypto"           -> CryptographicWeakness
  "cryptographic"    -> CryptographicWeakness
  "data-exposure"    -> DataExposure
  "deserialization"  -> InsecureDeserialization
  "dependency"       -> DependencyVulnerability
  "code-smell"       -> CodeSmell
  "type-safety"      -> TypeSafetyGap
  "resource-leak"    -> ResourceLeak
  "concurrency"      -> ConcurrencyIssue
  "error-handling"   -> ErrorHandlingGap
  "configuration"    -> ConfigurationIssue
  other              -> OtherCategory other

-- | Parse remediation strategy
parseRemediationStrategy :: Text -> RemediationStrategy
parseRemediationStrategy t = case T.toUpper (T.strip t) of
  "AUTO"      -> AutoFix
  "SEMI_AUTO" -> SemiAutoFix
  "MANUAL"    -> ManualFix
  "SUPPRESS"  -> Suppress
  _           -> SemiAutoFix

-- | Parse confidence score
parseConfidence :: Text -> Double
parseConfidence t = case reads (T.unpack (T.strip t)) :: [(Double, String)] of
  [(n, _)] | n >= 0.0 && n <= 1.0 -> n
  _ -> 0.7

-- ---------------------------------------------------------------------------
-- Formatting
-- ---------------------------------------------------------------------------

-- | Format triage results for display / message passing
formatTriageResults :: [VulnerabilityTarget] -> Text
formatTriageResults vts = T.unlines $
  ["=== Triage Results ===", ""]
  ++ ["Total findings: " <> T.pack (show (length vts)), ""]
  ++ concatMap formatVt vts
  where
    formatVt vt =
      [ "[" <> severityToText (vtSeverity vt) <> "] " <> vtTitle vt
      , "  File: " <> T.pack (locFilePath (vtLocation vt))
      , "  Category: " <> T.pack (show (vtCategory vt))
      , "  Remediation: " <> T.pack (show (vtRemediation vt))
      , "  Confidence: " <> T.pack (show (vtConfidence vt))
      , ""
      ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Filter vulnerabilities by minimum severity
filterBySeverity :: Severity -> [VulnerabilityTarget] -> [VulnerabilityTarget]
filterBySeverity minSev = filter (\vt -> vtSeverity vt <= minSev)

-- | Auto-enrich vulnerability targets with heuristic classification
autoEnrich :: VulnerabilityTarget -> VulnerabilityTarget
autoEnrich vt = vt
  { vtCWE = inferCWE (vtCategory vt)
  }

-- | Infer CWE from category
inferCWE :: VulnerabilityCategory -> Maybe CWEID
inferCWE InjectionFlaw = Just (CWEID "CWE-79")
inferCWE AuthenticationIssue = Just (CWEID "CWE-287")
inferCWE AuthorizationIssue = Just (CWEID "CWE-862")
inferCWE CryptographicWeakness = Just (CWEID "CWE-327")
inferCWE DataExposure = Just (CWEID "CWE-200")
inferCWE InsecureDeserialization = Just (CWEID "CWE-502")
inferCWE ResourceLeak = Just (CWEID "CWE-404")
inferCWE ConcurrencyIssue = Just (CWEID "CWE-362")
inferCWE _ = Nothing

-- | Create a SecurityFinding from a VulnerabilityTarget
mkFinding :: VulnerabilityTarget -> SecurityFinding
mkFinding vt = SecurityFinding
  { findingId = vtId vt
  , findingTarget = vt
  , findingStatus = FindingTriaged
  , findingTriageDecision = Just TriageRemediate
  , findingPatchDiff = Nothing
  , findingTestOutput = Nothing
  , findingApprovalNotes = Nothing
  , findingRetryCount = 0
  , findingCreatedAt = Nothing
  , findingUpdatedAt = Nothing
  }

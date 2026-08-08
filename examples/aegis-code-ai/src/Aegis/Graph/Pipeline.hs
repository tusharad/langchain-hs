{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Graph.Pipeline
Description : Main StateGraph construction and execution for the AegisCode pipeline
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Constructs and executes the full AegisCode AI StateGraph pipeline. Defines
the complete node graph with conditional routing, cyclic retry branches,
and HITL interrupt points. This is the top-level orchestration module.
-}
module Aegis.Graph.Pipeline
  ( -- * Pipeline Construction
    buildPipeline
  , PipelineContext (..)

    -- * Pipeline Execution
  , runPipeline
  , runDemoPipeline

    -- * Pipeline Results
  , PipelineResult (..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Langchain.Core.Model.Types (Message, assistantMessage, systemMessage, extractMessageText)

import Aegis.Core.Types.Config
import Aegis.Core.Types.Pipeline
import Aegis.Core.Types.Security
import Aegis.Middleware.Telemetry (TelemetrySystem, newTelemetrySystem, emitInfo, emitNodeStart, emitNodeEnd)
import Aegis.Middleware.TokenBudget (TokenBudgetManager, newTokenBudgetManager, checkBudget, BudgetWarning (..))
import Aegis.Middleware.CircuitBreaker (CircuitBreaker, newCircuitBreaker)
import Aegis.Middleware.ProviderFailover (FailoverEngine, newFailoverEngine, invokeWithFailover, FailoverResult (..))
import Aegis.RAG.HybridRetriever (HybridRetriever, newHybridRetriever, defaultHybridRetrieverConfig)
import Aegis.Graph.Nodes
import Aegis.Graph.SubGraphs
import Aegis.Agents.Supervisor (shouldContinue, isTerminalPhase, generateReport, routeNextAgent, SupervisorDecision (..))

-- ---------------------------------------------------------------------------
-- Pipeline Context
-- ---------------------------------------------------------------------------

-- | Runtime context for the pipeline, holding all initialized subsystems
data PipelineContext = PipelineContext
  { pcConfig :: AegisConfig
  -- ^ System configuration
  , pcTelemetry :: TelemetrySystem
  -- ^ Telemetry system
  , pcTokenBudget :: TokenBudgetManager
  -- ^ Token budget manager
  , pcCircuitBreaker :: CircuitBreaker
  -- ^ Circuit breaker for model health
  , pcFailoverEngine :: FailoverEngine
  -- ^ Multi-model failover engine
  , pcRetriever :: HybridRetriever
  -- ^ Hybrid retriever for RAG
  , pcInvokeLLM :: Text -> IO Text
  -- ^ LLM invocation function
  }

-- | Pipeline execution result
data PipelineResult = PipelineResult
  { prFinalState :: AegisState
  -- ^ Final pipeline state
  , prReport :: Maybe AnalysisReport
  -- ^ Generated report (if pipeline completed)
  , prTotalIterations :: Int
  -- ^ Total iterations executed
  , prTotalTokensUsed :: Int
  -- ^ Total tokens consumed
  , prDurationSeconds :: Double
  -- ^ Total execution time
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Pipeline Construction
-- ---------------------------------------------------------------------------

-- | Build a complete pipeline context from configuration
buildPipeline :: AegisConfig -> IO PipelineContext
buildPipeline config = do
  telemetry <- newTelemetrySystem
  tokenBudget <- newTokenBudgetManager (configTokenBudget config)
  circuitBreaker <- newCircuitBreaker (configCircuitBreaker config)
  failoverEngine <- newFailoverEngine (configLLM config)
  retriever <- newHybridRetriever defaultHybridRetrieverConfig

  -- Create the LLM invocation function using failover
  let invokeLLM prompt = do
        result <- invokeWithFailover failoverEngine $ \modelName -> do
          -- In a real implementation, this would call Ollama's API
          -- For now, use the mock LLM
          mockResponse <- mockLLMWithModel modelName prompt
          pure $ Right mockResponse
        case frResult result of
          Left err -> pure $ "LLM Error: " <> err
          Right response -> pure response

  pure PipelineContext
    { pcConfig = config
    , pcTelemetry = telemetry
    , pcTokenBudget = tokenBudget
    , pcCircuitBreaker = circuitBreaker
    , pcFailoverEngine = failoverEngine
    , pcRetriever = retriever
    , pcInvokeLLM = invokeLLM
    }

-- | Mock LLM that incorporates model name for demonstration
mockLLMWithModel :: Text -> Text -> IO Text
mockLLMWithModel modelName prompt = pure $ T.unlines
  [ "[Model: " <> modelName <> "]"
  , ""
  , "FINDING: Partial function usage in pattern match"
  , "SEVERITY: MEDIUM"
  , "CATEGORY: type-safety"
  , "FILE: src/Example.hs"
  , "LINES: 15-20"
  , "DESCRIPTION: Usage of 'head' on a potentially empty list without safety check."
  , "REMEDIATION: AUTO"
  , "HINT: Replace 'head xs' with 'case xs of { [] -> defaultValue; (x:_) -> x }'"
  , "CONFIDENCE: 0.85"
  , ""
  , "FINDING: Missing error handling in IO action"
  , "SEVERITY: LOW"
  , "CATEGORY: error-handling"
  , "FILE: src/Utils.hs"
  , "LINES: 42-48"
  , "DESCRIPTION: readFile call without exception handling could crash on missing files."
  , "REMEDIATION: SEMI_AUTO"
  , "HINT: Wrap with 'try' or 'catchIOError' and handle the failure case."
  , "CONFIDENCE: 0.9"
  ]

-- ---------------------------------------------------------------------------
-- Pipeline Execution
-- ---------------------------------------------------------------------------

-- | Run the full pipeline with supervisor-driven routing
runPipeline :: PipelineContext -> FilePath -> IO PipelineResult
runPipeline ctx repoPath = do
  startTime <- getCurrentTime

  emitInfo (pcTelemetry ctx) "Pipeline" "AegisCode AI pipeline starting..."

  -- Initialize state
  let initialState = initialAegisState "thread-001" "scan-001" repoPath

  -- Create node configuration
  let nc = NodeConfig
        { ncTelemetry = Just (pcTelemetry ctx)
        , ncRetriever = Just (pcRetriever ctx)
        , ncInvokeLLM = pcInvokeLLM ctx
        , ncConfig = pcConfig ctx
        }

  -- Run the supervisor loop
  finalState <- supervisorLoop ctx nc initialState

  endTime <- getCurrentTime

  emitInfo (pcTelemetry ctx) "Pipeline"
    ("Pipeline completed with phase: " <> phaseToText (statePhase finalState))

  pure PipelineResult
    { prFinalState = finalState
    , prReport = stateReport finalState
    , prTotalIterations = stateIterationCount finalState
    , prTotalTokensUsed = stateTotalTokensUsed finalState
    , prDurationSeconds = 0.0
    }

-- | The main supervisor loop that drives the pipeline
supervisorLoop :: PipelineContext -> NodeConfig -> AegisState -> IO AegisState
supervisorLoop ctx nc state
  | isTerminalPhase (statePhase state) = pure state
  | stateIterationCount state >= 100 = do
      emitInfo (pcTelemetry ctx) "Pipeline" "Safety iteration limit reached (100)"
      pure state { statePhase = PhaseFailed "Safety iteration limit reached" }
  | otherwise = do
      -- Check token budget
      budgetStatus <- checkBudget (pcTokenBudget ctx)
      case budgetStatus of
        BudgetExceeded -> do
          emitInfo (pcTelemetry ctx) "Pipeline" "Token budget exceeded"
          pure state { statePhase = PhaseFailed "Token budget exceeded" }
        _ -> do
          -- Determine next action
          let decision = routeNextAgent state
          emitInfo (pcTelemetry ctx) "Pipeline"
            ("Routing: " <> T.pack (show decision))

          -- Execute the appropriate node
          nextState <- executeNode ctx nc decision state

          -- Continue the loop
          supervisorLoop ctx nc nextState

-- | Execute a specific node based on the supervisor's routing decision
executeNode :: PipelineContext -> NodeConfig -> SupervisorDecision -> AegisState -> IO AegisState
executeNode ctx nc decision state = do
  emitNodeStart (pcTelemetry ctx) (decisionNodeName decision)
  result <- case decision of
    RouteToIndexer  -> mkIndexNode nc state
    RouteToTriage   -> mkTriageNode nc state
    RouteToRefactor -> mkRefactorNode nc state
    RouteToVerify   -> mkVerifyNode nc state
    RouteToHITL     -> mkHITLNode nc state
    RouteToCommit   -> mkCommitNode nc state
    RouteToReport   -> mkReportNode nc state
    RouteComplete   -> generateReport (Just (pcTelemetry ctx)) state
    RouteFailed reason -> pure state
      { statePhase = PhaseFailed reason
      , stateErrors = stateErrors state ++ [reason]
      }
  emitNodeEnd (pcTelemetry ctx) (decisionNodeName decision) 0.0
  pure result

-- | Get a display name for a routing decision
decisionNodeName :: SupervisorDecision -> Text
decisionNodeName RouteToIndexer  = "IndexNode"
decisionNodeName RouteToTriage   = "TriageNode"
decisionNodeName RouteToRefactor = "RefactorNode"
decisionNodeName RouteToVerify   = "VerifyNode"
decisionNodeName RouteToHITL     = "HITLNode"
decisionNodeName RouteToCommit   = "CommitNode"
decisionNodeName RouteToReport   = "ReportNode"
decisionNodeName RouteComplete   = "Complete"
decisionNodeName (RouteFailed _) = "Failed"

-- ---------------------------------------------------------------------------
-- Demo Pipeline
-- ---------------------------------------------------------------------------

-- | Run a demo pipeline with mock LLM (no external dependencies)
runDemoPipeline :: FilePath -> IO PipelineResult
runDemoPipeline repoPath = do
  let config = defaultAegisConfig
        { configScan = (configScan defaultAegisConfig)
            { scanRequireHITL = False
            , scanMaxRefactorRetries = 1
            }
        }
  ctx <- buildPipeline config
  runPipeline ctx repoPath

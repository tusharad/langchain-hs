{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Graph.Nodes
Description : Individual graph node definitions for the AegisCode pipeline
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Defines all graph nodes for the AegisCode AI StateGraph pipeline. Each node
is a function @AegisState -> IO AegisState@ that performs a specific step
in the security analysis workflow.
-}
module Aegis.Graph.Nodes
  ( -- * Node Constructors
    mkIndexNode
  , mkTriageNode
  , mkRefactorNode
  , mkVerifyNode
  , mkHITLNode
  , mkCommitNode
  , mkReportNode
  , mkSupervisorNode

    -- * Router Nodes
  , mkRouterNode
  , mkRetryRouterNode

    -- * Node Configuration
  , NodeConfig (..)
  , defaultNodeConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Langchain.Core.Model.Types (Message, assistantMessage, systemMessage)

import Aegis.Core.Types.Pipeline
import Aegis.Core.Types.Config (AegisConfig (..))
import Aegis.Middleware.Telemetry (TelemetrySystem, emitNodeStart, emitNodeEnd, emitInfo)
import Aegis.RAG.CodeLoader (defaultCodeLoaderConfig, loadCodebase, LoadedFile (..))
import Aegis.RAG.ASTChunker (chunkSourceFile, defaultChunkerConfig)
import Aegis.RAG.HybridRetriever (HybridRetriever)
import Aegis.RAG.Indexer (indexCodebase, IndexResult (..))
import Aegis.Agents.TriageAgent (triageNode, defaultTriageConfig)
import Aegis.Agents.RefactorAgent (refactorNode, defaultRefactorConfig)
import Aegis.Agents.VerificationAgent (verificationNode, defaultVerificationConfig)
import Aegis.Agents.HITLAgent (hitlReviewNode)
import Aegis.Agents.Supervisor (supervisorNode, commitNode, generateReport, routeNextAgent, SupervisorDecision (..))

-- ---------------------------------------------------------------------------
-- Node Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for constructing pipeline nodes
data NodeConfig = NodeConfig
  { ncTelemetry :: Maybe TelemetrySystem
  -- ^ Telemetry system for event emission
  , ncRetriever :: Maybe HybridRetriever
  -- ^ Hybrid retriever for RAG
  , ncInvokeLLM :: Text -> IO Text
  -- ^ LLM invocation function
  , ncConfig :: AegisConfig
  -- ^ System configuration
  }

-- | Default node configuration with a mock LLM
defaultNodeConfig :: AegisConfig -> NodeConfig
defaultNodeConfig config = NodeConfig
  { ncTelemetry = Nothing
  , ncRetriever = Nothing
  , ncInvokeLLM = mockLLM
  , ncConfig = config
  }

-- | Mock LLM for demo/testing
mockLLM :: Text -> IO Text
mockLLM prompt = pure $ T.unlines
  [ "FINDING: Example vulnerability"
  , "SEVERITY: MEDIUM"
  , "CATEGORY: code-smell"
  , "FILE: src/Example.hs"
  , "LINES: 10-20"
  , "DESCRIPTION: This is a mock finding for demonstration purposes."
  , "REMEDIATION: AUTO"
  , "HINT: Replace partial function with total alternative."
  , "CONFIDENCE: 0.8"
  ]

-- ---------------------------------------------------------------------------
-- Index Node
-- ---------------------------------------------------------------------------

-- | Create the codebase indexing node
mkIndexNode :: NodeConfig -> AegisState -> IO AegisState
mkIndexNode nc state = do
  let ts = ncTelemetry nc
  mapM_ (\t -> emitNodeStart t "IndexNode") ts
  mapM_ (\t -> emitInfo t "IndexNode" ("Indexing codebase at: " <> T.pack (stateRepoPath state))) ts

  -- Run the indexing pipeline
  case ncRetriever nc of
    Nothing -> do
      -- Without a retriever, just load and chunk the files
      loadResult <- loadCodebase defaultCodeLoaderConfig (stateRepoPath state)
      case loadResult of
        Left err -> do
          mapM_ (\t -> emitNodeEnd t "IndexNode" 0.0) ts
          pure state
            { statePhase = PhaseIndexing
            , stateErrors = stateErrors state ++ [err]
            , stateMessages = [assistantMessage $ "Indexing failed: " <> err]
            , stateEventLog = stateEventLog state ++
                [logEvent PhaseIndexing "IndexNode" ("Indexing failed: " <> err) EventError]
            }
        Right loadedFiles -> do
          let allChunks = concatMap lfChunks loadedFiles
          mapM_ (\t -> emitInfo t "IndexNode"
            ("Indexed " <> T.pack (show (length loadedFiles)) <> " files, "
             <> T.pack (show (length allChunks)) <> " chunks")) ts
          mapM_ (\t -> emitNodeEnd t "IndexNode" 0.0) ts
          pure state
            { statePhase = PhaseIndexing
            , stateCodeChunks = allChunks
            , stateMessages =
                [assistantMessage $ "Indexed " <> T.pack (show (length loadedFiles)) <> " files into "
                  <> T.pack (show (length allChunks)) <> " code chunks."]
            , stateEventLog = stateEventLog state ++
                [logEvent PhaseIndexing "IndexNode"
                  ("Indexed " <> T.pack (show (length loadedFiles)) <> " files") EventInfo]
            , stateIterationCount = stateIterationCount state + 1
            }

    Just retriever -> do
      indexResult <- indexCodebase retriever defaultCodeLoaderConfig (stateRepoPath state)
      mapM_ (\t -> emitNodeEnd t "IndexNode" (irDurationSeconds indexResult)) ts
      pure state
        { statePhase = PhaseIndexing
        , stateCodeChunks = []  -- Chunks are in the retriever
        , stateErrors = stateErrors state ++ irErrors indexResult
        , stateMessages =
            [assistantMessage $ "Indexed " <> T.pack (show (irTotalFiles indexResult)) <> " files, "
              <> T.pack (show (irTotalChunks indexResult)) <> " chunks, "
              <> T.pack (show (irTotalSymbols indexResult)) <> " symbols."]
        , stateEventLog = stateEventLog state ++ irEvents indexResult
        , stateIterationCount = stateIterationCount state + 1
        }

-- ---------------------------------------------------------------------------
-- Agent Node Wrappers
-- ---------------------------------------------------------------------------

-- | Create the triage node
mkTriageNode :: NodeConfig -> AegisState -> IO AegisState
mkTriageNode nc = triageNode defaultTriageConfig (ncTelemetry nc) (ncInvokeLLM nc)

-- | Create the refactor node
mkRefactorNode :: NodeConfig -> AegisState -> IO AegisState
mkRefactorNode nc = refactorNode defaultRefactorConfig (ncTelemetry nc) (ncRetriever nc) (ncInvokeLLM nc)

-- | Create the verification node
mkVerifyNode :: NodeConfig -> AegisState -> IO AegisState
mkVerifyNode nc = verificationNode defaultVerificationConfig (ncTelemetry nc)

-- | Create the HITL review node
mkHITLNode :: NodeConfig -> AegisState -> IO AegisState
mkHITLNode nc = hitlReviewNode (ncTelemetry nc)

-- | Create the commit node
mkCommitNode :: NodeConfig -> AegisState -> IO AegisState
mkCommitNode nc = commitNode (ncTelemetry nc)

-- | Create the report generation node
mkReportNode :: NodeConfig -> AegisState -> IO AegisState
mkReportNode nc = generateReport (ncTelemetry nc)

-- | Create the supervisor node
mkSupervisorNode :: NodeConfig -> AegisState -> IO AegisState
mkSupervisorNode nc = supervisorNode (ncTelemetry nc)

-- ---------------------------------------------------------------------------
-- Router Nodes
-- ---------------------------------------------------------------------------

-- | Create a router node that directs flow based on supervisor decision
mkRouterNode :: NodeConfig -> AegisState -> IO Text
mkRouterNode _nc state =
  let decision = routeNextAgent state
  in pure $ case decision of
       RouteToIndexer  -> "index"
       RouteToTriage   -> "triage"
       RouteToRefactor -> "refactor"
       RouteToVerify   -> "verify"
       RouteToHITL     -> "hitl"
       RouteToCommit   -> "commit"
       RouteToReport   -> "report"
       RouteComplete   -> "__end__"
       RouteFailed _   -> "__end__"

-- | Create a retry router that decides between refactor retry and next step
mkRetryRouterNode :: NodeConfig -> AegisState -> IO Text
mkRetryRouterNode _nc state = do
  let retries = stateRetryCount state
      maxRetries = stateMaxRetries state
  if retries < maxRetries
    then do
      -- Check if last test failed
      case stateTestResults state of
        [] -> pure "report"
        results ->
          if testOutcome (last results) == TestPassed
          then pure "hitl"
          else pure "refactor"
    else pure "report"  -- Max retries, go to report

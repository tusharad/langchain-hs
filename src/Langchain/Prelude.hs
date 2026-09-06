{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Prelude
Description : Canonical umbrella re-export module for langchain-hs
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Exports all core data types, typeclasses, models, vector stores, memory stores,
graph orchestration primitives, advanced multi-agent patterns, guardrails, MCP client,
observability, structured logging, circuit breakers, pipeline DSLs, and runtime execution monads.
-}
module Langchain.Prelude
  ( -- * Core Monad & Errors
    LangchainT
  , LangchainConfig (LangchainConfig)
  , defaultLangchainConfig
  , runLangchainT
  , runLangchainTIO
  , askConfig
  , withConfig
  , LangchainError (..)
  , ErrorContext (..)
  , errorMessage
  , mkContext
  , mkContextIO
  , LangchainResult
  , llmError
  , parsingError
  , vectorStoreError
  , documentLoaderError
  , embeddingError
  , runnableError
  , toolError
  , agentError
  , memoryError
  , networkError
  , configurationError
  , validationError
  , internalError

    -- * Multi-Modal Models & Messages
  , ChatModel (..)
  , Message (..)
  , Role
  , ContentBlock (..)
  , ToolCall (..)
  , textMessage
  , userMessage
  , systemMessage
  , assistantMessage
  , toolMessage
  , imageMessage
  , extractMessageText
  , StreamEvent
    ( LLMStart
    , LLMChunk
    , LLMEnd
    , ToolStart
    , ToolEnd
    , ToolErrorEvent
    , ChainStart
    , ChainEnd
    , NodeStart
    , NodeEnd
    )
  , TokenUsage (..)
  , EventStream
  , collectEvents
  , printEvents

    -- * Pure AST Pipelines (RunnableTree)
  , RunnableTree (..)
  , (|>>)
  , (&>&)
  , interpret
  , runLambda
  , runPrim

    -- * Effect-Polymorphic Tools
  , Tool (..)
  , createTool
  , toolToValue
  , DeriveToolSchema (..)
  , deriveToolParametersSchema
  , executeToolAsync
  , executeToolWithTimeout
  , executeToolBatchConcurrently

    -- * State Graphs & Multi-Agent
  , StateGraph (..)
  , Node (Node)
  , Edge (..)
  , NodeId
  , startNodeId
  , endNodeId
  , StateReducer
  , emptyStateGraph
  , addNode
  , addEdge
  , addConditionalEdge
  , compileGraph
  , runGraph
  , appendMessagesReducer
  , replaceFieldReducer
  , Checkpointer (..)
  , MemoryCheckpointer (..)
  , newMemoryCheckpointer
  , SQLiteCheckpointer (..)
  , newSQLiteCheckpointer
  , hitlNode
  , resumeGraph
  , supervisorNode
  , embedSubGraphNode
  , parallelNode
  , addParallelNodes

    -- * Advanced Agent Patterns
  , PlanAndExecuteAgent (..)
  , newPlanAndExecuteAgent
  , runPlanAndExecute
  , FunctionsAgent (FunctionsAgent)
  , newFunctionsAgent
  , runFunctionsAgent
  , SpecialistAgent (..)
  , SupervisorTeam (..)
  , newSupervisorTeam
  , runSupervisorTeam

    -- * Guardrails & Safety
  , GuardrailResult (..)
  , Guardrail (..)
  , contentSafetyGuardrail
  , topicGuardrail
  , outputLengthGuardrail
  , composeGuardrails
  , withGuardrails

    -- * Model Context Protocol (MCP) Client
  , McpTransport (..)
  , McpToolInfo (..)
  , McpResource (..)
  , McpClient (..)
  , newStdioMcpClient
  , newHttpMcpClient
  , listMcpTools
  , callMcpTool
  , mcpToolToLangchainTool

    -- * Telemetry, Logging & OpenTelemetry
  , LogLevel (..)
  , LogEvent (..)
  , Logger (..)
  , InMemoryLogger (..)
  , newInMemoryLogger
  , getInMemoryLogs
  , stderrLogger
  , logEvent
  , logDebug
  , logInfo
  , logWarn
  , logError
  , SpanKind (..)
  , SpanStatus (..)
  , Span (..)
  , OTelTracer (..)
  , newOTelTracer
  , getSpans
  , startSpan
  , endSpan
  , addSpanAttribute
  , withSpan
  , exportSpansJson

    -- * Callbacks
  , CallbackEvent (..)
  , CallbackHandler (..)
  , CallbackManager (..)
  , newCallbackManager
  , registerHandler
  , dispatchEvent
  , dispatchEventAsync
  , newLoggingCallbackHandler

    -- * Resilience
  , CircuitState (..)
  , CircuitBreakerConfig (..)
  , defaultCircuitConfig
  , CircuitBreaker (..)
  , newCircuitBreaker
  , getCircuitState
  , withCircuitBreaker

    -- * Memory Systems
  , BaseMemory (..)
  , WindowBufferMemory (..)
  , newWindowBufferMemory
  , TokenBufferMemory (..)
  , newTokenBufferMemory
  , countTokens
  , SummaryMemory (..)
  , newSummaryMemory
  , EntityMemory (..)
  , newEntityMemory
  , initialMessages
  , trimMessages

    -- * Vector Stores & Retrieval
  , VectorStore (..)
  , InMemory (..)
  , emptyInMemoryVectorStore
  , fromDocuments
  , SqliteVecStore (..)
  , newSqliteVecStore
  , Retriever (..)
  , VectorStoreRetriever (..)

    -- * Embeddings
  , Embeddings (..)

    -- * Document Loaders
  , Document (..)
  , BaseLoader (..)
  , FileLoader (..)
  , DirectoryLoader (..)
  , DirectoryLoaderOptions (..)
  , defaultDirectoryLoaderOptions
  , CsvLoader (..)
  , defaultCsvLoader
  , JsonLoader (..)
  , defaultJsonLoader
  , HtmlLoader (..)
  , defaultHtmlLoader
  , WebPageLoader (..)
  , defaultWebPageLoader

    -- * Prompt Templates
  , PromptTemplate (..)
  , PromptTemplateOptions (..)
  , TemplateFormat (..)
  , defaultPromptTemplateOptions
  , fromTemplate
  , fromTemplateWithOptions
  , fromTemplateWithFormat
  , partialPromptTemplate
  , FewShotPromptTemplate (..)
  , renderPrompt
  , renderFewShotPrompt

    -- * Text Splitters
  , CharacterSplitterOps (CharacterSplitterOps)
  , defaultCharacterSplitterOps
  , splitText
  , RecursiveCharacterSplitterOps (RecursiveCharacterSplitterOps)
  , defaultRecursiveCharacterSplitterOps
  , splitTextRecursive
  , MarkdownSplitterOps (MarkdownSplitterOps)
  , defaultMarkdownSplitterOps
  , splitMarkdown
  , splitMarkdownToChunks
  , TokenSplitterOps (TokenSplitterOps)
  , defaultTokenSplitterOps
  , splitByTokens
  , Language (..)
  , CodeSplitterOps (CodeSplitterOps)
  , splitCode

    -- * Caching & Resilience
  , CacheBackend (..)
  , InMemoryCache (..)
  , newInMemoryCache
  , SQLiteCache (..)
  , newSQLiteCache
  , CachedModel (..)
  , withCaching
  , RetryPolicy (RetryPolicy)
  , defaultRetryPolicy
  , withRetry
  , RateLimiter (..)
  , newRateLimiter
  , withRateLimit

    -- * Chains
  , RetrievalQA (RetrievalQA)
  , newRetrievalQA
  , runRetrievalQA
  , MapReduceChain (..)
  , newMapReduceChain
  , runMapReduceChain

    -- * Structured Output & Parsers
  , OutputParser (..)
  , CommaSeparatedList (..)
  , JSONOutputStructure (..)
  , NumberSeparatedList (..)
  , StructuredOutput (..)
  , TypeSchema (..)
  , toOllamaSchema
  , fromOllamaSchema
  , structuredInvoke
  , structuredInvokeWithRetries
  , withJsonFormat
  , withSchemaFormat
  , withStructuredOutput

    -- * Agents & Execution
  , ReActAgent (ReActAgent)
  , AgentStep (..)
  , createReActAgent
  , reactStep
  , runReActAgent
  , AgentMiddleware (..)
  , defaultMiddleware
  , chainMiddlewares
  , loggingMiddleware

    -- * Hybrid Retrieval & BM25
  , BM25Index (..)
  , newBM25Index
  , newBM25IndexWithParams
  , addDocumentsBM25
  , bm25Search
  , bm25SearchWithScores
  , HybridRetriever (..)
  , newHybridRetriever
  , newHybridRetrieverWithWeights
  , searchHybrid
  , searchHybridWithScores
  , reciprocalRankFusion

    -- * Providers
  , Ollama (..)
  , OllamaClientConfig (..)
  , defaultConfig
  , newOllama
  , newOllamaWithClient
  , ModelOptions (..)
  , defaultOptions
  , withOptions
  , chatRequestFor
  , resolveChatRequest
  , withTools
  , toOllamaTool
  , toOllamaTools
  , OpenAI
  , newOpenAI
  , Gemini
  , newGemini
  ) where

import Langchain.Agent.Functions
import Langchain.Agent.Middleware
import Langchain.Agent.PlanAndExecute
import Langchain.Agent.ReAct
import Langchain.Agent.Supervisor
import Langchain.Cache.Core
import Langchain.Callback.Manager
import Langchain.Chain.MapReduce
import Langchain.Chain.RetrievalQA
import Langchain.Core.Error
import Langchain.Core.Model
import Langchain.Core.Monad hiding (defaultConfig)
import qualified Langchain.Core.Monad as CoreMonad
import Langchain.Core.Runnable
import Langchain.Core.Stream
import Langchain.Core.Tool
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.Csv
import Langchain.DocumentLoader.DirectoryLoader
import Langchain.DocumentLoader.FileLoader
import Langchain.DocumentLoader.Html
import Langchain.DocumentLoader.Json
import Langchain.DocumentLoader.WebPage
import Langchain.Embeddings.Core
import Langchain.Graph.Checkpointer
import Langchain.Graph.HITL
import Langchain.Graph.MultiAgent
import Langchain.Graph.Parallel
import Langchain.Graph.StateGraph
import Langchain.Guardrail.Core
import Langchain.MCP.Client
import Langchain.Memory.Core
import Langchain.Memory.Entity
import Langchain.Memory.Summary
import Langchain.Observability
import Langchain.OutputParser.Core
import Langchain.OutputParser.Structured
import Langchain.PromptTemplate.FewShot
import Langchain.PromptTemplate.Prompt
import Langchain.Provider.Gemini (Gemini, newGemini)
import Langchain.Provider.Ollama
  ( ModelOptions (..)
  , Ollama (..)
  , OllamaClientConfig (..)
  , chatRequestFor
  , defaultConfig
  , defaultOptions
  , newOllama
  , newOllamaWithClient
  , resolveChatRequest
  , toOllamaTool
  , toOllamaTools
  , withJsonFormat
  , withOptions
  , withSchemaFormat
  , withStructuredOutput
  , withTools
  )
import Langchain.Provider.OpenAI (OpenAI, newOpenAI)
import Langchain.Resilience.CircuitBreaker
import Langchain.Resilience.Retry
import Langchain.Retriever.BM25
import Langchain.Retriever.Core
import Langchain.Retriever.Hybrid
import Langchain.TextSplitter.Character
import Langchain.TextSplitter.Code
import Langchain.TextSplitter.Markdown
import Langchain.TextSplitter.RecursiveCharacter
import Langchain.TextSplitter.Token
import Langchain.Tool.Async
import Langchain.Tool.GenericSchema
import Langchain.VectorStore.Core
import Langchain.VectorStore.InMemory
import Langchain.VectorStore.SqliteVec

-- | Default runtime configuration alias
defaultLangchainConfig :: LangchainConfig
defaultLangchainConfig = CoreMonad.defaultConfig

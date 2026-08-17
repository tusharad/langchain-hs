{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , LangchainConfig (..)
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
  , MockModel (..)
  , newMockModel
  , Message (..)
  , Role
  , ContentBlock (..)
  , ToolCall (..)
  , textMessage
  , userMessage
  , systemMessage
  , assistantMessage
  , imageMessage
  , extractMessageText

    -- * Streaming Events
  , StreamEvent (..)
  , TokenUsage (..)
  , EventStream
  , collectEvents
  , printEvents

    -- * Pure AST Pipelines (RunnableTree) & Pipeline DSL
  , RunnableTree (..)
  , (|>>)
  , (&>&)
  , interpret
  , runLambda
  , runPrim
  , pipe
  , (>>>#)
  , pipeParallel
  , PipelineStep (..)
  , mkStep
  , runPipeline

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
  , CompiledGraph (..)
  , Node (..)
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
  , toDot
  , StateSnapshot (..)
  , TimeTravelHistory (..)
  , newTimeTravelHistory
  , recordSnapshot
  , getSnapshots
  , resumeFromSnapshot
  , parallelNode
  , addParallelNodes
  , SubGraphOptions (..)
  , defaultSubGraphOptions
  , embedSubGraphWithOptions

    -- * Advanced Agent Patterns
  , PlanAndExecuteAgent (..)
  , newPlanAndExecuteAgent
  , runPlanAndExecute
  , FunctionsAgent (..)
  , newFunctionsAgent
  , runFunctionsAgent
  , SpecialistAgent (..)
  , SupervisorTeam (..)
  , newSupervisorTeam
  , runSupervisorTeam
  , Debater (..)
  , DebateConfig (..)
  , defaultDebateConfig
  , runDebate
  , VotingClassifier (..)
  , newVotingClassifier
  , runVotingClassification
  , Blackboard (..)
  , KnowledgeSource (..)
  , BlackboardConfig (..)
  , newBlackboard
  , runBlackboard

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
  , ActionType (..)
  , TraceStep (..)
  , AgentTrace (..)
  , Tracer (..)
  , newTracer
  , recordStep
  , getTrace
  , findSlowestStep
  , filterByActionType
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
  , withSpan
  , exportSpansJson

    -- * Callbacks & Diagnostics
  , CallbackEvent (..)
  , CallbackHandler (..)
  , CallbackManager (..)
  , newCallbackManager
  , registerHandler
  , dispatchEvent
  , dispatchEventAsync
  , newLoggingCallbackHandler
  , HealthStatus (..)
  , ComponentHealth (..)
  , HealthReport (..)
  , checkOllamaHealth
  , checkSqliteHealth
  , runFullHealthCheck

    -- * Config Validation & Cost Accounting
  , ConfigIssue (..)
  , ValidationResult (..)
  , validateLangchainConfig
  , assertValidConfig
  , ModelPricing (..)
  , CostEstimate (..)
  , estimateTokenCount
  , getStandardPricing
  , calculateCost

    -- * HTTP Pooling & Resilience
  , ConnectionPoolConfig (..)
  , defaultPoolConfig
  , PooledHttpManager (..)
  , newPooledHttpManager
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
  , PgVectorStore (..)
  , defaultPgVectorStore
  , QdrantStore (..)
  , defaultQdrantStore
  , Retriever (..)
  , VectorStoreRetriever (..)
  , MultiQueryRetriever (..)
  , newMultiQueryRetriever
  , ContextualCompressionRetriever (..)
  , newContextualCompressionRetriever
  , ParentDocumentRetriever (..)
  , newParentDocumentRetriever
  , addParentDocuments

    -- * Embeddings
  , Embeddings (..)

    -- * Document Loaders & Transformers
  , Document (..)
  , BaseLoader (..)
  , FileLoader (..)
  , DirectoryLoader (..)
  , DirectoryLoaderOptions (..)
  , defaultDirectoryLoaderOptions
  , PdfLoader (..)
  , CsvLoader (..)
  , defaultCsvLoader
  , JsonLoader (..)
  , defaultJsonLoader
  , HtmlLoader (..)
  , defaultHtmlLoader
  , WebPageLoader (..)
  , defaultWebPageLoader
  , DocumentTransformer (..)
  , enrichDocumentMetadata
  , enrichDocuments
  , MetadataEnricher (..)
  , newMetadataEnricher

    -- * Prompt Templates & Example Selectors
  , PromptTemplate (..)
  , FewShotPromptTemplate (..)
  , renderPrompt
  , renderFewShotPrompt
  , Example
  , ExampleSelector (..)
  , LengthBasedSelector (..)
  , newLengthBasedSelector
  , selectByLength

    -- * Text Splitters
  , CharacterSplitterOps (..)
  , defaultCharacterSplitterOps
  , splitText
  , RecursiveCharacterSplitterOps (..)
  , defaultRecursiveCharacterSplitterOps
  , splitTextRecursive
  , MarkdownSplitterOps (..)
  , defaultMarkdownSplitterOps
  , splitMarkdown
  , splitMarkdownToChunks
  , TokenSplitterOps (..)
  , defaultTokenSplitterOps
  , splitByTokens
  , Language (..)
  , CodeSplitterOps (..)
  , splitCode

    -- * Caching & Resilience
  , CacheBackend (..)
  , InMemoryCache (..)
  , newInMemoryCache
  , SQLiteCache (..)
  , newSQLiteCache
  , CachedModel (..)
  , withCaching
  , RetryPolicy (..)
  , defaultRetryPolicy
  , withRetry
  , RateLimiter (..)
  , newRateLimiter
  , withRateLimit

    -- * Chains
  , RetrievalQA (..)
  , newRetrievalQA
  , runRetrievalQA
  , SequentialChain (..)
  , ChainStep (..)
  , newSequentialChain
  , runSequentialChain
  , ConversationalChain (..)
  , newConversationalChain
  , runConversationalChain
  , StuffDocumentsChain (..)
  , newStuffDocumentsChain
  , runStuffDocumentsChain
  , MapReduceChain (..)
  , newMapReduceChain
  , runMapReduceChain
  , SqlDatabaseChain (..)
  , newSqlDatabaseChain
  , runSqlDatabaseChain
  , ConversationalRetrievalQA (..)
  , newConversationalRetrievalQA
  , runConversationalRetrievalQA
  , SummarizationStrategy (..)
  , SummarizationChain (..)
  , newSummarizationChain
  , runSummarizationChain

    -- * Structured Output, Routers, and Advanced Parsers
  , StructuredOutput (..)
  , structuredInvoke
  , SemanticRouter (..)
  , newSemanticRouter
  , Route (..)
  , routeQuery
  , XmlOutputParser (..)
  , newXmlOutputParser
  , parseXmlOutput
  , EnumParser (..)
  , newEnumParser
  , parseEnum

    -- * Algebraic Laws Verification
  , verifyAllLaws

    -- * Agents & Execution
  , ReActAgent (..)
  , AgentStep (..)
  , createReActAgent
  , reactStep
  , runReActAgent
  , AgentMiddleware (..)
  , defaultMiddleware
  , chainMiddlewares
  , loggingMiddleware

    -- * Providers
  , Ollama
  , newOllama
  , OpenAI
  , newOpenAI
  , Anthropic
  , newAnthropic
  , Gemini
  , newGemini
  , DeepSeek
  , newDeepSeek
  ) where

import Langchain.Accounting.Cost
import Langchain.Agent.Core
import Langchain.Agent.Functions
import Langchain.Agent.Middleware
import Langchain.Agent.PlanAndExecute
import Langchain.Agent.Supervisor
import Langchain.Cache.Core
import Langchain.Callback.Manager
import Langchain.Chain.Conversational
import Langchain.Chain.ConversationalRetrievalQA
import Langchain.Chain.MapReduce
import Langchain.Chain.RetrievalQA
import Langchain.Chain.Sequential
import Langchain.Chain.SqlDatabase
import Langchain.Chain.StuffDocuments
import Langchain.Chain.Summarization
import Langchain.Config.Validation
import Langchain.Core.Error
import Langchain.Core.Model
import qualified Langchain.Core.Monad as CoreMonad
import Langchain.Core.Monad
import Langchain.Core.Runnable
import Langchain.Core.Stream
import Langchain.Core.Tool
import Langchain.Diagnostics.HealthCheck
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.Csv
import Langchain.DocumentLoader.DirectoryLoader
import Langchain.DocumentLoader.FileLoader
import Langchain.DocumentLoader.Html
import Langchain.DocumentLoader.Json
import Langchain.DocumentLoader.PdfLoader
import Langchain.DocumentLoader.WebPage
import Langchain.DocumentTransformer.MetadataEnricher
import Langchain.Embeddings.Core
import Langchain.Error (LangchainResult)
import Langchain.ExampleSelector.Similarity
import Langchain.Graph.Blackboard
import Langchain.Graph.Checkpointer
import Langchain.Graph.Debate
import Langchain.Graph.HITL
import Langchain.Graph.MultiAgent
import Langchain.Graph.Parallel
import Langchain.Graph.StateGraph
import Langchain.Graph.SubGraph
import Langchain.Graph.TimeTravel
import Langchain.Graph.Visualization
import Langchain.Graph.Voting
import Langchain.Guardrail.Core
import Langchain.HTTP.ConnectionPool
import Langchain.Laws
import Langchain.Logging.Structured
import Langchain.MCP.Client
import Langchain.Memory.Core
import Langchain.Memory.Entity
import Langchain.Memory.Summary
import Langchain.Observability.OpenTelemetry
import Langchain.OutputParser.Enum
import Langchain.OutputParser.Structured
import Langchain.OutputParser.Xml
import Langchain.Pipeline.DSL
import Langchain.PromptTemplate
import Langchain.Provider.Anthropic (Anthropic, newAnthropic)
import Langchain.Provider.DeepSeek (DeepSeek, newDeepSeek)
import Langchain.Provider.Gemini (Gemini, newGemini)
import Langchain.Provider.Ollama (Ollama, newOllama)
import Langchain.Provider.OpenAI (OpenAI, newOpenAI)
import Langchain.Resilience.CircuitBreaker
import Langchain.Resilience.Retry
import Langchain.Retriever.ContextualCompression
import Langchain.Retriever.Core
import Langchain.Retriever.MultiQueryRetriever
import Langchain.Retriever.ParentDocument
import Langchain.Router.Semantic
import Langchain.TextSplitter.Character
import Langchain.TextSplitter.Code
import Langchain.TextSplitter.Markdown
import Langchain.TextSplitter.RecursiveCharacter
import Langchain.TextSplitter.Token
import Langchain.Tool.Async
import Langchain.Tool.GenericSchema
import Langchain.Trace.Core
import Langchain.VectorStore.Core
import Langchain.VectorStore.InMemory
import Langchain.VectorStore.PgVector
import Langchain.VectorStore.Qdrant
import Langchain.VectorStore.SqliteVec

-- | Default runtime configuration alias
defaultLangchainConfig :: LangchainConfig
defaultLangchainConfig = CoreMonad.defaultConfig

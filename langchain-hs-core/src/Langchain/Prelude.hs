{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Prelude
Description : Canonical umbrella re-export module for langchain-hs-core
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Exports all core data types, typeclasses, vector stores, memory stores,
advanced agents, guardrails, observability, structured logging,
circuit breakers, callbacks, output parsers, and pipeline DSLs.
-}
module Langchain.Prelude
  ( -- * Core Monad & Errors
    LangchainT
  , runLangchainT
  , throwLangchainError
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
  , runPure
  , runPassthrough
  , runIdent
  , runBranch
  , runFallback
  , runChat
  , runModel
  , runRetriever
  , ModelRunnable (..)
  , TextModelRunnable (..)

    -- * Effect-Polymorphic Tools
  , Tool (..)
  , createTool
  , toolToValue
  , ToolBinder (..)
  , DeriveToolSchema (..)
  , deriveToolParametersSchema

    -- * Advanced Agent Patterns
  , PlanStep (..)
  , Plan (..)
  , StepExecutor (..)
  , PlanAndExecuteAgent (..)
  , newPlanAndExecuteAgent
  , newPlanAndExecuteAgentWithTools
  , runPlanAndExecute

    -- * Guardrails & Safety
  , GuardrailResult (..)
  , Guardrail (..)
  , contentSafetyGuardrail
  , topicGuardrail
  , outputLengthGuardrail
  , composeGuardrails
  , withGuardrails

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
  , getCallbackLogs

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
  , SomeMemory (..)
  , WindowBufferMemory (..)
  , newWindowBufferMemory
  , TokenBufferMemory (..)
  , newTokenBufferMemory
  , countTokens
  , SummaryMemory (..)
  , newSummaryMemory
  , initialMessages
  , trimMessages

    -- * Vector Stores & Retrieval
  , VectorStore (..)
  , InMemory (..)
  , emptyInMemoryVectorStore
  , fromDocuments
  , Retriever (..)
  , VectorStoreRetriever (..)
  , retrieveWithCallbacks

    -- * Embeddings (provider-agnostic typeclass)
  , Embeddings (..)

    -- * Document Loaders
  , Document (..)
  , BaseLoader (..)
  , FileLoader (..)
  , DirectoryLoader (..)
  , DirectoryLoaderOptions (..)
  , defaultDirectoryLoaderOptions

    -- * Prompt Templates
  , PromptTemplate (..)
  , PromptTemplateOptions (..)
  , TemplateFormat (..)
  , defaultPromptTemplateOptions
  , fromTemplate
  , fromTemplateWithOptions
  , fromTemplateWithFormat
  , partialPromptTemplate
  , renderPrompt

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
  , CacheableChatModel (..)
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

    -- * Structured Output & Parsers
  , OutputParser (..)
  , CommaSeparatedList (..)
  , JSONOutputStructure (..)
  , NumberSeparatedList (..)
  , StructuredOutput (..)
  , TypeSchema (..)
  , structuredInvoke
  , structuredInvokeWithRetries

    -- * Agents & Execution
  , ReActAgent (ReActAgent)
  , AgentStep (..)
  , AgentTrace (..)
  , ToolErrorStrategy (..)
  , defaultReActAgent
  , withMemory
  , withoutMemory
  , withSystemPrompt
  , withMaxIterations
  , withModelConfig
  , withCallbackManager
  , withStopCondition
  , withToolErrorStrategy
  , reactStep
  , runReActAgent
  , runReActAgentWithTrace

    -- * Tool Suite & Security
  , ToolTier (..)
  , SecurityPolicy (..)
  , defaultSecurityPolicy
  , readOnlySecurityPolicy
  , workspaceSandboxPolicy
  , withSecurity
  , newFileSystemSuite
  , findFilesTool
  , grepSearchTool
  , readFileTool
  , listDirTool
  , editFileTool
  , writeFileTool
  , newProcessSuite
  , commandRunTool
  , commandStatusTool
  , commandKillTool
  , shellTool

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
  ) where

import Langchain.Agent.PlanAndExecute
import Langchain.Agent.ReAct
import Langchain.Cache.Core
import Langchain.Callback.Manager
import Langchain.Chain.RetrievalQA
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.DirectoryLoader
import Langchain.DocumentLoader.FileLoader
import Langchain.Embeddings.Core
import Langchain.Error
import Langchain.Guardrail.Core
import Langchain.Memory.Core
import Langchain.Memory.Summary
import Langchain.Model
import Langchain.Monad
import Langchain.Observability
import Langchain.OutputParser.Core
import Langchain.OutputParser.Structured
import Langchain.PromptTemplate.Prompt
import Langchain.Resilience.CircuitBreaker
import Langchain.Resilience.Retry
import Langchain.Retriever.BM25
import Langchain.Retriever.Core
import Langchain.Retriever.Hybrid
import Langchain.Runnable hiding (invoke)
import Langchain.Stream
import Langchain.TextSplitter.Character
import Langchain.TextSplitter.Code
import Langchain.TextSplitter.Markdown
import Langchain.TextSplitter.RecursiveCharacter
import Langchain.TextSplitter.Token
import Langchain.Tool
import Langchain.Tool.Binding
import Langchain.Tool.FileSystem
import Langchain.Tool.GenericSchema
import Langchain.Tool.Process
import Langchain.Tool.Security
import Langchain.Tool.Shell (shellTool)
import Langchain.VectorStore.Core
import Langchain.VectorStore.InMemory

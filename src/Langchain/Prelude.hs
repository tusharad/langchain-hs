{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Prelude
Description : Canonical batteries-included prelude for langchain-hs
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Re-exports all essential types, typeclasses, and combinators for building LLM applications,
agent graphs, and semantic pipelines in Haskell.
-}
module Langchain.Prelude
  ( -- * Core Errors
    LangchainError (..)
  , LangchainResult
  , errorMessage
  , llmError
  , agentError
  , memoryError
  , toolError
  , vectorStoreError
  , documentLoaderError
  , embeddingError
  , runnableError
  , parsingError
  , validationError
  , internalError

    -- * Monad Transformer Stack
  , LangchainT
  , LangchainConfig (..)
  , defaultLangchainConfig
  , runLangchainT
  , runLangchainTIO
  , askConfig
  , withConfig
  , throwLangchainError

    -- * Multi-Modal Messages & Models
  , ChatModel (..)
  , MockModel (..)
  , newMockModel
  , Message (..)
  , Role (System, User, Assistant, Developer, Function)
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

    -- * Document Loaders
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

    -- * Prompt Templates
  , PromptTemplate (..)
  , FewShotPromptTemplate (..)
  , renderPrompt
  , renderFewShotPrompt

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
  , TokenSplitterOps (..)
  , defaultTokenSplitterOps
  , splitByTokens
  , CodeSplitterOps (..)
  , defaultCodeSplitterOps
  , Language (..)
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

import Langchain.Agent.Core
import Langchain.Agent.Middleware
import Langchain.Cache.Core
import Langchain.Chain.Conversational
import Langchain.Chain.MapReduce
import Langchain.Chain.RetrievalQA
import Langchain.Chain.Sequential
import Langchain.Chain.StuffDocuments
import Langchain.Core.Error
import Langchain.Core.Model
import qualified Langchain.Core.Monad as CoreMonad
import Langchain.Core.Monad
import Langchain.Core.Runnable
import Langchain.Core.Stream
import Langchain.Core.Tool
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.Csv
import Langchain.DocumentLoader.DirectoryLoader
import Langchain.DocumentLoader.FileLoader
import Langchain.DocumentLoader.Html
import Langchain.DocumentLoader.Json
import Langchain.DocumentLoader.PdfLoader
import Langchain.DocumentLoader.WebPage
import Langchain.Embeddings.Core
import Langchain.Error (LangchainResult)
import Langchain.Graph.Checkpointer
import Langchain.Graph.HITL
import Langchain.Graph.MultiAgent
import Langchain.Graph.StateGraph
import Langchain.Memory.Core
import Langchain.Memory.Entity
import Langchain.Memory.Summary
import Langchain.OutputParser.Enum
import Langchain.OutputParser.Structured
import Langchain.OutputParser.Xml
import Langchain.PromptTemplate
import Langchain.Provider.Anthropic (Anthropic, newAnthropic)
import Langchain.Provider.DeepSeek (DeepSeek, newDeepSeek)
import Langchain.Provider.Gemini (Gemini, newGemini)
import Langchain.Provider.Ollama (Ollama, newOllama)
import Langchain.Provider.OpenAI (OpenAI, newOpenAI)
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
import Langchain.VectorStore.Core
import Langchain.VectorStore.InMemory
import Langchain.VectorStore.PgVector
import Langchain.VectorStore.Qdrant
import Langchain.VectorStore.SqliteVec

-- | Default runtime configuration alias
defaultLangchainConfig :: LangchainConfig
defaultLangchainConfig = CoreMonad.defaultConfig

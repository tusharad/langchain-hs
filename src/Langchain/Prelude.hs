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

    -- * Memory
  , BaseMemory (..)
  , WindowBufferMemory (..)
  , newWindowBufferMemory
  , initialMessages
  , trimMessages

    -- * Vector Stores & Retrieval
  , VectorStore (..)
  , InMemory (..)
  , emptyInMemoryVectorStore
  , fromDocuments
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
  , PdfLoader (..)

    -- * Prompt Templates
  , PromptTemplate (..)
  , FewShotPromptTemplate (..)
  , renderPrompt
  , renderFewShotPrompt

    -- * Text Splitters
  , CharacterSplitterOps (..)
  , defaultCharacterSplitterOps
  , splitText

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
import qualified Langchain.Core.Error as CoreErr
import Langchain.Core.Error
  ( ErrorContext (..)
  , LangchainError (..)
  , agentError
  , configurationError
  , documentLoaderError
  , embeddingError
  , errorMessage
  , internalError
  , llmError
  , memoryError
  , networkError
  , parsingError
  , runnableError
  , toolError
  , validationError
  , vectorStoreError
  )
import Langchain.Core.Model
import qualified Langchain.Core.Monad as CoreMonad
import Langchain.Core.Monad
  ( LangchainConfig (..)
  , LangchainT
  , askConfig
  , runLangchainT
  , runLangchainTIO
  , throwLangchainError
  , withConfig
  )
import Langchain.Core.Runnable
import Langchain.Core.Stream
import Langchain.Core.Tool
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.DirectoryLoader
import Langchain.DocumentLoader.FileLoader
import Langchain.DocumentLoader.PdfLoader
import Langchain.Embeddings.Core
import Langchain.Error (LangchainResult)
import Langchain.Graph.Checkpointer
import Langchain.Graph.HITL
import Langchain.Graph.MultiAgent
import Langchain.Graph.StateGraph
import Langchain.Memory.Core
import Langchain.PromptTemplate
import Langchain.Provider.Anthropic (Anthropic, newAnthropic)
import Langchain.Provider.DeepSeek (DeepSeek, newDeepSeek)
import Langchain.Provider.Gemini (Gemini, newGemini)
import Langchain.Provider.Ollama (Ollama, newOllama)
import Langchain.Provider.OpenAI (OpenAI, newOpenAI)
import Langchain.Retriever.Core
import Langchain.TextSplitter.Character
import Langchain.VectorStore.Core
import Langchain.VectorStore.InMemory

-- | Default runtime configuration alias
defaultLangchainConfig :: LangchainConfig
defaultLangchainConfig = CoreMonad.defaultConfig

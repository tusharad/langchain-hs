---
title: Stateful Graphs (StateGraph)
description: Cyclic workflows, typed channels, state reducers, and checkpointed state machines.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Graph</span> <span class="badge badge-primary">Langchain.Graph.State</span>
</div>

Cyclic workflows, typed channels, state reducers, and checkpointed state machines.

## Key Concepts

- **Cyclic Execution**: Construct complex agentic state machines with arbitrary cycles and conditional routing.
- **State Reducers**: Define pure reducer functions (`s -> s -> s`) for managing append-only lists or state updates.
- **Compiled Graph**: Compile graphs into deterministic executable runnables with static validation.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run stategraphollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

{- |
  StateGraph example: a 3-node document Q&A pipeline.

  Graph topology:

    ingest ──► retrieve ──► answer ──┬──► refine ──► __end__
                                     └──────────────► __end__

  * ingest   – loads a file, splits it into chunks, stores them in the state
  * retrieve – BM25-searches the chunks for the user question
  * answer   – invokes Ollama with context; if the model signals uncertainty
               the conditional edge routes to a "refine" node that re-prompts
               with a more explicit instruction; otherwise goes to __end__
  * refine   – re-asks with a more directive prompt as a fallback

  The shared state is 'PipelineState', reduced by 'replaceFieldReducer' so
  every node simply replaces the whole state on each step.

  Node actions live in 'ExceptT LangchainError IO', the canonical concrete
  type that satisfies the 'MonadIO + MonadError LangchainError' constraints
  required by 'runGraph'.
-}
module Ollama.StateGraph (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , systemMessage
  , userMessage
  )
import Langchain.DocumentLoader.Core (BaseLoader (..), Document (..))
import Langchain.DocumentLoader.FileLoader (FileLoader (..))
import Langchain.Graph.StateGraph
import Langchain.Provider.Ollama (defaultConfig, newOllama)
import Langchain.Retriever.BM25 (bm25Search, newBM25Index)
import Langchain.TextSplitter.Character
  ( CharacterSplitterOps (..)
  , defaultCharacterSplitterOps
  , splitText
  )

-- ---------------------------------------------------------------------------
-- Shared pipeline state
-- ---------------------------------------------------------------------------

data PipelineState = PipelineState
  { question :: T.Text
  , chunks :: [Document]
  , context :: T.Text
  , answer :: T.Text
  }

initialState :: T.Text -> PipelineState
initialState q =
  PipelineState
    { question = q
    , chunks = []
    , context = ""
    , answer = ""
    }

-- Convenience alias for the concrete monad stack used by all nodes.
type App = ExceptT LangchainError IO

-- ---------------------------------------------------------------------------
-- Node actions — all in App = ExceptT LangchainError IO
-- ---------------------------------------------------------------------------

-- | Load and chunk a file into Documents.
ingestNode :: FilePath -> PipelineState -> App PipelineState
ingestNode filePath state = do
  docs <- load (FileLoader filePath)
  let ops = defaultCharacterSplitterOps {chunkSize = 600, separator = "\n\n"}
      mkDoc t = Document t Map.empty
      splitDocs =
        concatMap
          (map mkDoc . splitText ops . pageContent)
          docs
  liftIO $ T.putStrLn $ "[ingest] loaded " <> T.pack (show (length splitDocs)) <> " chunks"
  pure state {chunks = splitDocs}

-- | BM25-retrieve the top-3 relevant chunks for the question.
retrieveNode :: PipelineState -> App PipelineState
retrieveNode state = do
  let idx = newBM25Index (chunks state)
      topDocs = bm25Search idx (question state) 3
      ctx = T.intercalate "\n\n---\n\n" (map (TL.toStrict . pageContent) topDocs)
  liftIO $ T.putStrLn $ "[retrieve] using " <> T.pack (show (length topDocs)) <> " context chunks"
  pure state {context = ctx}

-- | Ask Ollama, storing the answer in the state.
answerNode :: T.Text -> PipelineState -> App PipelineState
answerNode model state = do
  o <- liftIO $ newOllama model defaultConfig
  let prompt =
        T.unlines
          [ "Answer the question using ONLY the context below."
          , "Context:"
          , context state
          , ""
          , "Question: " <> question state
          ]
      msgs = [systemMessage "You are a concise technical assistant.", userMessage prompt]
  resp <- invoke o msgs Nothing
  let ans = extractMessageText resp
  liftIO $ T.putStrLn $ "[answer] " <> T.take 120 ans
  pure state {answer = ans}

-- | Fallback: re-prompt with a more explicit instruction.
refineNode :: T.Text -> PipelineState -> App PipelineState
refineNode model state = do
  o <- liftIO $ newOllama model defaultConfig
  let prompt =
        T.unlines
          [ "The previous attempt did not produce a clear answer."
          , "Context:"
          , context state
          , ""
          , "Please answer this question as specifically as possible: " <> question state
          ]
      msgs = [systemMessage "You are a precise technical assistant.", userMessage prompt]
  resp <- invoke o msgs Nothing
  let ans = extractMessageText resp
  liftIO $ T.putStrLn $ "[refine] " <> T.take 120 ans
  pure state {answer = ans}

-- | Conditional router: uncertain answers go to "refine", otherwise __end__.
answerRouter :: PipelineState -> App NodeId
answerRouter state = do
  let ans = T.toLower (answer state)
      uncertain =
        any
          (`T.isInfixOf` ans)
          ["i don't know", "i do not know", "not sure", "unclear", "cannot determine"]
  pure $ if uncertain then "refine" else endNodeId

-- ---------------------------------------------------------------------------
-- Build and run the graph
-- ---------------------------------------------------------------------------

runApp :: IO ()
runApp = do
  let model = "gemma3"
      filePath = "README.md"
      userQ = "What are the main features of langchain-hs?"

  T.putStrLn "Building StateGraph pipeline..."

  -- Node actions have type: PipelineState -> App (Either LangchainError PipelineState)
  -- We wrap each App action with 'fmap Right' since App already carries errors via ExceptT.
  let liftNode f s = fmap Right (f s)
      graph =
        addEdge "refine" endNodeId
          . addConditionalEdge "answer" (fmap Right . answerRouter)
          . addEdge "retrieve" "answer"
          . addEdge "ingest" "retrieve"
          . addNode "refine" (liftNode (refineNode model))
          . addNode "answer" (liftNode (answerNode model))
          . addNode "retrieve" (liftNode retrieveNode)
          . addNode "ingest" (liftNode (ingestNode filePath))
          $ emptyStateGraph replaceFieldReducer

  case compileGraph graph of
    Left err -> T.putStrLn $ "Graph compilation failed: " <> T.pack (show err)
    Right compiled -> do
      res <- runExceptT $ runGraph compiled "ingest" (initialState userQ)
      case res of
        Left err -> T.putStrLn $ "Pipeline error: " <> T.pack (show err)
        Right finalState -> do
          T.putStrLn "\n=== Final Answer ==="
          T.putStrLn (answer finalState)
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run stategraphopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

{- |
  StateGraph example: a 3-node document Q&A pipeline with OpenAI / OpenRouter.

  Graph topology:

    ingest ──► retrieve ──► answer ──┬──► refine ──► __end__
                                     └──────────────► __end__

  * ingest   – loads a file, splits it into chunks, stores them in the state
  * retrieve – BM25-searches the chunks for the user question
  * answer   – invokes OpenAI with context; if the model signals uncertainty
               the conditional edge routes to a "refine" node that re-prompts
               with a more explicit instruction; otherwise goes to __end__
  * refine   – re-asks with a more directive prompt as a fallback

  The shared state is 'PipelineState', reduced by 'replaceFieldReducer' so
  every node simply replaces the whole state on each step.

  Node actions live in 'ExceptT LangchainError IO', the canonical concrete
  type that satisfies the 'MonadIO + MonadError LangchainError' constraints
  required by 'runGraph'.
-}
module OpenAI.StateGraph (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , systemMessage
  , userMessage
  )
import Langchain.DocumentLoader.Core (BaseLoader (..), Document (..))
import Langchain.DocumentLoader.FileLoader (FileLoader (..))
import Langchain.Graph.StateGraph
import Langchain.Retriever.BM25 (bm25Search, newBM25Index)
import Langchain.TextSplitter.Character
  ( CharacterSplitterOps (..)
  , defaultCharacterSplitterOps
  , splitText
  )
import OpenAI.Common (defaultModelName, getOpenRouterModel)

-- ---------------------------------------------------------------------------
-- Shared pipeline state
-- ---------------------------------------------------------------------------

data PipelineState = PipelineState
  { question :: T.Text
  , chunks :: [Document]
  , context :: T.Text
  , answer :: T.Text
  }

initialState :: T.Text -> PipelineState
initialState q =
  PipelineState
    { question = q
    , chunks = []
    , context = ""
    , answer = ""
    }

-- Convenience alias for the concrete monad stack used by all nodes.
type App = ExceptT LangchainError IO

-- ---------------------------------------------------------------------------
-- Node actions — all in App = ExceptT LangchainError IO
-- ---------------------------------------------------------------------------

-- | Load and chunk a file into Documents.
ingestNode :: FilePath -> PipelineState -> App PipelineState
ingestNode filePath state = do
  docs <- load (FileLoader filePath)
  let ops = defaultCharacterSplitterOps {chunkSize = 600, separator = "\n\n"}
      mkDoc t = Document t Map.empty
      splitDocs =
        concatMap
          (map mkDoc . splitText ops . pageContent)
          docs
  liftIO $ T.putStrLn $ "[ingest] loaded " <> T.pack (show (length splitDocs)) <> " chunks"
  pure state {chunks = splitDocs}

-- | BM25-retrieve the top-3 relevant chunks for the question.
retrieveNode :: PipelineState -> App PipelineState
retrieveNode state = do
  let idx = newBM25Index (chunks state)
      topDocs = bm25Search idx (question state) 3
      ctx = T.intercalate "\n\n---\n\n" (map (TL.toStrict . pageContent) topDocs)
  liftIO $ T.putStrLn $ "[retrieve] using " <> T.pack (show (length topDocs)) <> " context chunks"
  pure state {context = ctx}

-- | Ask OpenAI, storing the answer in the state.
answerNode :: T.Text -> PipelineState -> App PipelineState
answerNode model state = do
  o <- liftIO $ getOpenRouterModel model
  let prompt =
        T.unlines
          [ "Answer the question using ONLY the context below."
          , "Context:"
          , context state
          , ""
          , "Question: " <> question state
          ]
      msgs = [systemMessage "You are a concise technical assistant.", userMessage prompt]
  resp <- invoke o msgs Nothing
  let ans = extractMessageText resp
  liftIO $ T.putStrLn $ "[answer] " <> T.take 120 ans
  pure state {answer = ans}

-- | Fallback: re-prompt with a more explicit instruction.
refineNode :: T.Text -> PipelineState -> App PipelineState
refineNode model state = do
  o <- liftIO $ getOpenRouterModel model
  let prompt =
        T.unlines
          [ "The previous attempt did not produce a clear answer."
          , "Context:"
          , context state
          , ""
          , "Please answer this question as specifically as possible: " <> question state
          ]
      msgs = [systemMessage "You are a precise technical assistant.", userMessage prompt]
  resp <- invoke o msgs Nothing
  let ans = extractMessageText resp
  liftIO $ T.putStrLn $ "[refine] " <> T.take 120 ans
  pure state {answer = ans}

-- | Conditional router: uncertain answers go to "refine", otherwise __end__.
answerRouter :: PipelineState -> App NodeId
answerRouter state = do
  let ans = T.toLower (answer state)
      uncertain =
        any
          (`T.isInfixOf` ans)
          ["i don't know", "i do not know", "not sure", "unclear", "cannot determine"]
  pure $ if uncertain then "refine" else endNodeId

-- ---------------------------------------------------------------------------
-- Build and run the graph
-- ---------------------------------------------------------------------------

runApp :: IO ()
runApp = do
  let model = defaultModelName
      filePath = "README.md"
      userQ = "What are the main features of langchain-hs?"

  T.putStrLn "Building StateGraph pipeline..."

  let liftNode f s = fmap Right (f s)
      graph =
        addEdge "refine" endNodeId
          . addConditionalEdge "answer" (fmap Right . answerRouter)
          . addEdge "retrieve" "answer"
          . addEdge "ingest" "retrieve"
          . addNode "refine" (liftNode (refineNode model))
          . addNode "answer" (liftNode (answerNode model))
          . addNode "retrieve" (liftNode retrieveNode)
          . addNode "ingest" (liftNode (ingestNode filePath))
          $ emptyStateGraph replaceFieldReducer

  case compileGraph graph of
    Left err -> T.putStrLn $ "Graph compilation failed: " <> T.pack (show err)
    Right compiled -> do
      res <- runExceptT $ runGraph compiled "ingest" (initialState userQ)
      case res of
        Left err -> T.putStrLn $ "Pipeline error: " <> T.pack (show err)
        Right finalState -> do
          T.putStrLn "\n=== Final Answer ==="
          T.putStrLn (answer finalState)
```
:::
:::

## Core Types & Functions

```haskell
Graph builder with typed state s
```
```haskell
NodeId -> (s -> m s) -> StateGraph s m ()
```
```haskell
NodeId -> NodeId -> StateGraph s m ()
```
```haskell
NodeId -> (s -> m NodeId) -> StateGraph s m ()
```
```haskell
StateGraph s m () -> CompiledGraph s m
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run stategraphollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run stategraphopenai
```

---
title: Human-in-the-Loop (HITL)
description: Interrupting workflows before critical actions, waiting for human approval, and resuming.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Graph</span> <span class="badge badge-primary">Langchain.Graph.Checkpointer</span>
</div>

Interrupting workflows before critical actions, waiting for human approval, and resuming.

## Key Concepts

- **Interruption Points**: Designate graph nodes that require manual operator verification before state progression.
- **State Checkpointing**: Serialize agent state to memory or disk to survive process restarts while waiting for human feedback.
- **Resume & Time Travel**: Rehydrate the graph from any prior checkpoint with updated human instructions.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run hitlollama"}
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  Human-In-The-Loop (HITL) and State Checkpointing example using langchain-hs-graph.

  Workflow:

    generateDraft ──► humanReview [INTERRUPT] ──(resume)──► publishPost ──► __end__

  1. The LLM generates a draft response.
  2. The 'hitlNode' automatically snapshots the state to a persistent or in-memory
     Checkpointer (TVar or SQLite) and halts graph execution with a 'HITL_INTERRUPT' signal.
  3. The application catches the interrupt via 'isHITLInterrupt', renders the draft
     for human evaluation or modification.
  4. The human provides feedback/edits, and 'resumeGraph' reloads the checkpointed
     state, applies the human changes, and continues the graph to completion.
-}
module Ollama.HITL (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (extractMessageText, systemMessage, userMessage)
import Langchain.Graph.Checkpointer
  ( MemoryCheckpointer
  , newMemoryCheckpointer
  )
import Langchain.Graph.HITL
  ( hitlNode
  , isHITLInterrupt
  , resumeGraph
  )
import Langchain.Graph.StateGraph
  ( Node (..)
  , addEdge
  , addNode
  , compileGraph
  , emptyStateGraph
  , endNodeId
  , replaceFieldReducer
  , runGraph
  )
import Langchain.Prelude (invoke)
import Langchain.Provider.Ollama (defaultConfig, newOllama)

-- ---------------------------------------------------------------------------
-- Shared blog post draft state
-- ---------------------------------------------------------------------------

data BlogPostState = BlogPostState
  { topic :: T.Text
  , draftContent :: T.Text
  , humanFeedback :: T.Text
  , isPublished :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type App = ExceptT LangchainError IO

runApp :: IO ()
runApp = do
  let modelName = "gemma3"
      threadId = "session-hitl-001"
  o <- newOllama modelName defaultConfig
  cp <- newMemoryCheckpointer

  T.putStrLn "=== Initializing Human-in-the-Loop (HITL) Pipeline ==="

  -- -------------------------------------------------------------------------
  -- 1. Node actions
  -- -------------------------------------------------------------------------

  -- Draft generation node
  let generateDraftNode s = do
        liftIO $ T.putStrLn $ "[1. generateDraft] Drafting article for: " <> topic s
        let prompt =
              T.unlines
                [ "Write a short 2-paragraph introductory article on the topic: " <> topic s
                , "Keep the tone professional and informative."
                ]
            msgs = [systemMessage "You are a professional technical content writer.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let draft = extractMessageText resp
        liftIO $ T.putStrLn "  -> Draft generated successfully."
        pure s {draftContent = draft}

      -- Publish post node (runs only after human review)
      publishPostNode s = do
        liftIO $ T.putStrLn "[3. publishPost] Finalizing and publishing article..."
        liftIO $ T.putStrLn $ "  -> Incorporated Human Feedback: " <> humanFeedback s
        pure s {isPublished = True}

      liftNode f s = fmap Right (f s)

  -- -------------------------------------------------------------------------
  -- 2. Build the Graph with HITL checkpoint interrupt
  -- -------------------------------------------------------------------------
  let reviewHitlNode =
        hitlNode cp threadId "humanReview" (pure . Right)

      graphDef =
        addEdge "publishPost" endNodeId
          . addEdge "humanReview" "publishPost"
          . addEdge "generateDraft" "humanReview"
          . addNode "publishPost" (liftNode publishPostNode)
          . addNode "humanReview" (nodeAction reviewHitlNode)
          . addNode "generateDraft" (liftNode generateDraftNode)
          $ emptyStateGraph replaceFieldReducer

  compiledGraph <- case compileGraph graphDef of
    Left err -> error $ "Graph compilation failed: " ++ show err
    Right g -> pure g

  -- -------------------------------------------------------------------------
  -- 3. Execute graph until the HITL interrupt
  -- -------------------------------------------------------------------------
  let initialInput =
        BlogPostState
          { topic = "Why Immutable Data Structures Prevent Concurrency Bugs"
          , draftContent = ""
          , humanFeedback = ""
          , isPublished = False
          }

  T.putStrLn "\n--- Starting Initial Run (Will pause at Human Review) ---"
  runResult <- runExceptT $ runGraph compiledGraph "generateDraft" initialInput

  case runResult of
    Right _ -> T.putStrLn "Unexpected: Graph completed without interrupting for human review."
    Left err -> case isHITLInterrupt err of
      Nothing -> T.putStrLn $ "Unexpected pipeline failure: " <> T.pack (show err)
      Just pausedNodeId -> do
        T.putStrLn $ "\n[!] HITL Interrupt triggered at node: " <> pausedNodeId
        T.putStrLn "    State checkpoint saved to Checkpointer."

        -- -------------------------------------------------------------------
        -- 4. Human Review & State Modification Step
        -- -------------------------------------------------------------------
        T.putStrLn "\n--- [Human Review Station] ---"
        T.putStrLn "Human reviewer inspects the state and modifies content..."

        let humanModifier s =
              s
                { draftContent =
                    draftContent s
                      <> "\n\n[Editor's Verification]: Audited and approved for production release."
                , humanFeedback = "Verified technical accuracy and added editor's signature."
                }

        -- -------------------------------------------------------------------
        -- 5. Resume Graph Execution from Checkpoint
        -- -------------------------------------------------------------------
        T.putStrLn "--- Resuming Graph Execution via 'resumeGraph' ---"
        resumeResult <-
          runExceptT $
            resumeGraph
              compiledGraph
              cp
              threadId
              "humanReview"
              "publishPost"
              humanModifier

        case resumeResult of
          Left resumeErr -> T.putStrLn $ "Error during resume: " <> T.pack (show resumeErr)
          Right finalState -> do
            T.putStrLn "\n=== Article Published Successfully ==="
            T.putStrLn $ "Topic: " <> topic finalState
            T.putStrLn $ "Published Status: " <> T.pack (show (isPublished finalState))
            T.putStrLn $ "Feedback: " <> humanFeedback finalState
            T.putStrLn "\n=== Final Article Content ==="
            T.putStrLn (draftContent finalState)
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run hitlopenai"}
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  Human-In-The-Loop (HITL) and State Checkpointing example using langchain-hs-graph with OpenAI / OpenRouter.

  Workflow:

    generateDraft ──► humanReview [INTERRUPT] ──(resume)──► publishPost ──► __end__

  1. The LLM generates a draft response.
  2. The 'hitlNode' automatically snapshots the state to a persistent or in-memory
     Checkpointer (TVar or SQLite) and halts graph execution with a 'HITL_INTERRUPT' signal.
  3. The application catches the interrupt via 'isHITLInterrupt', renders the draft
     for human evaluation or modification.
  4. The human provides feedback/edits, and 'resumeGraph' reloads the checkpointed
     state, applies the human changes, and continues the graph to completion.
-}
module OpenAI.HITL (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (extractMessageText, systemMessage, userMessage)
import Langchain.Graph.Checkpointer
  ( MemoryCheckpointer
  , newMemoryCheckpointer
  )
import Langchain.Graph.HITL
  ( hitlNode
  , isHITLInterrupt
  , resumeGraph
  )
import Langchain.Graph.StateGraph
  ( Node (..)
  , addEdge
  , addNode
  , compileGraph
  , emptyStateGraph
  , endNodeId
  , replaceFieldReducer
  , runGraph
  )
import Langchain.Prelude (invoke)
import OpenAI.Common (defaultModelName, getOpenRouterModel)

-- ---------------------------------------------------------------------------
-- Shared blog post draft state
-- ---------------------------------------------------------------------------

data BlogPostState = BlogPostState
  { topic :: T.Text
  , draftContent :: T.Text
  , humanFeedback :: T.Text
  , isPublished :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type App = ExceptT LangchainError IO

runApp :: IO ()
runApp = do
  let modelName = defaultModelName
      threadId = "session-hitl-001"
  o <- getOpenRouterModel modelName
  cp <- newMemoryCheckpointer

  T.putStrLn "=== Initializing Human-in-the-Loop (HITL) Pipeline with OpenAI / OpenRouter ==="

  -- -------------------------------------------------------------------------
  -- 1. Node actions
  -- -------------------------------------------------------------------------

  -- Draft generation node
  let generateDraftNode s = do
        liftIO $ T.putStrLn $ "[1. generateDraft] Drafting article for: " <> topic s
        let prompt =
              T.unlines
                [ "Write a short 2-paragraph introductory article on the topic: " <> topic s
                , "Keep the tone professional and informative."
                ]
            msgs = [systemMessage "You are a professional technical content writer.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let draft = extractMessageText resp
        liftIO $ T.putStrLn "  -> Draft generated successfully."
        pure s {draftContent = draft}

      -- Publish post node (runs only after human review)
      publishPostNode s = do
        liftIO $ T.putStrLn "[3. publishPost] Finalizing and publishing article..."
        liftIO $ T.putStrLn $ "  -> Incorporated Human Feedback: " <> humanFeedback s
        pure s {isPublished = True}

      liftNode f s = fmap Right (f s)

  -- -------------------------------------------------------------------------
  -- 2. Build the Graph with HITL checkpoint interrupt
  -- -------------------------------------------------------------------------
  let reviewHitlNode =
        hitlNode cp threadId "humanReview" (pure . Right)

      graphDef =
        addEdge "publishPost" endNodeId
          . addEdge "humanReview" "publishPost"
          . addEdge "generateDraft" "humanReview"
          . addNode "publishPost" (liftNode publishPostNode)
          . addNode "humanReview" (nodeAction reviewHitlNode)
          . addNode "generateDraft" (liftNode generateDraftNode)
          $ emptyStateGraph replaceFieldReducer

  compiledGraph <- case compileGraph graphDef of
    Left err -> error $ "Graph compilation failed: " ++ show err
    Right g -> pure g

  -- -------------------------------------------------------------------------
  -- 3. Execute graph until the HITL interrupt
  -- -------------------------------------------------------------------------
  let initialInput =
        BlogPostState
          { topic = "Why Immutable Data Structures Prevent Concurrency Bugs"
          , draftContent = ""
          , humanFeedback = ""
          , isPublished = False
          }

  T.putStrLn "\n--- Starting Initial Run (Will pause at Human Review) ---"
  runResult <- runExceptT $ runGraph compiledGraph "generateDraft" initialInput

  case runResult of
    Right _ -> T.putStrLn "Unexpected: Graph completed without interrupting for human review."
    Left err -> case isHITLInterrupt err of
      Nothing -> T.putStrLn $ "Unexpected pipeline failure: " <> T.pack (show err)
      Just pausedNodeId -> do
        T.putStrLn $ "\n[!] HITL Interrupt triggered at node: " <> pausedNodeId
        T.putStrLn "    State checkpoint saved to Checkpointer."

        -- -------------------------------------------------------------------
        -- 4. Human Review & State Modification Step
        -- -------------------------------------------------------------------
        T.putStrLn "\n--- [Human Review Station] ---"
        T.putStrLn "Human reviewer inspects the state and modifies content..."

        let humanModifier s =
              s
                { draftContent =
                    draftContent s
                      <> "\n\n[Editor's Verification]: Audited and approved for production release."
                , humanFeedback = "Verified technical accuracy and added editor's signature."
                }

        -- -------------------------------------------------------------------
        -- 5. Resume Graph Execution from Checkpoint
        -- -------------------------------------------------------------------
        T.putStrLn "--- Resuming Graph Execution via 'resumeGraph' ---"
        resumeResult <-
          runExceptT $
            resumeGraph
              compiledGraph
              cp
              threadId
              "humanReview"
              "publishPost"
              humanModifier

        case resumeResult of
          Left resumeErr -> T.putStrLn $ "Error during resume: " <> T.pack (show resumeErr)
          Right finalState -> do
            T.putStrLn "\n=== Article Published Successfully ==="
            T.putStrLn $ "Topic: " <> topic finalState
            T.putStrLn $ "Published Status: " <> T.pack (show (isPublished finalState))
            T.putStrLn $ "Feedback: " <> humanFeedback finalState
            T.putStrLn "\n=== Final Article Content ==="
            T.putStrLn (draftContent finalState)
```
:::
:::

## Core Types & Functions

```haskell
[NodeId] -> StateGraph s m ()
```
```haskell
Checkpointer s -> ThreadId -> s -> m s
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run hitlollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run hitlopenai
```

---
title: Multi-Agent Systems
description: Supervisor routing, sub-graph delegation, and multi-agent coordination.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Graph.StateGraph</span>
</div>

Supervisor routing, hierarchical sub-graphs, and multi-agent state coordination.

## Key Concepts

- **Supervisor Pattern**: A supervisor agent analyzes incoming goals and delegates tasks to specialized sub-agents.
- **Embedded Sub-Graphs**: Sub-graphs are independently compiled `CompiledGraph sub m` state machines embedded as single nodes inside a parent graph via `addSubGraphNode`.
- **Typed State Mapping**: Pure functions map parent state into child sub-graph state `(parent -> sub)` and reconcile results back into parent state `(parent -> sub -> parent)`.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="Ollama (Local)" data-exe="stack run multiagentollama"}
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  Multi-Agent Supervisor and Sub-graph example using langchain-hs-graph.

  Architecture:

                    ┌──► researcherNode ──────────────────┐
                    │                                      ▼
    supervisor ─────┤                                   __end__
                    │                                      ▲
                    └──► coderNode (embedded sub-graph) ──┘
                         [coderGen ──► coderReview]
-}
module Ollama.MultiAgent (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import Langchain.Graph.StateGraph
  ( addConditionalEdge
  , addEdge
  , addNode
  , addSubGraphNode
  , compileGraph
  , emptyStateGraph
  , endNodeId
  , replaceFieldReducer
  , runGraph
  )
import Langchain.Model (extractMessageText, systemMessage, userMessage)
import Langchain.Prelude (invoke)
import Langchain.Provider.Ollama

data ParentState = ParentState
  { taskPrompt :: T.Text
  , selectedRoute :: T.Text
  , finalAnswer :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CoderSubState = CoderSubState
  { codeTask :: T.Text
  , rawCode :: T.Text
  , reviewedCode :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

runApp :: IO ()
runApp = do
  let modelName = "gemma3"
  o <- newOllama modelName defaultConfig

  T.putStrLn "=== Initializing Multi-Agent System with Ollama ==="

  -- 1. Construct the specialized Coder Sub-Graph
  let coderGenNode s = do
        liftIO $ T.putStrLn "  [Coder Sub-Graph] Generating initial code implementation..."
        let prompt =
              T.unlines
                [ "Write a minimal, idiomatic Haskell implementation for the following requirement."
                , "Only output the code, no markdown commentary."
                , "Requirement: " <> codeTask s
                ]
            msgs = [systemMessage "You are an expert Haskell software engineer.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {rawCode = out}

      coderReviewNode s = do
        liftIO $ T.putStrLn "  [Coder Sub-Graph] Reviewing and formatting code..."
        let prompt =
              T.unlines
                [ "Review and improve this Haskell code. Ensure types are explicit and clean."
                , "Code to review:"
                , rawCode s
                ]
            msgs = [systemMessage "You are a senior Haskell code reviewer.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {reviewedCode = out}

      liftSubNode f s = fmap Right (f s)

      subGraphDef =
        addEdge "coderReview" endNodeId
          . addEdge "coderGen" "coderReview"
          . addNode "coderReview" (liftSubNode coderReviewNode)
          . addNode "coderGen" (liftSubNode coderGenNode)
          $ emptyStateGraph replaceFieldReducer

  compiledSubGraph <- case compileGraph subGraphDef of
    Left err -> error $ "Failed to compile coder sub-graph: " ++ show err
    Right sg -> pure sg

  -- 2. Embed Coder Sub-Graph as a single node in Parent Graph
  let toSubState p =
        CoderSubState
          { codeTask = taskPrompt p
          , rawCode = ""
          , reviewedCode = ""
          }
      fromSubState p sub =
        p {finalAnswer = reviewedCode sub}

  -- 3. Construct Researcher node
  let researcherNode s = do
        liftIO $ T.putStrLn "  [Researcher Agent] Researching conceptual explanation..."
        let prompt =
              T.unlines
                [ "Explain the following technical concept clearly and concisely in 2-3 paragraphs."
                , "Question: " <> taskPrompt s
                ]
            msgs = [systemMessage "You are a computer science research specialist.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {finalAnswer = out}

      liftParentNode f s = fmap Right (f s)

  -- 4. Supervisor Routing Node
  let supervisorAction s = do
        let prompt =
              "You are a supervisor delegating tasks to sub-agents. Available routes:\n"
                <> "researcher -> researcherNode\n"
                <> "coder -> coderNode\n"
                <> "\nTask context: "
                <> taskPrompt s
                <> "\nReply with ONLY 'researcher' or 'coder'."
        resp <- invoke o [userMessage prompt] Nothing
        let rawRoute = T.toLower $ T.strip (extractMessageText resp)
            target = if "code" `T.isInfixOf` rawRoute then "coderNode" else "researcherNode"
        pure $ Right $ s {selectedRoute = target}

  -- 5. Compile Parent Multi-Agent Graph
  let parentGraphDef =
        addEdge "researcherNode" endNodeId
          . addEdge "coderNode" endNodeId
          . addConditionalEdge "supervisor" (pure . Right . selectedRoute)
          . addNode "researcherNode" (liftParentNode researcherNode)
          . addSubGraphNode "coderNode" "coderGen" compiledSubGraph toSubState fromSubState
          . addNode "supervisor" supervisorAction
          $ emptyStateGraph replaceFieldReducer

  compiledParentGraph <- case compileGraph parentGraphDef of
    Left err -> error $ "Failed to compile parent graph: " ++ show err
    Right g -> pure g

  -- 6. Test with a coding task
  let codingTask =
        ParentState
          { taskPrompt = "Write a function `fibonacci :: Int -> Integer` with memoization."
          , selectedRoute = ""
          , finalAnswer = ""
          }

  T.putStrLn "\n--- Dispatching Task 1: Coding Request ---"
  T.putStrLn $ "User: " <> taskPrompt codingTask
  res1 <- runExceptT $ runGraph compiledParentGraph "supervisor" codingTask
  case res1 of
    Left err -> T.putStrLn $ "Error in run 1: " <> T.pack (show err)
    Right finalSt -> do
      T.putStrLn $ "Route Selected: " <> selectedRoute finalSt
      T.putStrLn "=== Result ==="
      T.putStrLn (finalAnswer finalSt)

  -- 7. Test with a conceptual research task
  let researchTask =
        ParentState
          { taskPrompt = "What is the difference between Lazy and Strict evaluation in functional languages?"
          , selectedRoute = ""
          , finalAnswer = ""
          }

  T.putStrLn "\n--- Dispatching Task 2: Conceptual Research Request ---"
  T.putStrLn $ "User: " <> taskPrompt researchTask
  res2 <- runExceptT $ runGraph compiledParentGraph "supervisor" researchTask
  case res2 of
    Left err -> T.putStrLn $ "Error in run 2: " <> T.pack (show err)
    Right finalSt -> do
      T.putStrLn $ "Route Selected: " <> selectedRoute finalSt
      T.putStrLn "=== Result ==="
      T.putStrLn (finalAnswer finalSt)
```
:::

::: {.provider-panel data-provider="openai" data-label="OpenAI / OpenRouter" data-exe="stack run multiagentopenai"}
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  Multi-Agent Supervisor and Sub-graph example using langchain-hs-graph with OpenAI / OpenRouter.

  Architecture:

                    ┌──► researcherNode ──────────────────┐
                    │                                      ▼
    supervisor ─────┤                                   __end__
                    │                                      ▲
                    └──► coderNode (embedded sub-graph) ──┘
                         [coderGen ──► coderReview]
-}
module OpenAI.MultiAgent (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import Langchain.Graph.StateGraph
  ( addConditionalEdge
  , addEdge
  , addNode
  , addSubGraphNode
  , compileGraph
  , emptyStateGraph
  , endNodeId
  , replaceFieldReducer
  , runGraph
  )
import Langchain.Model (extractMessageText, systemMessage, userMessage)
import Langchain.Prelude (invoke)
import OpenAI.Common (defaultModelName, getOpenRouterModel)

data ParentState = ParentState
  { taskPrompt :: T.Text
  , selectedRoute :: T.Text
  , finalAnswer :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CoderSubState = CoderSubState
  { codeTask :: T.Text
  , rawCode :: T.Text
  , reviewedCode :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

runApp :: IO ()
runApp = do
  let modelName = defaultModelName
  o <- getOpenRouterModel modelName

  T.putStrLn "=== Initializing Multi-Agent System with OpenAI / OpenRouter ==="

  -- 1. Construct the specialized Coder Sub-Graph
  let coderGenNode s = do
        liftIO $ T.putStrLn "  [Coder Sub-Graph] Generating initial code implementation..."
        let prompt =
              T.unlines
                [ "Write a minimal, idiomatic Haskell implementation for the following requirement."
                , "Only output the code, no markdown commentary."
                , "Requirement: " <> codeTask s
                ]
            msgs = [systemMessage "You are an expert Haskell software engineer.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {rawCode = out}

      coderReviewNode s = do
        liftIO $ T.putStrLn "  [Coder Sub-Graph] Reviewing and formatting code..."
        let prompt =
              T.unlines
                [ "Review and improve this Haskell code. Ensure types are explicit and clean."
                , "Code to review:"
                , rawCode s
                ]
            msgs = [systemMessage "You are a senior Haskell code reviewer.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {reviewedCode = out}

      liftSubNode f s = fmap Right (f s)

      subGraphDef =
        addEdge "coderReview" endNodeId
          . addEdge "coderGen" "coderReview"
          . addNode "coderReview" (liftSubNode coderReviewNode)
          . addNode "coderGen" (liftSubNode coderGenNode)
          $ emptyStateGraph replaceFieldReducer

  compiledSubGraph <- case compileGraph subGraphDef of
    Left err -> error $ "Failed to compile coder sub-graph: " ++ show err
    Right sg -> pure sg

  -- 2. Embed Coder Sub-Graph as a single node in Parent Graph
  let toSubState p =
        CoderSubState
          { codeTask = taskPrompt p
          , rawCode = ""
          , reviewedCode = ""
          }
      fromSubState p sub =
        p {finalAnswer = reviewedCode sub}

  -- 3. Construct Researcher node
  let researcherNode s = do
        liftIO $ T.putStrLn "  [Researcher Agent] Researching conceptual explanation..."
        let prompt =
              T.unlines
                [ "Explain the following technical concept clearly and concisely in 2-3 paragraphs."
                , "Question: " <> taskPrompt s
                ]
            msgs = [systemMessage "You are a computer science research specialist.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {finalAnswer = out}

      liftParentNode f s = fmap Right (f s)

  -- 4. Supervisor Routing Node
  let supervisorAction s = do
        let prompt =
              "You are a supervisor delegating tasks to sub-agents. Available routes:\n"
                <> "researcher -> researcherNode\n"
                <> "coder -> coderNode\n"
                <> "\nTask context: "
                <> taskPrompt s
                <> "\nReply with ONLY 'researcher' or 'coder'."
        resp <- invoke o [userMessage prompt] Nothing
        let rawRoute = T.toLower $ T.strip (extractMessageText resp)
            target = if "code" `T.isInfixOf` rawRoute then "coderNode" else "researcherNode"
        pure $ Right $ s {selectedRoute = target}

  -- 5. Compile Parent Multi-Agent Graph
  let parentGraphDef =
        addEdge "researcherNode" endNodeId
          . addEdge "coderNode" endNodeId
          . addConditionalEdge "supervisor" (pure . Right . selectedRoute)
          . addNode "researcherNode" (liftParentNode researcherNode)
          . addSubGraphNode "coderNode" "coderGen" compiledSubGraph toSubState fromSubState
          . addNode "supervisor" supervisorAction
          $ emptyStateGraph replaceFieldReducer

  compiledParentGraph <- case compileGraph parentGraphDef of
    Left err -> error $ "Failed to compile parent graph: " ++ show err
    Right g -> pure g

  -- 6. Test with a coding task
  let codingTask =
        ParentState
          { taskPrompt = "Write a function `fibonacci :: Int -> Integer` with memoization."
          , selectedRoute = ""
          , finalAnswer = ""
          }

  T.putStrLn "\n--- Dispatching Task 1: Coding Request ---"
  T.putStrLn $ "User: " <> taskPrompt codingTask
  res1 <- runExceptT $ runGraph compiledParentGraph "supervisor" codingTask
  case res1 of
    Left err -> T.putStrLn $ "Error in run 1: " <> T.pack (show err)
    Right finalSt -> do
      T.putStrLn $ "Route Selected: " <> selectedRoute finalSt
      T.putStrLn "=== Result ==="
      T.putStrLn (finalAnswer finalSt)

  -- 7. Test with a conceptual research task
  let researchTask =
        ParentState
          { taskPrompt = "What is the difference between Lazy and Strict evaluation in functional languages?"
          , selectedRoute = ""
          , finalAnswer = ""
          }

  T.putStrLn "\n--- Dispatching Task 2: Conceptual Research Request ---"
  T.putStrLn $ "User: " <> taskPrompt researchTask
  res2 <- runExceptT $ runGraph compiledParentGraph "supervisor" researchTask
  case res2 of
    Left err -> T.putStrLn $ "Error in run 2: " <> T.pack (show err)
    Right finalSt -> do
      T.putStrLn $ "Route Selected: " <> selectedRoute finalSt
      T.putStrLn "=== Result ==="
      T.putStrLn (finalAnswer finalSt)
```
:::
:::

## Expected Output

```text
=== Initializing Multi-Agent System ===

--- Dispatching Task 1: Coding Request ---
User: Write a function `fibonacci :: Int -> Integer` with memoization.
  [Coder Sub-Graph] Generating initial code implementation...
  [Coder Sub-Graph] Reviewing and formatting code...
Route Selected: coderNode
=== Result ===
fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

--- Dispatching Task 2: Conceptual Research Request ---
User: What is the difference between Lazy and Strict evaluation in functional languages?
  [Researcher Agent] Researching conceptual explanation...
Route Selected: researcherNode
=== Result ===
In functional programming, evaluation strategy dictates when expressions are calculated into values. In a strict language, expressions are evaluated immediately when bound to a variable or passed as an argument (call-by-value). 

In contrast, lazy evaluation (call-by-need) defers computation until the value is strictly required for execution. Expressions are represented as unevaluated thunks and memoized once calculated.
```

## Running This Example

### Local Ollama
```bash
ollama run gemma3
stack run multiagentollama
```

### OpenAI / OpenRouter
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run multiagentopenai
```

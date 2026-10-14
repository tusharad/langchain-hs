---
title: Building Your First StateGraph
description: Build cyclic multi-step workflows with STM checkpointers, reducers, and LangGraph-style orchestration.
category: Getting Started
---

## What is a `StateGraph`?

`langchain-hs-graph` brings the power of **LangGraph** to Haskell. A `StateGraph s m` is a cyclic state machine where nodes process an application state `s` and pass updates back to a central pure state reducer.

Key guarantees in Haskell:
- **Pure State Updates**: State transitions are merged using a pure `StateReducer s`.
- **Thread Safety**: Concurrent parallel node branches with STM `TVar` checkpointers.
- **Resumability**: Full state serialization with `MemoryCheckpointer` or `SQLiteCheckpointer`.
- **Time-Travel**: Snapshot history inspection, human-in-the-loop review, and rollbacks.

```text
       [ __START__ ]
             |
             v
       +-----------+
       |  planner  | <----------+
       +-----+-----+            |
             |                  | (needs_revision)
             v                  |
       +-----------+            |
       | generator |            |
       +-----+-----+            |
             |                  |
             v                  |
       +-----------+            |
       | reviewer  | -----------+
       +-----+-----+
             | (approved)
             v
        [ __END__ ]
```

---

## 1. Defining the State and Reducer

Define your workflow state record and an associative state reducer:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude

data WorkflowState = WorkflowState
  { draftText  :: Text
  , critique   :: Text
  , iterations :: Int
  } deriving (Show, Eq)

-- Pure reducer merging new state fields with existing state
workflowReducer :: StateReducer WorkflowState
workflowReducer = StateReducer $ \old new ->
  WorkflowState
    { draftText  = if draftText new /= "" then draftText new else draftText old
    , critique   = if critique new /= "" then critique new else critique old
    , iterations = iterations old + 1
    }
```

---

## 2. Defining Graph Nodes

Nodes are standard effectful functions with signature `s -> m s`:

```haskell
plannerNode :: WorkflowState -> IO WorkflowState
plannerNode st = do
  putStrLn $ "[planner] Generating draft version " <> show (iterations st + 1)
  pure st { draftText = "Draft " <> showText (iterations st + 1) <> ": Building Agents with Haskell State Graphs" }

generatorNode :: WorkflowState -> IO WorkflowState
generatorNode st = do
  putStrLn "[generator] Expanding technical details and architecture guarantees"
  pure st { draftText = draftText st <> " (Expanded: type safety, zero race conditions)" }

reviewerNode :: WorkflowState -> IO WorkflowState
reviewerNode st = do
  putStrLn "[reviewer] Inspecting draft quality..."
  if iterations st >= 2
    then do
      putStrLn "[reviewer] Quality criteria met. Approving workflow."
      pure st { critique = "APPROVED" }
    else do
      putStrLn "[reviewer] Revision needed. Requesting another iteration."
      pure st { critique = "NEEDS_WORK" }
```

---

## 3. Constructing, Compiling, and Running the Graph

Wire nodes, sequential edges, and conditional routing edges:

```haskell
buildWorkflow :: StateGraph WorkflowState IO
buildWorkflow = emptyStateGraph workflowReducer
  & addNode "planner" plannerNode
  & addNode "generator" generatorNode
  & addNode "reviewer" reviewerNode
  & addEdge startNodeId "planner"
  & addEdge "planner" "generator"
  & addEdge "generator" "reviewer"
  & addConditionalEdge "reviewer" (\st -> 
      if critique st == "APPROVED" then endNodeId else "planner")
      [ ("approved", endNodeId)
      , ("needs_work", "planner")
      ]

main :: IO ()
main = do
  putStrLn "--- Initializing MemoryCheckpointer ---"
  checkpointer <- newMemoryCheckpointer

  putStrLn "--- Compiling StateGraph ---"
  let compiled = compileGraph buildWorkflow (Just checkpointer)

  let initialState = WorkflowState "" "" 0
  finalState <- runGraph compiled initialState

  putStrLn "\n--- Workflow Execution Completed ---"
  putStrLn $ "Final Iterations: " ++ show (iterations finalState)
  putStrLn $ "Review Status:    " ++ show (critique finalState)
  putStrLn $ "Final Draft:      " ++ show (draftText finalState)
```

---

## Execution Output

When you run this program with `stack run` or `cabal run`, the graph loops through the cyclic revision edge until the reviewer approves:

```text
--- Initializing MemoryCheckpointer ---
--- Compiling StateGraph ---
[planner] Generating draft version 1
[generator] Expanding technical details and architecture guarantees
[reviewer] Inspecting draft quality...
[reviewer] Revision needed. Requesting another iteration.
[planner] Generating draft version 2
[generator] Expanding technical details and architecture guarantees
[reviewer] Inspecting draft quality...
[reviewer] Quality criteria met. Approving workflow.

--- Workflow Execution Completed ---
Final Iterations: 3
Review Status:    "APPROVED"
Final Draft:      "Draft 2: Building Agents with Haskell State Graphs (Expanded: type safety, zero race conditions)"
```

<div class="admonition">
  <div class="admonition-title">Graphviz Export</div>
  You can export any <code>StateGraph</code> to standard DOT format using <code>toDot buildWorkflow</code> and render it with Graphviz!
</div>

---
title: State Graphs & Checkpointers
description: Cyclic state machines, pure reducers, STM TVar checkpointers, SQLite persistence, and Time-Travel.
category: Core Concepts
---

## LangGraph in Haskell: `StateGraph s m`

In `langchain-hs-graph`, complex multi-step workflows are expressed as directed graphs with nodes, edges, conditional branches, and cyclic loops:

```haskell
data StateGraph s m = StateGraph
  { graphReducer :: StateReducer s
  , graphNodes   :: Map NodeId (Node s m)
  , graphEdges   :: [Edge s m]
  }
```

---

## 1. Pure State Reducers

State updates are merged using a `StateReducer s`, which is an associative binary operation:

```haskell
newtype StateReducer s = StateReducer { runReducer :: s -> s -> s }

-- Predefined reducers:
appendMessagesReducer :: StateReducer [Message]
replaceFieldReducer  :: (s -> a -> s) -> StateReducer s
```

---

## 2. Checkpointers & Persistence

Checkpointers save state snapshots at every step to allow pause/resume, human approval, and distributed execution.

### MemoryCheckpointer (STM `TVar`)
Ultra-low latency in-memory checkpointer utilizing Haskell's Software Transactional Memory:

```haskell
checkpointer <- newMemoryCheckpointer
let compiled = compileGraph workflow (Just checkpointer)
```

### SQLiteCheckpointer (ACID Disk Persistence)
Saves execution states into SQLite database tables for long-running workflows:

```haskell
sqliteCheckpointer <- newSQLiteCheckpointer "workflow_state.db"
let compiled = compileGraph workflow (Just sqliteCheckpointer)
```

---

## 3. Time-Travel & History Inspection

`TimeTravelHistory` stores state snapshots with timestamps, allowing you to replay or fork graph executions from any historical checkpoint:

```haskell
timeTravel <- newTimeTravelHistory

-- Retrieve all previous snapshots
snapshots <- getSnapshots timeTravel

-- Rollback and resume execution from step 3
resumedState <- resumeFromSnapshot compiled (snapshots !! 2)
```

---

## 4. Graphviz DOT Export

Inspect and visualize your compiled state graph visually:

```haskell
putStrLn (toDot workflow)
```
Generates standard Graphviz DOT markup that can be rendered to SVG or PNG.

---
title: Pure AST Pipelines (RunnableTree)
description: Constructing, optimizing, and interpreting purely functional runnable pipelines.
category: Core Concepts
---

## What is `RunnableTree`?

`RunnableTree m i o` is a Generalized Algebraic Data Type (GADT) representing an abstract syntax tree of a computation that accepts input `i` and produces output `o` in monad `m`.

Unlike Python's LCEL (LangChain Expression Language) which executes immediately via dynamic dispatch, `RunnableTree` in Haskell builds a pure AST that can be inspected, visualized, optimized, or transformed prior to interpretation.

---

## AST Constructors

```haskell
data RunnableTree m i o where
  -- | Pure in-memory transformation
  RunLambda  :: (i -> o) -> RunnableTree m i o
  
  -- | Primitive monadic action (e.g. LLM call, DB lookup)
  RunPrim    :: (i -> m o) -> RunnableTree m i o
  
  -- | Sequential composition (Sequential pipeline)
  RunSeq     :: RunnableTree m i a -> RunnableTree m a o -> RunnableTree m i o
  
  -- | Parallel branch execution (splits input to both, combines results into tuple)
  RunPar     :: RunnableTree m i a -> RunnableTree m i b -> RunnableTree m i (a, b)
```

---

## Pipeline Operators

`langchain-hs` provides clean infix operators to compose pipelines with ease:

| Operator | Type | Description |
|---|---|---|
| `(\|>>)` | `RunnableTree m i a -> RunnableTree m a o -> RunnableTree m i o` | Sequential pipe |
| `(&>&)` | `RunnableTree m i a -> RunnableTree m i b -> RunnableTree m i (a, b)` | Parallel fork |
| `(>>>#)` | `PipelineStep m i a -> PipelineStep m a o -> PipelineStep m i o` | DSL Chain Step |

---

## Example: Building a RAG Pipeline

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Prelude

-- Construct pipeline AST
ragPipeline :: RunnableTree IO Text Text
ragPipeline =
      runLambda (\q -> (q, q))                      -- duplicate query
  |>> (fetchDocs &>& rewriteQuery)                   -- parallel retrieval and query expansion
  |>> runLambda (\(docs, expanded) -> buildPrompt docs expanded)
  |>> invokeLLM model
  |>> runLambda extractMessageText

main :: IO ()
main = do
  -- Pure AST constructed without performing any IO
  putStrLn "Interpreting AST..."
  result <- interpret ragPipeline "Explain Type Families in Haskell"
  T.putStrLn result
```

<div class="admonition tip">
  <div class="admonition-title">⚡ High Performance</div>
  Because parallel branches (<code>&>&</code>) are explicitly represented in the AST, the interpreter evaluates them concurrently using lightweight GHC green threads (<code>async</code>).
</div>

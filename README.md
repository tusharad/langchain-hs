# 🦜️🔗 LangChain Haskell v2 (`langchain-hs`)

> **Functional Programming Principles First AI Agent & Orchestration Engine**
> 
> *A superior, strictly-typed, effect-polymorphic Haskell AI framework built on pure AST pipelines, graph state machines, laws, and thread-safe persistence.*

---

[![Build Status](https://img.shields.io/badge/tests-202%20passed-brightgreen.svg)]()
[![Hackage](https://img.shields.io/badge/hackage-v0.5.0-blue.svg)](https://hackage.haskell.org/package/langchain-hs)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## 🌟 Why `langchain-hs` v2?

`langchain-hs` v2 is **NOT** a direct line-by-line port of Python/JS LangChain. Instead, it is redesigned from first principles to leverage Haskell's unique strengths:

1. **Zero-Dependency Pure Core (`langchain-hs-core`)**: Pure GADT pipeline ASTs (`RunnableTree m i o`), unified multi-modal message block model (`ContentBlock`), effect-polymorphic `ChatModel`, and `StreamEvent` streaming protocols without ANY HTTP dependencies.
2. **Type-Safe Graph State Machine Engine (`langchain-hs-graph`)**: First-class `StateGraph s m`, pure state merge reducers (`StateReducer s`), thread-safe `MemoryCheckpointer` (`TVar`), and persistent `SQLiteCheckpointer`.
3. **Algebraic Laws & Property Verification**: Reducer associativity laws `(a <> b) <> c == a <> (b <> c)` and checkpointer invariants verified via QuickCheck property tests.
4. **Human-in-the-Loop (HITL)**: Built-in interrupt signals (`hitlNode`) and state resume (`resumeGraph`) allowing human modification before continuing workflow execution.
5. **Effect-Polymorphic Tools (`Tool m`)**: Pure schema generation (`toolToValue`) and type-safe tool execution without string-based workaround hacks.
6. **Multi-Agent Supervisor Pattern**: LLM-guided supervisor nodes and nested sub-graph embedding (`embedSubGraphNode`).

---

## 📦 Packages in Monorepo

| Package | Version | Description |
|---|---|---|
| [`langchain-hs-core`](./langchain-hs-core) | `0.2.0.0` | Pure AST pipeline (`RunnableTree`), `ChatModel`, `ContentBlock`, `Tool m`, `StreamEvent`. Zero HTTP deps. |
| [`langchain-hs-graph`](./langchain-hs-graph) | `0.5.0.0` | `StateGraph s m`, `StateReducer s`, `Checkpointer` (Memory & SQLite), `HITL`, `MultiAgent`. |
| [`langchain-hs`](./) | `0.5.0.0` | High-level LLM providers (Ollama, OpenAI, Anthropic, Gemini, DeepSeek), Memory, VectorStore, Retriever. |

---

## 🚀 Supported Providers

- 🦙 **Ollama**: Local models (`gemma3:latest`, `qwen3.5:2b`, `llama3.2`) with `ollama-haskell 0.3.0.0`
- 🧠 **DeepSeek**: R1 Reasoning models with automated reasoning chain extraction (`<think>...</think>`)
- 🤖 **OpenAI & OpenAI-Compatible**: GPT-4o, Claude endpoints, OpenRouter, Together, Fireworks
- 🎭 **Anthropic**: Claude 3.5 Sonnet with extended thinking budget payload & vision
- ♊ **Google Gemini**: Gemini 1.5 Pro/Flash and Gemini Embeddings

---

## 💡 Quickstart Examples

### 1. Pure AST Pipeline (`RunnableTree`)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Langchain.Core.Runnable
import Langchain.Core.Error
import Control.Monad.Except

main :: IO ()
main = do
  let uppercase = runLambda (\t -> pure $ Right (T.toUpper t))
      exclamation = runLambda (\t -> pure $ Right (t <> "!"))
      pipeline = uppercase |>> exclamation

  res <- runExceptT $ interpret pipeline "hello world"
  print res -- Right "HELLO WORLD!"
```

### 2. Stateful Graph Agent with HITL Interrupt & Resume

```haskell
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
import Langchain.Graph.StateGraph
import Langchain.Graph.Checkpointer
import Langchain.Graph.HITL
import GHC.Generics
import Data.Aeson

data AppState = AppState { result :: Text } deriving (Eq, Show, Generic, ToJSON, FromJSON)

appReducer :: StateReducer AppState
appReducer _ new = new

main :: IO ()
main = do
  cp <- newMemoryCheckpointer
  let threadId = "thread-1"
      prepareNode = Node "prepare" $ \s -> pure $ Right s { result = "Draft Plan" }
      approvalHitl = hitlNode cp threadId "approval" $ \s -> pure $ Right s
      finalizeNode = Node "finalize" $ \s -> pure $ Right s { result = result s <> " -> Executed!" }

      graph = addEdge "prepare" "approval" 
            $ addEdge "approval" "finalize" 
            $ addEdge "finalize" endNodeId 
            $ addNode "prepare" (nodeAction prepareNode)
            $ addNode "approval" (nodeAction approvalHitl)
            $ addNode "finalize" (nodeAction finalizeNode)
            $ emptyStateGraph appReducer

  Right compiled <- pure $ compileGraph graph
  -- Executes until HITL interrupt at node "approval"
  res <- runExceptT $ runGraph compiled "prepare" (AppState "")
  -- Resume execution after human review
  resFinal <- runExceptT $ resumeGraph compiled cp threadId "approval" "finalize" (\s -> s { result = result s <> " [Human Approved]" })
  print resFinal -- Right (AppState {result = "Draft Plan [Human Approved] -> Executed!"})
```

---

## 🧪 Comprehensive 4-Tier Test Suite

Run full workspace test suite:
```bash
cd langchain-hs && stack test
```

- **Unit Tests**: Provider parsing, model responses, tool execution, file system operations.
- **Property Tests**: QuickCheck verification of `RunnableTree` identity/associativity laws & `StateReducer` associativity laws.
- **Integration Tests**: Live execution against local Ollama models (`gemma3:latest`), SQLite checkpoint persistence, and multi-agent routing.
- **Showcase Application**: Run the comprehensive showcase app stretching all features:
  ```bash
  stack run big-showcase
  ```

---

## 📜 License

Distributed under the MIT License. See [LICENSE](LICENSE) for details.

# 🦜️🔗 LangChain Haskell (`langchain-hs`)

> **The Pure Functional, Effect-Polymorphic AI Agent & Multi-Agent Graph Engine in Haskell**
>
> *A strictly typed, effect-polymorphic, AI ecosystem built on pure AST pipelines (`RunnableTree`), cyclic state machines (`StateGraph`), Model Context Protocol (MCP), and production observability.*

---

[![Hackage](https://img.shields.io/badge/hackage-0.0.5.0-blue.svg)](https://hackage.haskell.org/package/langchain-hs)
[![GHC](https://img.shields.io/badge/GHC-9.8%2B-purple.svg)](https://www.haskell.org/ghc/)
[![Components](https://img.shields.io/badge/components-20%20verified-brightgreen.svg)](#-20-core-components--verified-targets)
[![Providers](https://img.shields.io/badge/providers-Ollama%20%7C%20OpenAI%20%7C%20Gemini-orange.svg)](#-dual-provider-parity-ollama--openai)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Whitepaper](https://img.shields.io/badge/whitepaper-read%20now-blueviolet.svg)](whitepaper.md)

---

## Why `langchain-hs`?

Modern AI orchestration frameworks often struggle with race conditions, hidden side-effects, fragile dynamic schemas, and uninspectable opaque execution chains. `langchain-hs` brings mathematical precision and functional programming principles to AI development:

1. **First-Class Runnable AST Composition (`RunnableTree`)**: Every component—models, prompts, tools, chains, retrievers, and parsers—implements the `Runnable` typeclass. Connect components into trees or graphs using type-safe operators:
   - `|>>` : Sequential composition (data flows from left to right).
   - `&>&` : Parallel fan-out (concurrent evaluation of independent branches).
   - `>>>#` : Fallback chains (automatic failover if the primary branch errors).
2. **LangGraph in Haskell (`StateGraph`)**: Full cyclic state machine engine with pure monoidal state reducers (`StateReducer s`), thread-safe STM memory checkpointers (`TVar`), persistent SQLite checkpointers, Human-in-the-Loop (`HITL`) interrupts, and Time-Travel state replay.

---


### Monorepo Packages

| Package | Directory | Version | Description |
|---|---|---|---|
| `langchain-hs-core` | [`langchain-hs-core/`](./langchain-hs-core) | `0.0.5.0` | Zero-dependency pure core: `RunnableTree`, `ChatModel`, `ContentBlock`, `Tool`, and `LangchainT`. |
| `langchain-hs-graph` | [`langchain-hs-graph/`](./langchain-hs-graph) | `0.0.5.0` | Stateful graph engine: `StateGraph s m`, checkpointers, HITL, time-travel, and parallel nodes. |
| `langchain-hs` | [`./`](./) | `0.0.5.0` | Production ecosystem: Ollama/OpenAI providers, Agents, MCP, Vector Stores, Chains, Observability. |
| `examples` | [`examples/`](./examples) | - | 41 runnable executables covering all 20 components for Ollama and OpenAI. |
| `site` | [`site/`](./site) | - | Hakyll documentation website with live provider toggle and component reference. |

---

## 20 Core Components & Verified Targets

| # | Component | Package Layer | Ollama Executable | OpenAI Executable | Documentation |
|:---:|---|---|---|---|:---:|
| 1 | **Chat Models** | `Langchain.Core.Model` | `stack run simpleollama` | `stack run simpleopenai` | [Docs](site/components/chat-models.md) |
| 2 | **Conduit Streaming** | `Langchain.Core.Stream` | `stack run streamollama` | `stack run streamopenai` | [Docs](site/components/streaming.md) |
| 3 | **Langchain Monad** | `Langchain.Core.Monad` | `stack run monadollama` | `stack run monadopenai` | [Docs](site/components/monad.md) |
| 4 | **Tools & Function Calling** | `Langchain.Core.Tool` | `stack run toolollama` | `stack run toolopenai` | [Docs](site/components/tools.md) |
| 5 | **Structured Outputs** | `Langchain.OutputParser` | `stack run jsonollama` | `stack run jsonopenai` | [Docs](site/components/structured-output.md) |
| 6 | **RAG & Embeddings** | `Langchain.Embedding` | `stack run ragollama` | `stack run ragopenai` | [Docs](site/components/rag.md) |
| 7 | **Hybrid Retrievers** | `Langchain.Retriever` | `stack run retrieverollama` | `stack run retrieveropenai` | [Docs](site/components/retrievers.md) |
| 8 | **Memory Systems** | `Langchain.Memory` | `stack run memoryollama` | `stack run memoryopenai` | [Docs](site/components/memory.md) |
| 9 | **Retrieval QA Chains** | `Langchain.Chain.RetrievalQA` | `stack run retrievalqaollama` | `stack run retrievalqaopenai` | [Docs](site/components/retrieval-qa.md) |
| 10 | **Map-Reduce Processing** | `Langchain.Chain.MapReduce` | `stack run mapreduceollama` | `stack run mapreduceopenai` | [Docs](site/components/map-reduce.md) |
| 11 | **ReAct Agent** | `Langchain.Agent.ReAct` | `stack run reactollama` | `stack run reactopenai` | [Docs](site/components/react-agent.md) |
| 12 | **Plan-and-Execute Agent** | `Langchain.Agent.PlanAndExecute` | `stack run planandexecuteollama` | `stack run planandexecuteopenai` | [Docs](site/components/plan-and-execute.md) |
| 13 | **Guardrails & Safety** | `Langchain.Guardrails` | `stack run guardrailollama` | `stack run guardrailopenai` | [Docs](site/components/guardrails.md) |
| 14 | **Resilience & Retries** | `Langchain.Resilience` | `stack run resilienceollama` | `stack run resilienceopenai` | [Docs](site/components/resilience.md) |
| 15 | **Observability & Tracing** | `Langchain.Observability` | `stack run observabilityollama` | `stack run observabilityopenai` | [Docs](site/components/observability.md) |
| 16 | **Model Context Protocol** | `Langchain.MCP.Client` | `stack run mcpollama` | `stack run mcpopenai` | [Docs](site/components/mcp.md) |
| 17 | **StateGraph Workflows** | `Langchain.Graph` | `stack run stategraphollama` | `stack run stategraphopenai` | [Docs](site/components/state-graph.md) |
| 18 | **Multi-Agent Systems** | `Langchain.Graph.MultiAgent` | `stack run multiagentollama` | `stack run multiagentopenai` | [Docs](site/components/multi-agent.md) |
| 19 | **Human-in-the-Loop (HITL)** | `Langchain.Graph.Checkpointer` | `stack run hitlollama` | `stack run hitlopenai` | [Docs](site/components/hitl.md) |
| 20 | **Runnables & AST Composition** | `Langchain.Core.Runnable` | `stack run runnableollama` | `stack run runnableopenai` | [Docs](site/components/runnables.md) |

---

## Code Showcases

### 1. The Power of Runnables: Pure AST Composition

Compose complex multi-stage pipelines using typed operators without executing any `IO` until interpretation:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Langchain.Prelude

-- Compose pure AST pipelines with (|>>), (&>&), and (>>>#)
pipeline :: RunnableTree IO Text Text
pipeline =
      runLambda (\q -> (q, q))                          -- duplicate input query
  |>> (fetchDocuments &>& generateFollowup)              -- parallel branch fan-out
  |>> runLambda (\(docs, fup) -> renderPrompt docs fup) -- pure prompt synthesis
  |>> (invokeLLM primaryModel >>># invokeLLM backupModel) -- fallback resilience
  |>> parseStructuredResponse                           -- JSON parser

main :: IO ()
main = do
  output <- interpret pipeline "Explain Monads in Haskell"
  print output
```

---

### 2. Dual-Provider Chat Comparison: Ollama vs OpenAI

#### Ollama (Local & Offline)
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude

main :: IO ()
main = do
  -- Connect to local Ollama instance (DeepSeek, Llama 3, Gemma)
  model <- newOllama "gemma3" defaultConfig
  
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runExceptT $ invoke model msg Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m  -> T.putStrLn $ extractMessageText m
```
*Run:* `stack run simpleollama`

#### OpenAI / OpenRouter (Cloud)
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

main :: IO ()
main = do
  -- Connect to OpenAI or OpenRouter using environment API key
  model <- getOpenRouterModel defaultModelName
  
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runExceptT $ invoke model msg Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m  -> T.putStrLn $ extractMessageText m
```
*Run:* `stack run simpleopenai`

---

### 3. Stateful Graphs (`StateGraph`): Cyclic Multi-Agent Workflow

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Langchain.Graph.StateGraph
import Langchain.Prelude

-- Pure state with a list-append reducer
data AgentState = AgentState { messages :: [Message], loopCount :: Int }

-- Build the graph using pure combinators
workflow :: StateGraph AgentState IO
workflow =
  addEdge "reviewer" "planner"          -- cyclic feedback loop!
    $ addConditionalEdge "executor"
        (\s -> pure $ if done s then Right endNodeId else Right "reviewer")
    $ addEdge "planner" "executor"
    $ addEdge startNodeId "planner"
    $ addNode "reviewer" (Node reviewerNode replaceFieldReducer)
    $ addNode "executor" (Node executorNode replaceFieldReducer)
    $ addNode "planner"  (Node plannerNode  replaceFieldReducer)
    $ emptyStateGraph

main :: IO ()
main = do
  checkpointer <- newMemoryCheckpointer
  case compileGraph workflow of
    Left err -> print err
    Right compiled -> do
      result <- runGraph compiled initialState (Just checkpointer)
      print result
```
*Run:* `stack run stategraphollama` or `stack run stategraphopenai`

---

### 4. Model Context Protocol (MCP) Tools Integration

Connect Haskell agents to any external MCP server (e.g., Hackage doc search, SQLite, Filesystem, GitHub) over stdio:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Langchain.Prelude

main :: IO ()
main = do
  -- Connect to any MCP server via stdio JSON-RPC 2.0
  client <- newStdioMcpClient "docker" ["run", "-i", "--rm", "mcp/hackage-doc"]
  
  -- Discover available tools from server
  mcpTools <- listMcpTools client
  let nativeTools = map mcpToolToLangchainTool mcpTools
  
  -- Bind tools to your ReAct or Plan-and-Execute Agent
  let agent = createReActAgent model nativeTools defaultAgentConfig
  res <- runReActAgent agent "Search Hoogle for the signature of 'traverse'"
  print res
```
*Run:* `stack run mcpollama` or `stack run mcpopenai`

---

## Installation

### Stack
Add to your `stack.yaml`:
```yaml
extra-deps:
  - langchain-hs-core-0.0.5.0
  - langchain-hs-graph-0.0.5.0
  - langchain-hs-0.0.5.0
```
Then in your `.cabal` or `package.yaml`:
```yaml
dependencies:
  - langchain-hs        # full ecosystem (providers, agents, MCP, vector stores)
  - langchain-hs-core   # pure core only (no HTTP dependencies)
  - langchain-hs-graph  # graph engine only
```

### Cabal
```bash
cabal install langchain-hs
```

---

## Development & Quality Commands

The repository enforces strict code quality and formatting via `make`:

```bash
# Build the entire monorepo and all 41 example executables
stack build

# Run unit and property-based test suites
stack test

# Run HLint across all source trees (zero hints policy)
make lint

# Check code formatting with Fourmolu
make format-check

# Format all files in-place
make format

# Build the documentation website (Hakyll)
make site-build

# Run live documentation server with auto-reload (port 8000)
make site-watch
```

---

## Documentation & Research

| Resource | Description |
|:---|:---|
| **[Hackage Docs](https://hackage.haskell.org/package/langchain-hs)** | Full Haddock API reference for all exported modules |
| **[Whitepaper](whitepaper.md)** | Deep technical dive: category theory foundations, algebraic laws, effect-polymorphic design, and advanced multi-agent patterns |
| **[Documentation Website](site/)** | Hakyll site with 20 component pages, live provider toggle, and instant search (`Cmd+K`) |
| **[Examples](examples/)** | 41 runnable executables covering every component for Ollama and OpenAI |

To build the Haddock API docs locally:
```bash
make docs
# Opens in .stack-work/install/.../doc/index.html
```

---

## License

Distributed under the **MIT License**. See [LICENSE](LICENSE) for details.

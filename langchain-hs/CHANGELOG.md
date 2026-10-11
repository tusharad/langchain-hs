# Changelog for `langchain-hs`

## 0.0.6.0 - 2026-10-02

### Modular Provider Decoupling & Lightweight Core

- **Modular Provider Split**:
  - `langchain-hs` is now completely lightweight. All concrete AI provider integrations (`Ollama`, `OpenAI`, `Gemini`) and protocol clients (`MCP`) have been decoupled into standalone sub-packages.
  - **`langchain-hs-ollama` (0.0.6.0)**: Dedicated Ollama chat model and embeddings integration via `ollama-haskell`.
  - **`langchain-hs-openai` (0.0.6.0)**: Dedicated OpenAI & OpenAI-compatible provider (OpenRouter, Together, Fireworks), streaming, structured outputs, and embeddings integration via `openai`.
  - **`langchain-hs-gemini` (0.0.6.0)**: Dedicated Google Gemini provider with function calling and SSE streaming.
  - **`langchain-hs-mcp` (0.0.6.0)**: Standalone Model Context Protocol client over stdio and HTTP JSON-RPC 2.0.
  - **`langchain-hs` (0.0.6.0)**: Core framework containing agents (`ReAct`, `PlanAndExecute`), output parsers, vector stores (`InMemory`, `SqliteVec`), chains (`RetrievalQA`, `MapReduce`), memory, resilience (`CircuitBreaker`, `Retry`), and observability (`OpenTelemetry`, `InMemoryLogger`).
- **Cleaned Dependencies & Build Plans**:
  - Completely removed direct dependencies on `ollama-haskell`, `openai`, `servant`, `req` from `langchain-hs`.
  - Decoupled `withStructuredOutput` in `langchain-hs-openai` using `Langchain.OutputParser.Structured` directly, eliminating any transitive dependency on `ollama-haskell`.
  - Purged obsolete extra-deps across all Stack and LTS configurations (`pdf-toolbox-document`, `pdf-toolbox-core`, `pdf-toolbox-content`, `cryptohash`, `cryptonite`, `ginger`).
- **Independent Package Testing**:
  - Every sub-package now has its own isolated `stack.yaml` and test suite that builds and executes independently.
  - Verified across GHC 9.4 through GHC 9.12 (LTS 21.25, 22.44, 23.28, 24.12, 24.56, Nightly).

### ⚠️ Breaking Changes from 0.0.5.0

- Provider modules are no longer re-exported from `langchain-hs`:
  - For Ollama: Add `langchain-hs-ollama` to dependencies and `import Langchain.Provider.Ollama` / `import Langchain.Embeddings.Ollama`.
  - For OpenAI: Add `langchain-hs-openai` to dependencies and `import Langchain.Provider.OpenAI` / `import Langchain.Embeddings.OpenAI`.
  - For Gemini: Add `langchain-hs-gemini` to dependencies and `import Langchain.Provider.Gemini`.
  - For MCP: Add `langchain-hs-mcp` to dependencies and `import Langchain.MCP.Client`.
- `Langchain.Prelude` no longer re-exports provider-specific constructors (`newOllama`, `newOpenAI`, `newGemini`, `newStdioMcpClient`). Import the corresponding provider package.

## 0.0.5.0 - 2026-09-10

### Major Architecture & Ecosystem Evolution

- **3-Tier Monorepo Architecture**:
  - `langchain-hs-core` (0.0.5.0): Zero-dependency pure core with `RunnableTree`, `ChatModel`, `ContentBlock`, `Tool`, `StreamEvent`, and `LangchainT`.
  - `langchain-hs-graph` (0.0.5.0): Graph-based state machine engine with `StateGraph`, `StateReducer`, checkpointers, HITL, and multi-agent coordination.
  - `langchain-hs` (0.0.5.0): Production integrations for Ollama, OpenAI, Gemini, Vector Stores, MCP, and Observability.
- **Pure AST Pipelines (`RunnableTree`)**:
  - The core selling point: Every component implements the `Runnable` typeclass.
  - Compose pure GADT abstract syntax trees using `|>>` (sequential composition), `&>&` (parallel fan-out), and `>>>#` (fallback failover) without side effects before interpretation.
- **LangGraph in Haskell (`StateGraph`)**:
  - Cyclic state machines with pure state reducers (`StateReducer s`) satisfying monoid associativity laws.
  - Thread-safe STM in-memory checkpointer (`MemoryCheckpointer`) and persistent `SQLiteCheckpointer`.
  - First-class Human-in-the-Loop (`HITL`) node interruption, inspect/edit state, and resumption via `resumeGraph`.
  - Time-travel state replay and Graphviz DOT visualization export.
- **Model Context Protocol (MCP)**:
  - Native stdio and HTTP JSON-RPC 2.0 client implementation.
  - Dynamic tool inspection and bidirectional schema mapping to Haskell `Tool` definitions.
- **Decoupled Monad Transformer (`LangchainT env m a`)**:
  - Removed redundant global config structs in favor of parameterization over custom user environment `env`.
  - Complete `MonadReader`, `MonadError`, `MonadIO`, and `MonadTrans` instances.
  - OpenTelemetry distributed tracing spans (`withSpan`) and structured JSON telemetry.
  - Three-state Circuit Breaker, exponential backoff retries with randomized jitter, and in-memory caching.
- **Dependency & Performance Upgrades**:
  - Upgraded to `ollama-haskell` `0.4.1.0` with JSON schema grammar constraints.
  - Migrated to `MercuryTechnologies/openai` client.
  - Full PVP upper bounds across all packages for Hackage compliance.

### ⚠️ Breaking Changes from 0.0.3.0

- **`LangchainT` is now parameterized over `env`** (`LangchainT env m a` instead of the former implicit `LangchainConfig`).
  - Replace `runLangchainT config action` with `runLangchainT env action` where `env` is your custom environment type (use `()` if you have no shared config).
  - The `MonadReader env (LangchainT env m)` instance gives you `ask`/`asks` to read your environment from within the monad.
- **`ChatMessage` renamed to `Message`** throughout — update all pattern matches and constructor calls.
- **Agent modules restructured** — `Langchain.Agent` is now split into `Langchain.Agent.ReAct` and `Langchain.Agent.PlanAndExecute` with updated type signatures for tool-call support.
- **Removed `HtmlLoader`, `JsonLoader`, and `WebPageLoader` from `Langchain.DocumentLoader`** — Along with the `tagsoup` dependency. Users can write custom document loaders tailored to their schemas and formats.

## 0.0.3.0 - 2025-11-16

### Added

- Added Agent middleware support.
- Added image input support for OpenAI and Gemini.

### Fixed

- Fixed Ollama stream's onComplete callback.
- Fixed system_fingerprint field to be nullable.

### Changed

- * Revamped Agent module with native tool_call support and improved architecture.
- * Migrated to Mercury's OpenAI client.
- * Changed Ollama params to use ChatOps type.
- * Renamed ChatMessage to ChatHistory.
- * Parameterized StreamHandler Token type.
- * Added Langchain error type for better error handling.
- Updated ollama-haskell to 0.2.1.0.

## 0.0.2.0 - 2025-05-04

### Added

- Added `OpenAI` LLM integration.
- Added `DirectoryLoader` for loading Documents from a directory.
- Added `HuggingFace` LLM integration.
- Added docusaurus documentation.
- Added `OpenAI` embeddings integration.
- Added GHC CI matrix build.
- Added `TokenBufferMemory` Memory integration.
- Added `RetrievalQA` chain.
- Added `CalculatorTool` tool.

### Fixed 

- Fixed `loadAndSplit` function for `PdfLoader`.
- Minor documentation fixes.
- Fixed `WebScraper` to only scrape textual content.
- Made langchain-hs buildable till stack-lts-19.33
- Fixed `React` agent.

### Changed

- Generalized LLMParams to accept different type per LLM. 

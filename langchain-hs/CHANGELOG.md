# Changelog for `langchain-hs`

## 0.0.6.0 - 2026-10-15

### Highlights

- **Complete Modular Provider Architecture**:
  All concrete AI providers and protocol clients are now decoupled into standalone Hackage packages:
  - `langchain-hs-core` (0.0.6.0): Core abstractions, runnables, tools, agents, vector stores, memory, output parsers, and resilience.
  - `langchain-hs-graph` (0.0.6.0): Production StateGraph engine with state reducers, checkpointing, and time-travel replay.
  - `langchain-hs-ollama` (0.0.6.0): Ollama chat model and embeddings provider.
  - `langchain-hs-openai` (0.0.6.0): OpenAI, OpenRouter, and OpenAI-compatible provider with structured output and streaming.
  - `langchain-hs-gemini` (0.0.6.0): Google Gemini provider with function calling and SSE streaming.
  - `langchain-hs-mcp` (0.0.6.0): Model Context Protocol (MCP) client over stdio and HTTP JSON-RPC 2.0.
  - `langchain-hs` (0.0.6.0): Umbrella package re-exporting `langchain-hs-core` and `langchain-hs-graph`.

### StateGraph Productionization (`langchain-hs-graph`)

- Replaced prototype modules with robust execution combinators: `runGraph`, `runGraphWithConfig`, `runGraphInterleaved`, and `streamGraphSteps`.
- Added `runnableNode` and `chatModelNode` combinators to lift arbitrary `Runnable` and `ChatModel` instances into graph nodes.
- Full support for recursion limits, step-by-step transition logging, checkpointing, and human-in-the-loop interruption.

### Memory Hardening (`langchain-hs-core`)

- **`InVariableMemory`**: Thread-safe STM-backed transactional memory with snapshotting, resets, and clear semantics.
- **`SessionMemory`**: Multi-session conversation manager supporting per-session state isolation, active session tracking, and TTL-based pruning.
- **`FileMemory`**: Persistent file-backed conversation memory with atomic disk writes and metadata preservation across process restarts.
- **Reset Helpers**: Added explicit seed-state reset combinators `resetConversationBuffer`, `resetWindowBuffer`, `resetTokenBuffer`, and `resetEntitiesToInitial`.
- **Umbrella Prelude Re-exports**: Re-exported all memory implementations (`ConversationBufferMemory`, `SessionMemoryStore`, `EntityMemory`, `CombinedMemory`), buffer reset helpers, and `splitDocuments` in `Langchain.Prelude`.

### Output Parsers & Document Loaders

- Trimmed fragile character-scanning parsers (`NumberSeparatedList`) and redundant wrapper types (`JSONOutputStructure`) in favor of type-safe `StructuredOutput` and `TextOutputParser`.
- Hardened boolean output parser to exact normalized token recognition.
- Added standard numeric `OutputParser` instances for `Int`, `Integer`, and `Double` with whitespace stripping.
- Eliminated lazy I/O file handle leaks in `FileLoader` and `DirectoryLoader` with strict text I/O.
- Added `splitDocuments` preserving document metadata across chunk splits, and provided default `loadAndSplit` implementation in `BaseLoader`.

### Breaking Changes from 0.0.5.0

- Provider modules are no longer bundled in `langchain-hs`:
  - For Ollama: Add `langchain-hs-ollama` to dependencies and `import Langchain.Provider.Ollama`.
  - For OpenAI: Add `langchain-hs-openai` to dependencies and `import Langchain.Provider.OpenAI`.
  - For Gemini: Add `langchain-hs-gemini` to dependencies and `import Langchain.Provider.Gemini`.
  - For MCP: Add `langchain-hs-mcp` to dependencies and `import Langchain.MCP.Client`.
- `Langchain.Prelude` no longer re-exports provider-specific constructors (`newOllama`, `newOpenAI`, `newGemini`, `newStdioMcpClient`). Import the corresponding provider package.
- `JSONOutputStructure` and `NumberSeparatedList` have been removed from `Langchain.OutputParser.Core` and `Langchain.Prelude`. Use `StructuredOutput` or `TextOutputParser`.

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

### Breaking Changes from 0.0.3.0

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

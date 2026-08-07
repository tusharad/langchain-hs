# Product Requirements Document (PRD)

## Project: `langchain-hs` — A Functional-First AI Framework for Haskell

- **Status**: Active / Milestones 0, 1, 2, 3, 4 COMPLETED & VERIFIED (202/202 Tests Passing)
- **Author**: Tushar Adhatrao (Maintainer)
- **PRD Version**: 2.5.0
- **Framework Version Target**: `0.5.0` → `1.0.0`
- **GHC Support**: 9.4, 9.6, 9.8, 9.10, 9.12

---

## 1. Vision & North Star

`langchain-hs` exists to answer a single, uncompromising question:

> **What does an AI application framework look like when the language refuses to let you write bugs?**

Every other AI framework—Python's LangChain, Elixir's LangChain, Rust's langchain-rs, Java's LangChain4j, Go's LangChainGo—was built to mirror Python's LangChain API in another language. They carry the original sin: they are **LLM wrappers dressed as frameworks**. Their abstractions leak, their types lie, their pipelines fail at runtime, and their streaming is bolted on as callbacks.

`langchain-hs` v2 is **not a port**. It is a rethink. It is what a functional-programming-first AI framework looks like — built on Haskell's type system, not against it. It will be **demonstrably superior** across the following axes:

1. **Correctness**: Pipeline type errors are compile errors.
2. **Composability**: Pipelines are algebraic values, not executable side-effects.
3. **Observability**: Streaming is a first-class Conduit stream, not a callback soup.
4. **Safety**: No unhandled exceptions, no leaked handles, no race conditions.
5. **Power**: State-graph agents with checkpointing, time-travel, and HITL — as a pure library.

---

## 2. Empirical State-of-the-Art Audit

The following table is derived from **direct source code inspection** of all 6 competing implementations residing in this workspace.

### Status Legend

| Symbol | Meaning |
|--------|---------|
| FULL | Native, production-grade. Complete abstractions, safety guarantees, edge-case coverage. |
| PARTIAL | Present but incomplete; missing key safety, generality, or composability. |
| MISSING | Entirely absent from the implementation. |

---

### Cross-Implementation Feature Matrix

| Feature | Python | Elixir | Rust | Java | Go | **Haskell v1** | **Haskell v2 (target)** |
|:---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| **Core Architecture** | | | | | | | |
| Monad/Effect-polymorphic API | MISSING | PARTIAL | PARTIAL | PARTIAL | PARTIAL | MISSING | **FULL** |
| Pure pipeline AST (compose without executing) | MISSING | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| Compile-time I/O type safety across pipelines | MISSING | MISSING | PARTIAL | PARTIAL | MISSING | PARTIAL | **FULL** |
| Structured error hierarchy (not `String`/exceptions) | PARTIAL | MISSING | PARTIAL | PARTIAL | MISSING | FULL | **FULL** |
| **Chat Model Layer** | | | | | | | |
| Unified `ChatModel` typeclass | FULL | PARTIAL | FULL | FULL | FULL | FULL | **FULL** |
| Multi-modal content blocks (text, image, audio) | FULL | PARTIAL | PARTIAL | FULL | PARTIAL | MISSING | **FULL** |
| Tool/function calling with JSON Schema | FULL | PARTIAL | PARTIAL | FULL | PARTIAL | PARTIAL | **FULL** |
| Reasoning / thinking tokens | FULL | MISSING | MISSING | PARTIAL | MISSING | PARTIAL | **FULL** |
| OpenAI / Anthropic / Gemini / Ollama / DeepSeek | FULL | PARTIAL | PARTIAL | FULL | FULL | FULL | **FULL** |
| **Pipeline Composition (LCEL)** | | | | | | | |
| Sequential composition | FULL | MISSING | PARTIAL | PARTIAL | PARTIAL | PARTIAL | **FULL** |
| Parallel composition | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| Conditional branching | FULL | MISSING | MISSING | MISSING | MISSING | PARTIAL | **FULL** |
| Fallback chains | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| Lambda/pure transforms in pipelines | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| Pipeline as inspectable pure value (not IO) | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| **Streaming** | | | | | | | |
| Structured event protocol (astream_events) | FULL | MISSING | MISSING | PARTIAL | MISSING | MISSING | **FULL** |
| Resource-safe (backpressure, cancellation) | PARTIAL | FULL | FULL | PARTIAL | FULL | MISSING | **FULL** |
| Token streaming | FULL | PARTIAL | FULL | FULL | FULL | FULL | **FULL** |
| Tool call streaming deltas | FULL | MISSING | MISSING | PARTIAL | MISSING | MISSING | **FULL** |
| **Tool System** | | | | | | | |
| Auto JSON Schema derivation from types | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| Type-safe tool input parsing | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| Dynamic tool dispatch (heterogeneous list) | FULL | PARTIAL | PARTIAL | FULL | PARTIAL | PARTIAL | **FULL** |
| Built-in tools (search, calculator, scraper) | FULL | MISSING | PARTIAL | FULL | PARTIAL | FULL | **FULL** |
| **Memory** | | | | | | | |
| Window buffer memory | FULL | FULL | PARTIAL | FULL | FULL | FULL | **FULL** |
| Token-bounded memory | FULL | MISSING | MISSING | FULL | MISSING | FULL | **FULL** |
| Summary memory | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| STM-safe concurrent memory | MISSING | FULL | FULL | PARTIAL | FULL | MISSING | **FULL** |
| **Agent Framework** | | | | | | | |
| ReAct agent pattern | FULL | MISSING | PARTIAL | FULL | FULL | FULL | **FULL** |
| Plan-and-Execute agent | FULL | MISSING | MISSING | PARTIAL | MISSING | MISSING | **FULL** |
| State-graph engine (LangGraph-style) | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| Durable checkpointing | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| Human-in-the-loop (HITL) interrupt/resume | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| Agent middleware/hooks | FULL | MISSING | MISSING | FULL | PARTIAL | FULL | **FULL** |
| **RAG Pipeline** | | | | | | | |
| Document loaders (text, PDF, HTML, MD, JSON) | FULL | PARTIAL | PARTIAL | FULL | PARTIAL | FULL | **FULL** |
| Recursive character splitter | FULL | MISSING | PARTIAL | FULL | PARTIAL | MISSING | **FULL** |
| Token-aware splitter | FULL | MISSING | FULL | FULL | MISSING | MISSING | **FULL** |
| In-memory vector store | FULL | PARTIAL | PARTIAL | FULL | PARTIAL | PARTIAL | **FULL** |
| External vector DBs (pgvector, Qdrant, Pinecone) | FULL | MISSING | PARTIAL | FULL | FULL | MISSING | **FULL** |
| Hybrid search (dense + sparse) | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| Multi-query retriever | FULL | MISSING | MISSING | FULL | MISSING | FULL | **FULL** |
| Contextual compression retriever | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| **Output Parsing** | | | | | | | |
| JSON structured output | FULL | PARTIAL | PARTIAL | FULL | PARTIAL | PARTIAL | **FULL** |
| Auto-retry on parse failure | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| Schema-validated structured output | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| **Prompt Engineering** | | | | | | | |
| Prompt templates (variable substitution) | FULL | FULL | FULL | FULL | FULL | FULL | **FULL** |
| Few-shot prompt templates | FULL | MISSING | MISSING | FULL | MISSING | PARTIAL | **FULL** |
| Chat prompt templates (multi-role) | FULL | PARTIAL | PARTIAL | FULL | PARTIAL | MISSING | **FULL** |
| Prompt composition operators | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| **Observability** | | | | | | | |
| OpenTelemetry tracing | FULL | PARTIAL | MISSING | FULL | MISSING | MISSING | **FULL** |
| Token usage metrics | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| LangSmith integration | FULL | MISSING | MISSING | MISSING | MISSING | MISSING | **FULL** |
| Structured event system | FULL | PARTIAL | MISSING | FULL | MISSING | PARTIAL | **FULL** |
| **Package Architecture** | | | | | | | |
| Modular split packages | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |
| Core / providers / graph / RAG separation | FULL | MISSING | MISSING | FULL | MISSING | MISSING | **FULL** |

---

## 3. Critical Deficiencies in `langchain-hs` v1

This section documents **concrete, code-level** deficiencies found in the current codebase.

### 3.1 Hardcoded `IO` — The Root Problem

Every typeclass method in v1 is pinned to concrete `IO`:

```haskell
-- v1: forces IO everywhere — no effect polymorphism
class LLM llm where
  generate :: llm -> Text -> Maybe (LLMParams llm) -> IO (LangchainResult Text)
  chat :: llm -> ChatHistory -> Maybe (LLMParams llm) -> IO (LangchainResult Message)
```

This makes the library **untestable without real I/O**, impossible to use in `MonadIO`-polymorphic stacks, and resistant to effect systems (`effectful`, `polysemy`, `mtl`).

```haskell
-- v2: monad-polymorphic
class ChatModel model where
  type ModelConfig model :: *
  invoke :: (MonadIO m, MonadError LangchainError m)
         => model -> [Message] -> Maybe (ModelConfig model) -> m Message
```

### 3.2 `Runnable` Executes Eagerly — No Pipeline AST

The current `(|>>)` operator **runs IO immediately** when composing — it is not algebraic:

```haskell
-- v1: executes immediately — pipeline cannot be inspected or serialized
chain :: (...) => r1 -> r2 -> RunnableInput r1 -> IO (Either LangchainError (RunnableOutput r2))
(|>>) = chain  -- NOT a pipeline builder; an executor
```

Python's LangChain builds a `RunnableSequence` AST and executes later. `langchain-hs` v2 must do the same.

### 3.3 `Callback` System Has Only 3 Events — Grossly Inadequate

```haskell
-- v1: 3 events, String errors, no tool/chain events, no metadata
data Event = LLMStart | LLMEnd | LLMError String
type Callback = Event -> IO ()
```

Python's `astream_events` protocol has 10+ event types with rich metadata (`runId`, `modelName`, `tokenUsage`, `toolArgs`). v2 must implement a full `StreamEvent` ADT backed by `Conduit`.

### 3.4 `Tool` Has No JSON Schema — Tool Calling Fundamentally Broken

```haskell
-- v1: no schema derivation — LLM cannot know how to call the tool
class Tool a where
  type Input a
  type Output a
  runTool :: a -> Input a -> IO (Output a)  -- raw Haskell types, no JSON
```

The `ToolAcceptingToolCall` wrapper requiring `Input t ~ ToolCall` in `Agent.Core` is a workaround — it forces the tool to accept the raw LLM output instead of typed arguments. This is the wrong abstraction.

### 3.5 `Memory` is Not Thread-Safe

`WindowBufferMemory` stores state in a plain Haskell record. State updates return a new value requiring manual threading. Concurrent sessions on the same memory object have no consistency guarantees. No `TVar`, no `STM`, no `IORef`.

### 3.6 `Agent` Loop is an Unstructured Recursive `IO` Function

`AgentExecutor` is a recursive `IO` function with a manual counter. There is no:
- State graph with named typed nodes and edges
- Persistent checkpointing between turns
- Human-in-the-loop interrupt/resume
- Sub-graph delegation or parallel sub-agents

### 3.7 `Message.content` is `Text` — No Multimodal Support

Despite Claude, GPT-4o, and Gemini supporting multimodal content, messages in v1 use `content :: Text`. The `messageImages :: Maybe [Text]` workaround in `MessageData` does not compose with content blocks.

### 3.8 Error Construction Silently Ignores Parameters

All error constructors drop their `_model` and `_operation` parameters:

```haskell
-- v1: arguments named with _ prefix — silently ignored
llmError :: Text -> Maybe Text -> Maybe Text -> LangchainError
llmError msg _model _operation =
  LangchainError { errorMessage = msg, errorContext = Nothing, ... }
```

This defeats the purpose of having those parameters in the API.

### 3.9 Full Missing Features List

- No `RunnableParallel` (n-ary parallel composition)
- No `FewShotPromptTemplate` as `Runnable` (commented out)
- No `SummaryMemory`
- No `ContextualCompressionRetriever`
- No external vector stores (only in-memory)
- No `RecursiveCharacterTextSplitter`
- No `TokenTextSplitter`
- No OpenTelemetry tracing
- No `with_structured_output` pattern
- No `ChatPromptTemplate` (multi-role)
- No `Plan-and-Execute` agent
- No `EnsembleRetriever`
- No `ParentDocumentRetriever`
- No `PgvectorStore` / `QdrantStore`
- No `MonadTracer`
- No LangSmith exporter

---

## 4. Design Principles for `langchain-hs` v2

These are **non-negotiable** principles that guide every architectural decision.

### P1. Functional-First, Not LangChain-First

`langchain-hs` is not a port of Python's LangChain. It is a Haskell framework for the same problem domain. When Python's API conflicts with Haskell idioms, the Haskell way wins every time.

### P2. Types Are Documentation

Every public function's type must be self-documenting. No `IO (Either String a)`. No `Dynamic`. No `Any`. The type checker is the first line of documentation.

### P3. Algebras Before Side Effects

Pipelines, agents, and graphs are **pure algebraic values** until explicitly executed. Composition operators build data structures; execution functions interpret them.

### P4. Effect Polymorphism

No concrete `IO` in typeclasses. All typeclasses are parameterized over `m` with appropriate constraints (`MonadIO m`, `MonadError LangchainError m`, `MonadUnliftIO m`).

### P5. Resource Safety Is Non-Negotiable

All HTTP connections, file handles, and streaming contexts MUST be managed with `ResourceT` and `bracket` patterns. No resource leaks under any error path.

### P6. Streaming Is a First-Class Citizen

Streaming responses are `Conduit` streams of typed `StreamEvent` values with full lifecycle events, backpressure, and cancellation. Not callbacks. Not token-by-token `IO ()` functions.

### P7. Zero Implicit Global State

The framework works with zero environment variables or global mutable state. All configuration is explicit, passed as values.

---

## 5. Functional Requirements

### FR-CORE: Core Module (`langchain-hs-core`)

#### FR-CORE-1: Effect-Polymorphic `ChatModel` Typeclass
- `ChatModel` MUST be parameterized over `m` with `(MonadIO m, MonadError LangchainError m)`
- Methods: `invoke`, `batch`, `stream`
- `ModelConfig` associated type for per-provider configuration
- `generate` convenience wrapper for single-turn text completion

#### FR-CORE-2: Multi-Modal Message Model
- `ContentBlock` sum type: `TextBlock Text`, `ImageBlock MimeType Base64`, `AudioBlock MimeType Base64`, `DataBlock ByteString`
- `Message` replaces `content :: Text` with `contents :: NonEmpty ContentBlock`
- `Role` includes `System | User | Assistant | Tool | Developer | Function`
- `ToolCall` arguments as `Value` (not `Map Text Value`)
- All types derive `ToJSON`, `FromJSON`, `Generic`, `Eq`, `Show`

#### FR-CORE-3: Pure Pipeline GADT (`RunnableTree`)
- `RunnableTree m i o` is a **pure GADT**: `Id | Prim | Lambda | Seq | Par | Branch | Fallback`
- `(|>>)` builds a `Seq` node — pure, no IO
- `(&>&)` builds a `Par` node — parallel composition
- `interpret :: RunnableTree m i o -> i -> m (Either LangchainError o)` — sole execution point
- Pipeline AST is inspectable (foldable for visualization/serialization)
- `withFallback` wraps two trees, tries first, falls to second on any error

#### FR-CORE-4: Standardized Streaming Event Protocol
- `StreamEvent` ADT: `LLMStart | LLMChunk | LLMEnd | ToolStart | ToolEnd | ToolError | ChainStart | ChainEnd | NodeStart | NodeEnd`
- Each event carries `runId :: Text` for correlation
- `LLMEnd` carries `TokenUsage { promptTokens, completionTokens, totalTokens }`
- `EventStream m = ConduitT () StreamEvent m ()` — canonical stream type
- All streaming models emit `LLMStart`, one or more `LLMChunk`, then `LLMEnd`
- Backpressure and cancellation via `Conduit` resource management

#### FR-CORE-5: Structured Error Hierarchy
- Retain and enhance `LangchainError` with `errorCategory`, `errorSeverity`, `errorContext`, `errorCause`
- Fix: error construction helpers MUST NOT ignore `_model` and `_operation` parameters
- Add `MonadError LangchainError m` constraint to all fallible typeclasses
- `isRetryable :: LangchainError -> Bool` properly implemented

#### FR-CORE-6: Type-Safe Tool System with Auto Schema
- `IsTool` typeclass: `ToolInput t`, `ToolOutput t`, `toolName`, `toolDescription`, `toolSchema :: Value`, `executeTool`
- `toolSchema` automatically derivable for any type with `Generic + ToJSON + SchemaGen`
- `DynamicTool m` wrapper for runtime heterogeneous tool dispatch
- `mkTool :: IsTool t => t -> DynamicTool m` smart constructor
- `ToolRegistry m` — named registry with O(log n) lookup by name
- Tool execution returns `Either LangchainError Value` for LLM integration

#### FR-CORE-7: Memory with STM Safety
- `BaseMemory mem m` typeclass parameterized over `m`
- `WindowBufferMemory` backed by `TVar` — thread-safe
- `SummaryMemory` — LLM-compressed history when window exceeds threshold
- `VectorRetrieverMemory` — semantic similarity-based context retrieval
- `clear`, `addMessage`, `getMessages` — all polymorphic over `m`

#### FR-CORE-8: Prompt Engineering
- `PromptTemplate` — retain current implementation
- `ChatPromptTemplate` — multi-role templates producing `[Message]`
- `FewShotPromptTemplate` — complete `Runnable` instance (fix commented-out code)
- `MessagePlaceholder` — variable substitution within message sequences
- Prompt composition: `(<>) :: PromptTemplate -> PromptTemplate -> PromptTemplate`

---

### FR-PROVIDERS: Provider Module (`langchain-hs-providers`)

#### FR-PROV-1: OpenAI
- Chat completions with tool calling, streaming, structured output
- Embeddings API (`text-embedding-3-small`, `text-embedding-3-large`)
- Reasoning models (`o1`, `o3`) with thinking token support
- `OpenAICompatible` reuse for OpenRouter, Together AI, Fireworks

#### FR-PROV-2: Anthropic
- Messages API with extended thinking support
- Tool use (function calling), vision (base64 image blocks)
- Streaming with tool call deltas

#### FR-PROV-3: Google Gemini
- Chat + Generate Content APIs, multimodal (text, image, audio, video)
- Function calling, embeddings (`text-embedding-004`)

#### FR-PROV-4: Ollama
- Full integration via `ollama-haskell` library
- Local model management (list, pull, delete)
- Streaming with proper `Conduit` adapter, embeddings

#### FR-PROV-5: DeepSeek
- Chat completions with R1 reasoning model, thinking chain extraction

#### FR-PROV-6: HuggingFace
- Text generation and embeddings via Inference API

---

### FR-GRAPH: State-Graph Agent Engine (`langchain-hs-graph`)

#### FR-GRAPH-1: Core `StateGraph` Engine
- `StateGraph s m` — directed cyclic graph with typed state `s` and monad `m`
- `NodeId` — opaque identifier for graph nodes
- `Node s m` — `runNode :: s -> m (Either LangchainError s)`
- `Edge s m` — `StaticEdge NodeId | ConditionalEdge (s -> m (Either LangchainError NodeId))`
- `compileGraph` validates at construction time (no dangling edges, valid entry/exit)
- `runGraph :: CompiledGraph s m -> s -> m (Either LangchainError s)`

#### FR-GRAPH-2: State Reducers
- `StateReducer s = s -> s -> s` — pure function merging partial state updates
- Default reducers: `appendMessages`, `replaceField`, `mergeMaps`

#### FR-GRAPH-3: Checkpointing
- `Checkpointer cp m` typeclass: `saveCheckpoint`, `loadCheckpoint`, `listCheckpoints`
- `MemoryCheckpointer` — in-memory with `TVar`
- `SQLiteCheckpointer` — persistent using `sqlite-simple`
- Thread-safe, checkpoint-per-thread-id

#### FR-GRAPH-4: Human-in-the-Loop (HITL)
- `interrupt :: m ()` — suspends graph at current node, persists state
- `resumeGraph` — restores from checkpoint and continues execution
- `HITLNode NodeId` — marks a node as an interrupt point

#### FR-GRAPH-5: Multi-Agent Patterns
- `Supervisor` node — routes to sub-agents based on task type
- `SubGraph` node — embed one `StateGraph` inside another
- `Send` command — dynamic fan-out to parallel sub-agents

---

### FR-RAG: RAG & Document Processing (`langchain-hs-rag`)

#### FR-RAG-1: Document Loaders
- `DocumentLoader` typeclass: `loadDocuments :: loader -> m (Either LangchainError [Document])`
- Implementations: `FileLoader`, `DirectoryLoader`, `PDFLoader`, `HTMLLoader`, `MarkdownLoader`, `JSONLoader`, `CSVLoader`
- `Document` with `pageContent :: Text` and `metadata :: Map Text Value`

#### FR-RAG-2: Text Splitters
- `TextSplitter` typeclass
- `CharacterTextSplitter` — retain current
- `RecursiveCharacterTextSplitter` — language-specific separator priority list
- `TokenTextSplitter` — split by token count
- `MarkdownHeaderSplitter` — split by header hierarchy

#### FR-RAG-3: Embeddings
- `Embeddings` typeclass: `embedDocuments`, `embedQuery`
- Implementations: `OpenAIEmbeddings`, `GeminiEmbeddings`, `OllamaEmbeddings`, `HuggingFaceEmbeddings`

#### FR-RAG-4: Vector Stores
- `VectorStore vs` typeclass: `addDocuments`, `similaritySearch`, `similaritySearchByVector`, `maxMarginalRelevanceSearch`, `delete`
- `InMemoryVectorStore` — enhanced with HNSW
- `PgvectorStore` — PostgreSQL pgvector
- `QdrantStore` — Qdrant vector database

#### FR-RAG-5: Retrievers
- `Retriever r` typeclass: `getRelevantDocuments :: r -> Text -> m [Document]`
- `VectorStoreRetriever`, `MultiQueryRetriever`, `ContextualCompressionRetriever`
- `EnsembleRetriever` — weighted multi-retriever combination
- `ParentDocumentRetriever` — small chunk retrieval, parent doc return

---

### FR-OBS: Observability

- `MonadTracer m` typeclass: `withSpan`, `addAttribute`, `recordException`
- `Span` with `traceId`, `spanId`, `parentSpanId`, `startTime`, `endTime`, `attributes`
- `NoOpTracer` — zero-cost default
- `OpenTelemetryTracer` — exports to OTLP collectors
- `LangSmithTracer` — exports run trees to LangSmith

---

## 6. Non-Functional Requirements

| ID | Category | Requirement |
|----|----------|-------------|
| NFR-1 | Reliability | Zero runtime panics. All errors in `LangchainError` or `MonadError`. |
| NFR-2 | Performance | HTTP connections reused via `Manager`. No per-call connection setup. |
| NFR-3 | Memory | `ResourceT` for all streaming contexts. No handle leaks. |
| NFR-4 | Concurrency | `STM` for shared state. `async` for parallel invocations. |
| NFR-5 | Documentation | Every public function has Haddock with example and type explanation. |
| NFR-6 | Testing | 80%+ unit test coverage. Integration tests gated behind env flag. |
| NFR-7 | Compatibility | GHC 9.4, 9.6, 9.8, 9.10, 9.12. Stack LTS-21 through LTS-24. |
| NFR-8 | Package size | Core package MUST NOT depend on HTTP or provider SDKs. |
| NFR-9 | Ergonomics | 3-line hello world. Complex pipelines readable without documentation. |
| NFR-10 | Hackage | All packages publishable to Hackage with correct bounds. |

---

## 7. Package Architecture

```
langchain-hs (meta / re-export)
├── langchain-hs-core
│   ├── Langchain.Core.Model          -- ChatModel typeclass, Message, ContentBlock
│   ├── Langchain.Core.Runnable       -- RunnableTree GADT, operators
│   ├── Langchain.Core.Tool           -- IsTool, DynamicTool, ToolRegistry
│   ├── Langchain.Core.Memory         -- BaseMemory typeclass
│   ├── Langchain.Core.Stream         -- StreamEvent, EventStream, Conduit types
│   ├── Langchain.Core.Error          -- LangchainError hierarchy
│   ├── Langchain.Core.Prompt         -- PromptTemplate, ChatPromptTemplate
│   ├── Langchain.Core.Retriever      -- Retriever typeclass
│   ├── Langchain.Core.Embeddings     -- Embeddings typeclass
│   └── Langchain.Core.Telemetry      -- MonadTracer, Span, TokenUsage
│
├── langchain-hs-providers
│   ├── Langchain.Provider.OpenAI
│   ├── Langchain.Provider.Anthropic
│   ├── Langchain.Provider.Gemini
│   ├── Langchain.Provider.Ollama
│   ├── Langchain.Provider.DeepSeek
│   └── Langchain.Provider.HuggingFace
│
├── langchain-hs-graph
│   ├── Langchain.Graph.StateGraph
│   ├── Langchain.Graph.Node
│   ├── Langchain.Graph.Edge
│   ├── Langchain.Graph.Checkpointer
│   ├── Langchain.Graph.HITL
│   └── Langchain.Graph.MultiAgent
│
└── langchain-hs-rag
    ├── Langchain.RAG.Loader
    ├── Langchain.RAG.Splitter
    ├── Langchain.RAG.VectorStore
    ├── Langchain.RAG.Retriever
    └── Langchain.RAG.Embeddings
```

---

## 8. Success Metrics

| Metric | v1 Baseline | v2 Target |
|--------|------------|-----------|
| Feature parity vs Python LangChain | ~35% | >90% |
| Compile-time pipeline safety | Partial | Full |
| Concurrent session safety | None | Full (STM) |
| Streaming event types | 3 | 10+ |
| LLM providers | 5 | 7+ |
| Vector store integrations | 1 | 4+ |
| Test coverage | ~40% | >80% |
| Hackage packages | 1 | 5 |
| Public documentation examples | ~30 | >150 |

---

## 9. Out of Scope (v2)

- Browser / WASM compilation target
- GraphQL API server
- Model fine-tuning APIs
- Native code generation (LLVM)
- GUI / visual pipeline editor
- Proprietary model hosting

---

## 10. Guiding Philosophy

> "Make illegal states unrepresentable. Make correct programs easy to write. Make incorrect programs impossible to compile."

`langchain-hs` v2 is the AI framework that Haskell deserves — not a faithful port of Python's design mistakes, but a ground-up implementation that uses the language's full power. When a Haskell developer builds an LLM application with `langchain-hs`, the type checker is their co-pilot, not their adversary.

# Milestones & User Stories

## Project: `langchain-hs` v2

- **Status**: Planning
- **Author**: Tushar Adhatrao (Maintainer)
- **Document Version**: 2.0.0

---

## Overview

The release is broken into 6 milestones, each independently releasable to Hackage. Each milestone builds on the previous. Work within a milestone is organized by user story.

```
M0: Foundation Fixes        (v0.1.0)   — Fix v1 correctness bugs
M1: Core Architecture       (v0.2.0)   — RunnableTree, ChatModel, StreamEvent
M2: Provider Rewrite        (v0.3.0)   — All providers on new ChatModel
M3: Tool System & Output    (v0.4.0)   — IsTool, DynamicTool, structured output
M4: Graph Agent Engine      (v0.5.0)   — StateGraph, HITL, checkpointing
M5: RAG & Observability     (v0.6.0)   — Full RAG, OTEL, LangSmith
M6: Production Hardening    (v1.0.0)   — Perf, docs, benchmarks, Hackage
```

---

## Milestone 0: Foundation Fixes (v0.1.0)

**Goal**: Fix all critical bugs and regressions in the current v1 codebase without breaking the API. Ship a clean, reliable v0.1.0 before starting the rewrite.

**Exit Criteria**: All existing tests pass. Zero `_` prefixed unused parameters. Memory is thread-safe. Error context is populated.

---

### M0-US-1: Fix Silent Error Context Dropping

**As a** library user debugging a failed LLM call,  
**I want** error messages to include the model name and operation context,  
**So that** I can understand what failed without reading source code.

**Acceptance Criteria**:
- `llmError`, `agentError`, `toolError`, and all other error constructors store the `model` and `operation` parameters in `errorContext`
- `ErrorContext.contextTimestamp` is populated automatically from `getCurrentTime`
- `displayException` includes component and operation in its output

**Files Changed**: `src/Langchain/Error.hs`

**Effort**: S (1-2 days)

---

### M0-US-2: Thread-Safe Memory via TVar

**As a** developer running concurrent chat sessions,  
**I want** memory operations to be safe under concurrent access,  
**So that** my multi-user service doesn't corrupt conversation history.

**Acceptance Criteria**:
- `WindowBufferMemory` stores messages in `TVar [Message]`
- `addMessage`, `addUserMessage`, `addAiMessage` use `atomically . modifyTVar'`
- `clear` uses STM to atomically reset
- A concurrent test verifies 100 concurrent writes produce consistent state
- API remains backward compatible (callers don't need to change)

**Files Changed**: `src/Langchain/Memory/Core.hs`

**Effort**: M (2-3 days)

---

### M0-US-3: Complete FewShotPromptTemplate Runnable Instance

**As a** developer building few-shot prompt pipelines,  
**I want** `FewShotPromptTemplate` to work as a `Runnable`,  
**So that** I can compose it with LLMs using `|>>`.

**Acceptance Criteria**:
- The commented-out `instance Runnable FewShotPromptTemplate` is completed
- `RunnableInput FewShotPromptTemplate = Map Text Text` (suffix variables)
- `RunnableOutput FewShotPromptTemplate = Text`
- Unit test: renders correctly with 2 examples and a suffix variable

**Files Changed**: `src/Langchain/PromptTemplate.hs`

**Effort**: S (1 day)

---

### M0-US-4: Fix Tool Schema — Remove `ToolAcceptingToolCall` Workaround Documentation

**As a** developer implementing a custom tool,  
**I want** the documentation to clearly explain the current limitation,  
**So that** I don't build incorrect abstractions on top of a workaround.

**Acceptance Criteria**:
- `Tool.Core` Haddock explains that `ToolAcceptingToolCall` is a temporary bridge
- `Agent.Core` documents that `executeTool` requires `Input t ~ ToolCall`
- A `TODO` linking to M3-US-1 is added in both files

**Files Changed**: `src/Langchain/Tool/Core.hs`, `src/Langchain/Agent/Core.hs`

**Effort**: XS (half day)

---

### M0-US-5: Add Missing `TokenBufferMemory` Test Coverage

**As a** maintainer,  
**I want** `TokenBufferMemory` to have comprehensive tests,  
**So that** I can refactor it safely in M1.

**Acceptance Criteria**:
- Tests cover: trim on token overflow, empty buffer, single message, system message preserved
- Tests run without network access

**Files Changed**: `test/Test/Langchain/Memory/TokenBufferMemory.hs`

**Effort**: S (1 day)

---

## Milestone 1: Core Architecture Rewrite (v0.2.0)

**Goal**: Introduce `langchain-hs-core` package split. Implement `RunnableTree` GADT, effect-polymorphic `ChatModel`, and `StreamEvent` protocol. This is the most architecturally significant milestone.

**Exit Criteria**: `langchain-hs-core` builds standalone. `RunnableTree` passes all property tests. `StreamEvent` is complete.

---

### M1-US-1: Pure Pipeline GADT (`RunnableTree`)

**As a** functional programmer,  
**I want** pipeline composition to be a pure, algebraic operation,  
**So that** I can build, inspect, and serialize pipelines before executing them.

**Acceptance Criteria**:
- `RunnableTree m i o` GADT with constructors: `Id`, `Prim`, `Lambda`, `Seq`, `Par`, `Branch`, `Fallback`
- `(|>>)` builds a `Seq` node — returns `RunnableTree`, no IO
- `(&>&)` builds a `Par` node — parallel composition with `async`
- `interpret :: RunnableTree m i o -> i -> m o` is the sole executor
- Identity laws pass: `interpret (Id |>> t) x == interpret t x`
- Sequential law: `interpret (Seq t1 t2) x == interpret t1 x >>= interpret t2`
- Property tests via QuickCheck for all algebraic laws
- `Par` uses `Control.Concurrent.Async.concurrently` for real parallelism

**New File**: `langchain-hs-core/Langchain/Core/Runnable.hs`

**Effort**: L (5-7 days)

---

### M1-US-2: Effect-Polymorphic `ChatModel` Typeclass

**As a** developer writing testable LLM code,  
**I want** the `ChatModel` typeclass to work with any monad stack,  
**So that** I can write pure unit tests with mock models.

**Acceptance Criteria**:
- `class ChatModel model where ...` parameterized over `m`
- `invoke :: (MonadIO m, MonadError LangchainError m) => model -> [Message] -> Maybe (ModelConfig model) -> m Message`
- `batch` has a default sequential implementation
- `stream :: ... => model -> [Message] -> Maybe (ModelConfig model) -> ConduitT () StreamEvent m ()`
- `MockModel` implementation for testing (always returns a fixed `Message`)
- `MockModel` in `test/` — demonstrates pure unit testing with `runExceptT`

**New File**: `langchain-hs-core/Langchain/Core/Model.hs`

**Effort**: M (3-4 days)

---

### M1-US-3: Multi-Modal Message Model (`ContentBlock`)

**As a** developer building a vision-enabled chatbot,  
**I want** to send image content alongside text in messages,  
**So that** I can use Claude Vision or GPT-4V without building my own message types.

**Acceptance Criteria**:
- `ContentBlock` sum type: `TextBlock Text | ImageBlock Text Text | AudioBlock Text Text | DataBlock ByteString`
- `Message.messageContents :: NonEmpty ContentBlock` replaces `content :: Text`
- `textMessage`, `userMessage`, `systemMessage` convenience constructors
- `imageMessage :: Role -> Text -> Text -> Text -> Message` (role, mimeType, base64, altText)
- Full `ToJSON`/`FromJSON` instances compatible with OpenAI and Anthropic wire formats
- Migration: existing code using `Message { role, content, messageData }` given deprecation warnings pointing to new API

**New File**: `langchain-hs-core/Langchain/Core/Model.hs`

**Effort**: M (3 days)

---

### M1-US-4: Standardized `StreamEvent` Protocol

**As a** developer monitoring LLM token usage,  
**I want** streaming to emit structured events with metadata,  
**So that** I can track costs and debug pipeline execution.

**Acceptance Criteria**:
- `StreamEvent` ADT with all 10 constructors (see TDD §2.4)
- `TokenUsage { promptTokens, completionTokens, totalTokens }` in `LLMEnd`
- `EventStream m = ConduitT () StreamEvent m ()`
- `runId :: Text` on every event for correlation
- Helper: `collectEvents :: EventStream m -> m [StreamEvent]` for testing
- Helper: `printEvents :: EventStream IO -> IO ()` for debugging

**New File**: `langchain-hs-core/Langchain/Core/Stream.hs`

**Effort**: M (3 days)

---

### M1-US-5: Package Split — `langchain-hs-core`

**As a** developer who only needs the core typeclasses without LLM providers,  
**I want** a minimal `langchain-hs-core` package,  
**So that** I can depend on it without pulling in HTTP or provider SDKs.

**Acceptance Criteria**:
- `langchain-hs-core.cabal` with zero HTTP dependencies
- Dependencies: `aeson, base, conduit, containers, mtl, stm, text, time, transformers, unliftio`
- All core typeclasses in `langchain-hs-core`: `ChatModel`, `Runnable`, `IsTool`, `BaseMemory`, `Retriever`, `Embeddings`, `VectorStore`
- `langchain-hs` becomes a meta-package re-exporting everything
- CI matrix includes isolated build of `langchain-hs-core` alone

**New Files**: `langchain-hs-core/langchain-hs-core.cabal`, package.yaml split

**Effort**: L (5 days)

---

## Milestone 2: Provider Rewrite (v0.3.0)

**Goal**: Port all existing LLM providers to the new `ChatModel` typeclass. Add missing providers (Anthropic standalone). Implement `Conduit`-based streaming for all.

**Exit Criteria**: All providers implement `ChatModel`. Streaming emits proper `StreamEvent` sequence.

---

### M2-US-1: OpenAI Provider on `ChatModel`

**As a** developer using GPT-4o,  
**I want** the OpenAI provider to use the new effect-polymorphic API,  
**So that** I can use it with `interpret` and get structured streaming events.

**Acceptance Criteria**:
- `OpenAI` implements `ChatModel` with `MonadIO m, MonadError LangchainError m`
- `stream` emits `LLMStart`, `LLMChunk` (one per token), `LLMEnd` with `TokenUsage`
- Tool calls during streaming emit `LLMChunk` with `toolCallDelta`
- `OpenAICompatible` remains for OpenRouter/Fireworks/Together
- HTTP manager is created once per `OpenAI` instance, not per-call
- Integration test (gated on `OPENAI_API_KEY`) verifies end-to-end

**Files Changed**: `src/Langchain/Provider/OpenAI.hs` (new path)

**Effort**: L (5-7 days)

---

### M2-US-2: Anthropic Provider (New)

**As a** developer using Claude 3.5 Sonnet,  
**I want** a first-class Anthropic provider,  
**So that** I can switch between OpenAI and Anthropic without changing pipeline code.

**Acceptance Criteria**:
- `Anthropic` data type with API key and HTTP manager
- `invoke` calls Messages API with proper request serialization
- `stream` calls streaming Messages API, emits `StreamEvent` sequence
- Extended thinking support: `AnthropicConfig { enableThinking :: Bool, thinkingBudget :: Maybe Int }`
- Vision: `ImageBlock` in `ContentBlock` maps to Anthropic's `image` source type
- Tool use: `ToolCall` maps to Anthropic's `tool_use` content block

**New Files**: `langchain-hs-providers/Langchain/Provider/Anthropic.hs`

**Effort**: L (5-7 days)

---

### M2-US-3: Gemini Provider on `ChatModel`

**As a** developer using Google Gemini,  
**I want** the Gemini provider to implement `ChatModel`,  
**So that** I can use Gemini in any pipeline that accepts a `ChatModel`.

**Acceptance Criteria**:
- `Gemini` implements `ChatModel`
- Multi-modal support: image, audio content blocks mapped to Gemini's `Part` types
- Function calling mapped to Gemini's function declarations format
- Streaming via server-sent events
- `GeminiEmbeddings` implements `Embeddings` typeclass

**Files Changed**: `src/Langchain/Provider/Gemini.hs` (new path)

**Effort**: M (4-5 days)

---

### M2-US-4: Ollama Provider via `ollama-haskell`

**As a** developer running local models,  
**I want** the Ollama provider to leverage `ollama-haskell`'s `OllamaClient`,  
**So that** local LLM usage is as ergonomic as cloud providers.

**Acceptance Criteria**:
- `Ollama` wraps `OllamaClient` from `ollama-haskell`
- `invoke` uses `OllamaClient`'s chat API
- `stream` adapts `ollama-haskell`'s streaming to `EventStream m`
- `OllamaEmbeddings` implements `Embeddings`
- `OllamaConfig` includes model name, context window, seed

**Files Changed**: `src/Langchain/Provider/Ollama.hs` (new path)

**Effort**: M (3-4 days)

---

### M2-US-5: DeepSeek Reasoning Chain Extraction

**As a** developer using DeepSeek R1,  
**I want** thinking chain output to be accessible as a typed field,  
**So that** I can log or display the model's reasoning.

**Acceptance Criteria**:
- `DeepSeek` implements `ChatModel`
- Response includes `<think>...</think>` block extracted to `messageThinking :: Maybe Text`
- `stream` emits thinking tokens as `LLMChunk { chunkText = thinkingToken, isThinking = True }`
- Integration test validates thinking chain is non-empty for R1 model

**Files Changed**: `src/Langchain/Provider/DeepSeek.hs`

**Effort**: S (2-3 days)

---

## Milestone 3: Tool System & Structured Output (v0.4.0)

**Goal**: Implement `IsTool` with auto JSON Schema derivation. Complete `DynamicTool` + `ToolRegistry`. Implement `with_structured_output` pattern. Migrate all built-in tools.

**Exit Criteria**: Any `Generic + ToJSON` type auto-derives its JSON Schema. Tool calls work end-to-end with OpenAI function calling.

---

### M3-US-1: `IsTool` with Auto JSON Schema Derivation

**As a** developer defining a tool,  
**I want** to define a plain Haskell record and have the JSON Schema generated automatically,  
**So that** I don't manually maintain schema documentation that can drift from code.

**Acceptance Criteria**:
- `IsTool` typeclass with `toolSchema :: t -> Value` that defaults via `Generic`
- `deriveToolSchema :: (Generic a, GToSchema (Rep a)) => Proxy a -> Value` utility
- Schema output matches JSON Schema Draft-07 format (compatible with OpenAI)
- Schema includes: `type`, `properties`, `required`, `description` (from Haddock if available)
- `mkTool :: IsTool t => t -> DynamicTool m` works for any `IsTool`
- Unit test: `data WeatherInput = WeatherInput { city :: Text, units :: Text }` produces correct schema

**New File**: `langchain-hs-core/Langchain/Core/Tool.hs`

**Effort**: L (5-7 days)

---

### M3-US-2: `ToolRegistry` and Heterogeneous Tool Dispatch

**As a** developer building an agent with multiple tools,  
**I want** to register tools by name and dispatch to them by name,  
**So that** the agent can call any tool from the LLM's function call response.

**Acceptance Criteria**:
- `ToolRegistry m = Map Text (DynamicTool m)` with `Monoid` instance
- `registerTool :: DynamicTool m -> ToolRegistry m -> ToolRegistry m`
- `lookupTool :: Text -> ToolRegistry m -> Maybe (DynamicTool m)`
- `callTool :: ToolRegistry m -> ToolCall -> m (Either LangchainError Value)`
- `callTool` validates tool name exists, parses arguments, executes, returns `Value`
- Error: `ToolError` with name "unknown_tool" when name not found
- Error: `ParsingError` when argument JSON doesn't match schema

**New File**: `langchain-hs-core/Langchain/Core/Tool.hs`

**Effort**: M (3-4 days)

---

### M3-US-3: Migrate Built-In Tools to `IsTool`

**As a** developer using the built-in calculator or DuckDuckGo tools,  
**I want** them to implement `IsTool` with proper schemas,  
**So that** I can include them in agent tool registries without workarounds.

**Acceptance Criteria**:
- `Calculator`, `DuckDuckGoSearch`, `WikipediaTool`, `WebScraper` all implement `IsTool`
- Each has a typed `ToolInput` record (e.g., `data CalcInput = CalcInput { expression :: Text }`)
- Each has a generated JSON Schema verifiable in unit tests
- `ToolAcceptingToolCall` wrapper removed or deprecated

**Files Changed**: `src/Langchain/Tool/Calculator.hs`, `DuckDuckGo.hs`, `WikipediaTool.hs`, `WebScraper.hs`

**Effort**: M (3-4 days)

---

### M3-US-4: Structured Output (`with_structured_output`)

**As a** developer who wants LLM output as a Haskell type,  
**I want** a `withStructuredOutput` combinator,  
**So that** I don't write boilerplate JSON parsing after every LLM call.

**Acceptance Criteria**:
- `withStructuredOutput :: (ChatModel m, FromJSON a, Generic a) => model -> [Message] -> m (Either LangchainError a)`
- Internally uses JSON mode (OpenAI) or tool calling with a schema-derived single tool
- On parse failure, retries up to 3 times with the parse error in the prompt
- `OutputParser a` typeclass for custom parsing logic
- Unit test: `data PersonInfo = PersonInfo { name :: Text, age :: Int }` parsed from LLM JSON response

**New File**: `langchain-hs-core/Langchain/Core/OutputParser.hs`

**Effort**: M (4 days)

---

### M3-US-5: `ChatPromptTemplate` (Multi-Role)

**As a** developer building a system with a persistent system prompt and user template,  
**I want** a `ChatPromptTemplate` that produces a list of `Message`s,  
**So that** I can define roles and variable slots in a structured way.

**Acceptance Criteria**:
- `ChatPromptTemplate = [PromptMessageTemplate]`
- `data PromptMessageTemplate = FixedMessage Role Text | TemplateMessage Role Text | PlaceholderMessage Text`
- `renderChatTemplate :: ChatPromptTemplate -> Map Text Text -> Either LangchainError [Message]`
- `ChatPromptTemplate` implements `Runnable` with `Input = Map Text Text`, `Output = [Message]`
- Unit test: system + user template with `{query}` variable renders correctly

**New File**: `langchain-hs-core/Langchain/Core/Prompt.hs`

**Effort**: M (3-4 days)

---

## Milestone 4: Graph Agent Engine (v0.5.0)

**Goal**: Implement `langchain-hs-graph`. Full `StateGraph` engine, checkpointers, and Human-in-the-Loop (HITL) interrupt/resume. Port `ReAct` to graph-based implementation.

**Exit Criteria**: A stateful multi-turn agent with checkpointing can be built in under 50 lines.

---

### M4-US-1: Core `StateGraph` Engine

**As a** developer building a multi-step agentic workflow,  
**I want** to define an agent as a typed state graph with nodes and edges,  
**So that** the agent's behavior is explicit, inspectable, and deterministic.

**Acceptance Criteria**:
- `StateGraph s m` with `graphNodes :: Map NodeId (Node s m)` and `graphEdges :: Map NodeId [Edge s m]`
- `compileGraph :: StateGraph s m -> Either GraphError (CompiledGraph s m)` validates structure
- `runGraph :: CompiledGraph s m -> s -> m (Either LangchainError s)` executes
- `StaticEdge NodeId` and `ConditionalEdge (s -> m (Either LangchainError NodeId))`
- `startNode` and `endNode` are reserved `NodeId` constants
- Graph with infinite loop (no path to `endNode`) returns `GraphError` at compile time
- Example: 3-node agent (start -> llm_call -> tool_use -> end) in under 30 lines

**New Files**: `langchain-hs-graph/Langchain/Graph/StateGraph.hs`

**Effort**: XL (7-10 days)

---

### M4-US-2: State Reducers

**As a** developer managing shared agent state,  
**I want** state updates to be pure functions,  
**So that** state transitions are deterministic and testable without IO.

**Acceptance Criteria**:
- `type StateReducer s = s -> s -> s` — pure merge function
- Default reducers: `appendMessagesReducer`, `replaceFieldReducer`, `mergeMapsReducer`
- `applyReducer :: StateReducer s -> s -> s -> s` — applies reducer transactionally
- All reducers satisfy associativity property (verified by QuickCheck)
- `StateGraph` takes `stateReducer :: StateReducer s` in its definition

**New File**: `langchain-hs-graph/Langchain/Graph/StateGraph.hs`

**Effort**: S (2 days)

---

### M4-US-3: Memory Checkpointing

**As a** developer building a multi-session agent,  
**I want** agent state to be saved after each node execution,  
**So that** I can resume an interrupted conversation without losing context.

**Acceptance Criteria**:
- `Checkpointer cp m` typeclass: `saveCheckpoint`, `loadCheckpoint`, `listCheckpoints`
- `MemoryCheckpointer` using `TVar (Map Text [(Text, Value)])` — thread-safe
- `SQLiteCheckpointer` using `sqlite-simple` — persists across process restarts
- `runGraphWithCheckpointer :: Checkpointer cp m => CompiledGraph s m -> cp -> Text -> s -> m (Either LangchainError s)`
- Saves checkpoint after every node, keyed by `(threadId, nodeId)`
- Unit test: save state, kill computation, reload, resume, verify final state

**New Files**: `langchain-hs-graph/Langchain/Graph/Checkpointer.hs`

**Effort**: L (5-7 days)

---

### M4-US-4: Human-in-the-Loop (HITL) Interrupt/Resume

**As a** developer building a human-review workflow,  
**I want** the agent to pause at designated nodes for human input,  
**So that** a human can review and optionally modify state before continuing.

**Acceptance Criteria**:
- `hitlNode :: Checkpointer cp m => cp -> Text -> NodeId -> Node s m` — saves state and halts
- Halting is signaled via a special `LangchainError` variant: `HITLInterrupt NodeId`
- `isHITLInterrupt :: LangchainError -> Bool` for detection in the caller
- `resumeGraph :: (FromJSON s, ToJSON s, Checkpointer cp m) => CompiledGraph s m -> cp -> Text -> m (Either LangchainError s)`
- Caller can modify state before resuming via `modifyCheckpoint`
- Integration test: interrupt at approval node, modify state, resume, verify result

**New Files**: `langchain-hs-graph/Langchain/Graph/HITL.hs`

**Effort**: M (4-5 days)

---

### M4-US-5: Graph-Based ReAct Agent

**As a** developer using the existing `ReAct` agent pattern,  
**I want** it reimplemented on the `StateGraph` engine,  
**So that** I get checkpointing, HITL, and observability for free.

**Acceptance Criteria**:
- `reactGraph :: ChatModel model => model -> ToolRegistry m -> StateGraph AgentState m`
- `AgentState = { messages :: [Message], iterations :: Int, result :: Maybe Text }`
- Nodes: `CallLLM`, `ParseAction`, `ExecuteTool`, `ReturnResult`
- Edges: `CallLLM -> ParseAction -> (ExecuteTool | ReturnResult)`, `ExecuteTool -> CallLLM`
- Max iterations enforced via `ConditionalEdge`
- Old `runAgentExecutor` reimplemented as `runGraph reactGraph`
- All existing `ReAct` tests pass with new implementation

**Files Changed**: `src/Langchain/Agent/ReAct.hs` (migrated to graph)

**Effort**: L (5-7 days)

---

### M4-US-6: Multi-Agent Supervisor Pattern

**As a** developer orchestrating multiple specialized agents,  
**I want** a supervisor node that routes tasks to the right sub-agent,  
**So that** complex tasks are handled by the most appropriate specialist.

**Acceptance Criteria**:
- `supervisorNode :: ChatModel model => model -> [(Text, CompiledGraph s m)] -> Node s m`
- Supervisor LLM decides which sub-agent to invoke based on current state
- `SubGraphNode :: CompiledGraph s m -> Node s m` — embeds graph as a node
- State is passed to sub-graph, result merged via `StateReducer`
- Example: orchestrator routes between `researcher`, `writer`, `reviewer` agents

**New File**: `langchain-hs-graph/Langchain/Graph/MultiAgent.hs`

**Effort**: L (5-7 days)

---

## Milestone 5: RAG & Observability (v0.6.0)

**Goal**: Implement `langchain-hs-rag` package. Add `RecursiveCharacterTextSplitter`, external vector stores, advanced retrievers. Add `MonadTracer`, OpenTelemetry, and LangSmith.

**Exit Criteria**: Full RAG pipeline from PDF to answer in under 20 lines. Traces visible in LangSmith.

---

### M5-US-1: `RecursiveCharacterTextSplitter`

**As a** developer processing large documents,  
**I want** a recursive splitter that respects natural text boundaries,  
**So that** chunks preserve semantic coherence rather than cutting mid-sentence.

**Acceptance Criteria**:
- `RecursiveCharacterTextSplitter { chunkSize, chunkOverlap, separators, keepSeparator }`
- Default separators: `["\n\n", "\n", " ", ""]`
- Language presets: `haskellSplitter`, `pythonSplitter`, `markdownSplitter`
- Chunk size measured in characters (not tokens) by default
- Overlap correctly maintained across chunks
- Property test: all chunks <= `chunkSize`, all adjacent chunks share `chunkOverlap` suffix

**New File**: `langchain-hs-rag/Langchain/RAG/Splitter.hs`

**Effort**: M (3-4 days)

---

### M5-US-2: `MarkdownHeaderSplitter`

**As a** developer chunking Markdown documentation,  
**I want** to split by header levels while preserving header context in metadata,  
**So that** each chunk knows what section it belongs to.

**Acceptance Criteria**:
- `MarkdownHeaderSplitter { headers :: [(Text, Text)] }` — e.g., `[("##", "section"), ("###", "subsection")]`
- Each chunk's `metadata` includes header hierarchy: `{ "section": "...", "subsection": "..." }`
- Unit test: standard Markdown document splits correctly at H2 and H3

**New File**: `langchain-hs-rag/Langchain/RAG/Splitter.hs`

**Effort**: S (2 days)

---

### M5-US-3: `PgvectorStore` (PostgreSQL)

**As a** developer needing a production-grade vector store,  
**I want** to store and search embeddings in PostgreSQL with pgvector,  
**So that** I can use my existing Postgres infrastructure for semantic search.

**Acceptance Criteria**:
- `PgvectorStore { connection :: Connection, tableName :: Text, embeddingDim :: Int }`
- `addDocuments` inserts documents with embeddings using `pgvector`'s vector type
- `similaritySearch` uses cosine distance operator `<=>` in SQL
- `delete` removes by document ID
- Metadata stored as JSONB column
- Integration test (gated on `DATABASE_URL`): add 10 docs, query, verify top result

**New File**: `langchain-hs-rag/Langchain/RAG/VectorStore/Pgvector.hs`

**Effort**: L (5-6 days)

---

### M5-US-4: `ContextualCompressionRetriever`

**As a** developer reducing context window usage,  
**I want** a retriever that compresses retrieved documents using an LLM,  
**So that** only the most relevant parts of each document are included in the context.

**Acceptance Criteria**:
- `ContextualCompressionRetriever { baseRetriever :: r, compressor :: LLMChainExtractor }`
- `LLMChainExtractor` uses LLM to extract relevant portions of each document
- Documents with no relevant content are filtered out (not returned as empty)
- `EmbeddingsFilter` as an alternative compressor (similarity-based, no LLM call)

**New File**: `langchain-hs-rag/Langchain/RAG/Retriever.hs`

**Effort**: M (4 days)

---

### M5-US-5: `MonadTracer` and OpenTelemetry Integration

**As a** developer monitoring production LLM costs and latency,  
**I want** every LLM call and chain execution to produce an OpenTelemetry span,  
**So that** I can correlate AI operations with my existing observability stack.

**Acceptance Criteria**:
- `MonadTracer m` typeclass: `withSpan`, `addAttribute`, `recordException`, `currentSpan`
- `NoOpTracer` — zero-cost default (no allocations on the hot path)
- `OTELTracer` — wraps `hs-opentelemetry` SDK, exports to OTLP
- Every `ChatModel.invoke` call wrapped in a span: name = `"langchain.llm.invoke"`, attributes include model name, token counts
- Every `ToolRegistry.callTool` call wrapped in a span: `"langchain.tool.call"`, tool name
- Span hierarchy: chain span > llm span, chain span > tool span

**New File**: `langchain-hs-core/Langchain/Core/Telemetry.hs`

**Effort**: L (6-8 days)

---

### M5-US-6: LangSmith Exporter

**As a** developer debugging agent behavior,  
**I want** my agent runs to be visible in LangSmith,  
**So that** I can trace the full decision tree with inputs, outputs, and token usage.

**Acceptance Criteria**:
- `LangSmithTracer { apiKey :: Text, projectName :: Text }`
- Implements `MonadTracer` by posting to LangSmith Run API
- `runId` from `StreamEvent` is used as LangSmith run ID
- Parent/child run hierarchy maps to span parent IDs
- Async posting (non-blocking LLM calls)
- Integration test (gated on `LANGCHAIN_API_KEY`): run appears in LangSmith project

**New File**: `langchain-hs-core/Langchain/Core/Telemetry/LangSmith.hs`

**Effort**: L (5-7 days)

---

### M5-US-7: `EnsembleRetriever`

**As a** developer combining BM25 and vector search,  
**I want** an ensemble retriever that merges results from multiple retrievers,  
**So that** I get the benefits of both sparse and dense retrieval.

**Acceptance Criteria**:
- `EnsembleRetriever { retrievers :: [(r, Float)] }` — retriever + weight pairs
- Uses Reciprocal Rank Fusion (RRF) to merge ranked document lists
- `getRelevantDocuments` returns top-k de-duplicated results by combined score
- Weights must sum to 1.0 (validated at construction)

**New File**: `langchain-hs-rag/Langchain/RAG/Retriever.hs`

**Effort**: M (3-4 days)

---

## Milestone 6: Production Hardening (v1.0.0)

**Goal**: Benchmark all critical paths. Complete Haddock documentation with 150+ examples. Publish all 5 packages to Hackage. Write tutorial blog post.

**Exit Criteria**: CI green on GHC 9.4–9.12. Hackage upload accepted. README has 5 working code examples.

---

### M6-US-1: Comprehensive Haddock Documentation

**As a** developer evaluating `langchain-hs`,  
**I want** every public function to have a runnable code example in Haddock,  
**So that** I can learn the API from the documentation without reading tests.

**Acceptance Criteria**:
- Every exported function has `@` example block in Haddock
- Every typeclass has a complete implementation example
- `README.md` has 5 complete, runnable examples: chat, pipeline, agent, RAG, streaming
- Doctests (via `doctest`) pass in CI
- Haddock builds without warnings

**Effort**: XL (10 days)

---

### M6-US-2: Performance Benchmarks (Criterion)

**As a** maintainer,  
**I want** benchmark results for critical hot paths,  
**So that** I can detect performance regressions in future releases.

**Acceptance Criteria**:
- Benchmark suite using `criterion`
- Benchmarks: `interpret` with 1/10/100 node pipeline, `WindowBufferMemory` concurrent writes, `InMemoryVectorStore.similaritySearch` at 1k/10k/100k documents, `RunnableTree Par` with 2/4/8 concurrent branches
- Baseline numbers documented in `BENCHMARKS.md`
- CI fails if any benchmark regresses more than 20%

**New File**: `bench/Main.hs`

**Effort**: M (4-5 days)

---

### M6-US-3: Full GHC Matrix CI

**As a** Haskell developer using a specific GHC version,  
**I want** the library to be tested on my GHC version,  
**So that** I'm confident it will work in my stack.

**Acceptance Criteria**:
- GitHub Actions matrix: GHC 9.4.8, 9.6.6, 9.8.4, 9.10.1, 9.12.x
- Stack LTS matrix: 21.x, 22.x, 23.x, 24.x
- Cabal matrix: latest cabal on each GHC version
- All packages build on all matrix entries
- Integration tests run on GHC 9.10 only (avoid rate limits)

**Files Changed**: `.github/workflows/ci.yml`

**Effort**: M (3 days)

---

### M6-US-4: Hackage Publication

**As a** Haskell developer discovering `langchain-hs` on Hackage,  
**I want** to find all 5 packages published with correct metadata,  
**So that** I can add them to my cabal/stack project easily.

**Acceptance Criteria**:
- All 5 packages published: `langchain-hs`, `langchain-hs-core`, `langchain-hs-providers`, `langchain-hs-graph`, `langchain-hs-rag`
- Correct dependency bounds (tested with `cabal check`)
- Category: `AI`, `Language Models`, `Machine Learning`
- License: MIT
- `CHANGELOG.md` updated with all changes
- Haddock rendered on Hackage without errors

**Effort**: M (3-4 days)

---

### M6-US-5: Tutorial & Example Applications

**As a** developer new to `langchain-hs`,  
**I want** complete example applications I can run,  
**So that** I can understand the framework by reading real code.

**Acceptance Criteria**:
- `examples/` directory with 5 complete, runnable applications:
  1. `SimpleChat.hs` — multi-turn conversation with OpenAI
  2. `Pipeline.hs` — `PromptTemplate |>> OpenAI |>> JSONOutputParser`
  3. `ReActAgent.hs` — agent with calculator and search tools
  4. `RAGPipeline.hs` — PDF ingestion to semantic Q&A
  5. `MultiAgent.hs` — supervisor routing to specialist sub-agents
- Each example has a `README.md` explaining what it does
- All examples compile and run with valid API keys

**New Files**: `examples/` directory

**Effort**: L (6-8 days)

---

## Story Point Reference

| Size | Description | Typical Effort |
|------|-------------|----------------|
| XS | Trivial change — comment, rename, tiny fix | 0.5 days |
| S | Small — single function, simple test | 1-2 days |
| M | Medium — a module or feature | 3-4 days |
| L | Large — multiple modules, integration | 5-7 days |
| XL | Extra Large — architectural change | 7-14 days |

---

## Total Milestone Estimates

| Milestone | Stories | Total Effort | Target Version |
|-----------|---------|-------------|----------------|
| M0: Foundation Fixes | 5 | ~9 days | v0.1.0 |
| M1: Core Architecture | 5 | ~21 days | v0.2.0 |
| M2: Provider Rewrite | 5 | ~21 days | v0.3.0 |
| M3: Tools & Output | 5 | ~19 days | v0.4.0 |
| M4: Graph Engine | 6 | ~33 days | v0.5.0 |
| M5: RAG & Observability | 7 | ~34 days | v0.6.0 |
| M6: Production Hardening | 5 | ~23 days | v1.0.0 |
| **Total** | **38** | **~160 days** | |

Assuming 1 full-time developer: approximately **8-9 months** from M0 to v1.0.0 release.
With community contributions targeting M2 and M5 in parallel: **5-6 months**.

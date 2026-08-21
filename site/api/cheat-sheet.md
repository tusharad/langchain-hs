---
title: Prelude Cheat Sheet
description: Quick-reference table of common types, constructors, operators, and functions in Langchain.Prelude.
category: Reference
---

## Core Types & Functions

| Function / Type | Type Signature | Purpose |
|---|---|---|
| `invoke` | `ChatModel m => m -> [Message] -> IO Message` | Invoke model with messages |
| `streamModel` | `ChatModel m => m -> [Message] -> (StreamEvent -> IO ()) -> IO ()` | Stream response events |
| `userMessage` | `Text -> Message` | Construct user message |
| `systemMessage` | `Text -> Message` | Construct system message |
| `assistantMessage` | `Text -> Message` | Construct assistant message |
| `extractMessageText` | `Message -> Text` | Extract plain text from message |

---

## Runnable AST Operators

| Operator | Type | Meaning |
|---|---|---|
| `(\|>>)` | `RunnableTree m i a -> RunnableTree m a o -> RunnableTree m i o` | Sequential pipeline composition |
| `(&>&)` | `RunnableTree m i a -> RunnableTree m i b -> RunnableTree m i (a, b)` | Parallel branch execution |
| `interpret` | `RunnableTree m i o -> i -> m o` | Evaluate runnable AST |
| `runLambda` | `(i -> o) -> RunnableTree m i o` | Pure function node |
| `runPrim` | `(i -> m o) -> RunnableTree m i o` | Monadic effect node |

---

## StateGraph & LangGraph

| Function | Type Signature | Purpose |
|---|---|---|
| `emptyStateGraph` | `StateReducer s -> StateGraph s m` | Create empty state graph |
| `addNode` | `NodeId -> Node s m -> StateGraph s m -> StateGraph s m` | Add workflow node |
| `addEdge` | `NodeId -> NodeId -> StateGraph s m -> StateGraph s m` | Add directed edge |
| `addConditionalEdge` | `NodeId -> (s -> NodeId) -> [(Text, NodeId)] -> StateGraph s m -> StateGraph s m` | Add branching edge |
| `compileGraph` | `StateGraph s m -> Maybe (Checkpointer s m) -> CompiledGraph s m` | Compile state graph |
| `runGraph` | `CompiledGraph s m -> s -> m s` | Execute compiled workflow |
| `toDot` | `StateGraph s m -> String` | Export Graphviz DOT representation |

---

## Model Context Protocol (MCP)

| Function | Type Signature | Purpose |
|---|---|---|
| `newStdioMcpClient` | `FilePath -> [String] -> IO McpClient` | Connect to stdio MCP server |
| `newHttpMcpClient` | `Text -> IO McpClient` | Connect to HTTP SSE MCP server |
| `listMcpTools` | `McpClient -> IO [McpToolInfo]` | Discover available MCP tools |
| `mcpToolToLangchainTool` | `McpToolInfo -> Tool m` | Convert to native Langchain Tool |

---

## Observability & Resilience

| Function | Type Signature | Purpose |
|---|---|---|
| `newOTelTracer` | `Maybe Text -> IO OTelTracer` | Create OpenTelemetry tracer |
| `withSpan` | `OTelTracer -> Text -> Maybe SpanId -> SpanKind -> Map Text Text -> m a -> m a` | Trace block in OpenTelemetry span |
| `exportSpansJson` | `OTelTracer -> IO Text` | Export traces to JSON |
| `newCircuitBreaker` | `CircuitBreakerConfig -> IO CircuitBreaker` | Create 3-state Circuit Breaker |
| `withCircuitBreaker` | `CircuitBreaker -> m a -> m a` | Protect action with breaker |

# Changelog for `langchain-hs-core`

All notable changes to this package will be documented in this file.

## 0.0.6.2 - 2026-11-12

> **Note**: `0.0.6.1` was skipped globally after `langchain-hs-ollama-v0.0.6.1`.

- **Tool-Call & Agent Streaming**:
  - Extended `StreamEvent` in `Langchain.Stream` with `ToolStart`, `ToolChunk`, `ToolEnd`, and `ToolErrorEvent`, plus `liftChatStream` and `tapStreamMessage`.
  - Added `StreamingTool`, `createStreamingTool`, `fromTool`, `streamToolCall`, and `streamStreamingToolCall` to `Langchain.Tool`.
  - Added `streamingShellTool` (`Langchain.Tool.Shell`) and `withStreamingSecurity` (`Langchain.Tool.Security`).
  - Added `withStreamingTools`, `reactStepStream`, `streamReActAgent`, `streamReActAgentWithTrace`, and `runReActAgentStream` to `Langchain.Agent.ReAct`.
  - Added `streamPlanAndExecute` and `runPlanAndExecuteStream` to `Langchain.Agent.PlanAndExecute`.
  - Updated `Langchain.Prelude` exports for streaming tools and agents.
- **Security, Checkpointing, Memory & Parsers (consolidated post-`0.0.6.0` alignment)**:
  - `SecurityPolicy`, `FileSystem`, `Process`, and `Checkpoint` tool suites (`Langchain.Tool.*`).
  - `InVariableMemory`, `SessionMemory`, `EntityMemory`, `CombinedMemory`, `FileMemory`, and buffer reset helpers.
  - Updated `sqlite-simple` bounds to `>= 0.4.18 && < 0.5`.

## 0.0.6.0 - 2026-10-02

- Updated bounds for GHC 9.12 and modern Stackage LTS releases.
- Standalone stack configuration and verified test suite.

## 0.0.5.0 - 2026-09-10

- Initial standalone Hackage release of `langchain-hs-core`.
- Zero-dependency pure core architecture.
- First-class `RunnableTree` GADT AST with sequential (`|>>`), parallel (`&>&`), and fallback (`>>>#`) operators.
- `ChatModel` effect-polymorphic interface.
- Multi-modal `ContentBlock` and message structures.
- Conduit-based `StreamEvent` and streaming protocol.
- Decoupled `LangchainT env m a` transformer.
- Typed `Tool` and `FunctionDefinition`.

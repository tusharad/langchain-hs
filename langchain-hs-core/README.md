# `langchain-hs-core`

> The foundational, effect-polymorphic core framework of the `langchain-hs` AI ecosystem.

`langchain-hs-core` provides pure GADT abstract syntax trees, effect-polymorphic chat models, streaming protocols, tools, agents, vector stores, retrievers, document loaders, memory, cache, output parsers, guardrails, resilience, observability, and the canonical `Langchain.Prelude` without concrete provider-specific dependencies.

## Key Primitives & Modules

- **Runnables & Pure ASTs (`Langchain.Runnable`)**: Pure GADT abstract syntax tree (`RunnableTree m i o`) for pipeline composition via `|>>` (sequential), `&>&` (parallel fan-out), `runLambda`, `runPrim`, and deferred evaluation via `interpret`.
- **Chat Models & Types (`Langchain.Model`, `Langchain.Model.Types`)**: Effect-polymorphic `ChatModel` typeclass, `Message`, `Role`, `ContentBlock` (multi-modal text, images, tool calls, tool results).
- **Streaming Protocols (`Langchain.Stream`)**: Conduit-based incremental token streaming via `StreamEvent` and `LLMChunk`.
- **Tools & Dynamic Calling (`Langchain.Tool`, `Langchain.Tool.Binding`)**: Effect-polymorphic `Tool m`, `createTool`, Generic Schemas, and built-in tools (`Calculator`, `FileSystem`, `Shell`). Provider-agnostic tool binding via `ToolBinder`.
- **Agents (`Langchain.Agent.ReAct`, `Langchain.Agent.PlanAndExecute`)**:
  - **ReAct Agent**: Full reasoning + acting agent loop with tool inspection, memory integration, callbacks, and step traces.
  - **Plan-and-Execute Agent**: Two-phase agent decomposing complex user goals into discrete execution plans.
- **Prompt Templates (`Langchain.PromptTemplate`)**: String prompt templates with `{var}` f-string interpolation and pre-bound partial variables.
- **Vector Stores (`Langchain.VectorStore`)**: Abstract `VectorStore` typeclass and thread-safe `InMemoryVectorStore`.
- **Retrievers (`Langchain.Retriever`)**: BM25 keyword search (`BM25Retriever`), vector similarity retriever, and Reciprocal Rank Fusion (`HybridRetriever`).
- **Document Loaders & Text Splitters (`Langchain.DocumentLoader`, `Langchain.TextSplitter`)**: `Document` metadata abstraction and splitters for character chunks, Markdown, programming code, and tokens.
- **Memory & Cache (`Langchain.Memory`, `Langchain.Cache`)**: `BaseMemory`, `ConversationBufferMemory`, `WindowBufferMemory`, and STM-backed in-memory `LLMCache`.
- **Output Parsers (`Langchain.OutputParser`)**: Strongly typed JSON Schema generation (`JsonOutputParser`) and structured parsing.
- **Guardrails & Safety (`Langchain.Guardrails`)**: Input and output content moderation, PII redaction, and semantic safety validations.
- **Resilience & Fault Tolerance (`Langchain.Resilience`)**: State-machine `CircuitBreaker`, exponential backoff `RetryConfig`, and fallback handlers.
- **Observability & Tracing (`Langchain.Observability`, `Langchain.Callback`)**: OpenTelemetry distributed tracing spans, structured JSON event logging, and lifecycle event callbacks.
- **Chains (`Langchain.Chain.RetrievalQA`)**: End-to-end question answering pipeline over indexed knowledge bases.
- **Langchain Monad (`Langchain.Monad`)**: Decoupled `LangchainT env m a` reader and error transformer.
- **Canonical Prelude (`Langchain.Prelude`)**: Batteries-included single-import entry point providing the full core framework API.

## Installation

Add to your `cabal` or `package.yaml` dependencies:

```cabal
build-depends:
    langchain-hs-core >= 0.0.6.0 && < 0.0.7.0
```

To connect concrete LLMs, combine with satellite provider packages:
- `langchain-hs-ollama`: Local & offline models (Llama 3, DeepSeek, Gemma).
- `langchain-hs-openai`: OpenAI, OpenRouter, and OpenAI-compatible cloud endpoints.
- `langchain-hs-gemini`: Google Gemini with multi-modal support.
- `langchain-hs-mcp`: Model Context Protocol tool client.

For cyclic state machines, checkpointers, and multi-agent coordination, add `langchain-hs-graph` (or use the umbrella `langchain-hs` package).

## License

Distributed under the **MIT License**. See [LICENSE](LICENSE) for details.

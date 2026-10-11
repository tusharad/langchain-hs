# 🦜️🔗 Langchain-hs Examples

A comprehensive suite of 43 verified example executables demonstrating each core component of the `langchain-hs` ecosystem using **Ollama** (local/offline), **OpenAI** (cloud API), and **Google Gemini** (Interactions v1 API).

## How to Build & Run

From the repository root:

```bash
# Build all example executables
cabal build examples

# Run a specific example (e.g., Gemini simple example)
cabal run simplegemini
```

Or with Stack:

```bash
# Build with Stack
stack --stack-yaml examples/stack.yaml build

# Run a specific example
stack --stack-yaml examples/stack.yaml run <executable-name>
```

---

## Environment Setup

- **Ollama**: Ensure Ollama is running locally (`ollama serve`). Pull required models (e.g. `ollama pull llama3` or `ollama pull gemma3`).
- **OpenAI**: Set your API key:
  ```bash
  export OPENAI_API_KEY="sk-..."
  ```
- **Google Gemini**: Set your API key in environment or store in `~/.gemini_api_key`:
  ```bash
  export GEMINI_API_KEY="AIzaSy..."
  ```

---

## Available Executables

| Component | Ollama Executable | OpenAI Executable | Gemini Executable | Description |
|---|---|---|---|---|
| **Chat Models** | `simpleollama` | `simpleopenai` | `simplegemini` | Basic invocation, templating, prompt generation, and batch |
| **Streaming** | `streamollama` | `streamopenai` | `streamgemini` | Conduit-based reactive token streaming (SSE) |
| **Tools & Calling** | `toolollama` | `toolopenai` | `toolgemini` | Function definition, invocation, and tool binders |
| **RAG & Embeddings** | `ragollama` | `ragopenai` | `raggemini` | Document loading, chunking, and embedding vectors (`gemini-embedding-001`) |
| **Langchain Monad** | `monadollama` | `monadopenai` | — | Decoupled `LangchainT` reader/error transformer |
| **Structured Output** | `jsonollama` | `jsonopenai` | — | Typed JSON Schema generation and parsing |
| **Prompt Template** | `prompttemplate` | `prompttemplate` | `prompttemplate` | Pure string template interpolation with pre-bound variables |
| **Hybrid Retrievers** | `retrieverollama` | `retrieveropenai` | — | BM25 + vector similarity hybrid retrieval |
| **Memory Systems** | `memoryollama` | `memoryopenai` | — | Conversation buffer and sliding window memory |
| **Retrieval QA** | `retrievalqaollama` | `retrievalqaopenai` | — | Question-answering pipeline over loaded documents |
| **ReAct Agent** | `reactollama` | `reactopenai` | — | Multi-turn reasoning and tool execution loop |
| **Plan-and-Execute** | `planandexecuteollama` | `planandexecuteopenai` | — | Two-phase planning and step execution agent |
| **Guardrails** | `guardrailollama` | `guardrailopenai` | — | Content safety and output length validation |
| **Resilience** | `resilienceollama` | `resilienceopenai` | — | Circuit breakers, retry policies, and fallbacks |
| **Observability** | `observabilityollama` | `observabilityopenai` | — | OpenTelemetry distributed tracing and structured logging |
| **MCP Integration** | `mcpollama` | `mcpopenai` | — | Model Context Protocol client over stdio |
| **StateGraph** | `stategraphollama` | `stategraphopenai` | — | Cyclic state machines with STM checkpoints |
| **Multi-Agent** | `multiagentollama` | `multiagentopenai` | — | Supervisor agent routing and multi-agent coordination |
| **HITL** | `hitlollama` | `hitlopenai` | — | Human-in-the-loop interrupts and state resumption |
| **Runnables** | `runnableollama` | `runnableopenai` | — | Pure AST composition (`|>>`, `&>&`, `>>>#`) |

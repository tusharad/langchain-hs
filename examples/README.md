# 🦜️🔗 Langchain-hs Examples

A comprehensive suite of 34 verified example executables demonstrating each core component of the `langchain-hs` ecosystem using **Ollama** (local/offline) and **OpenAI** (cloud API).

## How to Build & Run

From the repository root:

```bash
# Build all 34 example executables
stack --stack-yaml examples/stack.yaml build

# Run a specific example
stack --stack-yaml examples/stack.yaml run <executable-name>
```

Or from within the `examples/` directory:

```bash
cd examples
stack build
stack run <executable-name>
```

---

## Environment Setup

- **Ollama**: Ensure Ollama is running locally (`ollama serve`). Pull required models (e.g. `ollama pull llama3` or `ollama pull gemma3`).
- **OpenAI**: Set your API key:
  ```bash
  export OPENAI_API_KEY="sk-..."
  ```

---

## Available Executables

| Component | Ollama Executable | OpenAI Executable | Description |
|---|---|---|---|
| **Chat Models** | `simpleollama` | `simpleopenai` | Basic invocation, templating, and prompt generation |
| **Streaming** | `streamollama` | `streamopenai` | Conduit-based reactive token streaming |
| **Langchain Monad** | `monadollama` | `monadopenai` | Decoupled `LangchainT` reader/error transformer |
| **Structured Output** | `jsonollama` | `jsonopenai` | Typed JSON Schema generation and parsing |
| **Tools & Calling** | `toolollama` | `toolopenai` | Function definition, invocation, and tool binders |
| **RAG & Embeddings** | `ragollama` | `ragopenai` | Document loading, chunking, and embedding vectors |
| **Hybrid Retrievers** | `retrieverollama` | `retrieveropenai` | BM25 + vector similarity hybrid retrieval |
| **Memory Systems** | `memoryollama` | `memoryopenai` | Conversation buffer and sliding window memory |
| **Retrieval QA** | `retrievalqaollama` | `retrievalqaopenai` | Question-answering pipeline over loaded documents |
| **Map-Reduce** | `mapreduceollama` | `mapreduceopenai` | Document summarization via parallel map-reduce |
| **ReAct Agent** | `reactollama` | - | Multi-turn reasoning and tool execution loop |
| **Plan-and-Execute** | `planandexecuteollama` | - | Two-phase planning and step execution agent |
| **Guardrails** | `guardrailollama` | `guardrailopenai` | Content safety and output length validation |
| **Resilience** | `resilienceollama` | `resilienceopenai` | Circuit breakers, retry policies, and fallbacks |
| **Observability** | `observabilityollama` | `observabilityopenai` | OpenTelemetry distributed tracing and structured logging |
| **MCP Integration** | `mcpollama` | `mcpopenai` | Model Context Protocol client over stdio |
| **StateGraph** | `stategraphollama` | - | Cyclic state machines with STM checkpoints |
| **Multi-Agent** | - | - | Supervisor agent routing and multi-agent coordination |
| **HITL** | `hitlollama` | `hitlopenai` | Human-in-the-loop interrupts and state resumption |
| **Runnables** | `runnableollama` | `runnableopenai` | Pure AST composition (`\|>>`, `&>&`, `>>>#`) |

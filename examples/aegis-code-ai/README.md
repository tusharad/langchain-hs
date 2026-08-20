# AegisCode AI

Enterprise Autonomous Software Security, Refactoring & Compliance Orchestration Engine.

Built on [langchain-hs](https://github.com/tusharad/langchain-hs).

## Running

```bash
# Demo mode (MockModel, no external dependencies)
stack exec aegis-code-ai -- demo

# Scan a repository with Ollama
stack exec aegis-code-ai -- scan --repo /path/to/repo

# Start the REST + WebSocket server
stack exec aegis-code-ai -- serve --port 8080
```

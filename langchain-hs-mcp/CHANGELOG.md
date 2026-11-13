# Changelog for langchain-hs-mcp

## 0.0.6.2 - 2026-11-14

> **Note**: `0.0.6.1` was skipped globally after `langchain-hs-ollama-v0.0.6.1`.

- Coordinated monorepo release alignment to `0.0.6.2`.
- Updated `langchain-hs-core` dependency bound to `>= 0.0.6.2 && < 0.0.7` and refreshed Stack configuration.
- Added MCP Resource inspection (`listMcpResources`, `readMcpResource`) for stdio and HTTP transports.
- Added MCP Prompt inspection and template expansion (`listMcpPrompts`, `getMcpPrompt`).
- Added streaming tool adapter `mcpToolToStreamingTool` yielding incremental Conduit text chunks.

## 0.0.6.0 - 2026-10-02

- Initial release. Split from the monolithic `langchain-hs` package.
- Provides `Langchain.MCP.Client`.

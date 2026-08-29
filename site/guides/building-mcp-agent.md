---
title: Building an MCP-Powered Agent
description: Connect your Haskell AI agent to external tools and data sources with Model Context Protocol (MCP).
category: Guides & Recipes
---

## Building an MCP Agent in Haskell

Anthropic's **Model Context Protocol (MCP)** defines a standard for AI models to discover and execute tools across local and remote servers.

In this guide, we'll build a ReAct agent powered by both the standard filesystem MCP server and an SQLite MCP server.

---

## 1. Starting the MCP Client

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Langchain.Prelude

main :: IO ()
main = do
  -- 1. Initialize local LLM
  let model = newOllama "qwen2.5:7b" "http://localhost:11434"

  -- 2. Connect to Filesystem MCP Server over stdio
  fsMcp <- newStdioMcpClient "npx" 
    [ "-y", "@modelcontextprotocol/server-filesystem"
    , "/Users/alice/projects/my-app"
    ]

  -- 3. Discover tools exported by the server
  fsTools <- listMcpTools fsMcp
  let nativeTools = map mcpToolToLangchainTool fsTools

  -- 4. Equip ReAct Agent with MCP tools
  let agent = createReActAgent model nativeTools defaultAgentConfig

  -- 5. Run agent query
  putStrLn "Asking agent to inspect files..."
  response <- runReActAgent agent "List all files in the project root and read package.yaml."
  print response
```

---

## 2. Using Remote HTTP MCP Servers

For distributed enterprise services, connect via HTTP Server-Sent Events (SSE):

```haskell
httpMcp <- newHttpMcpClient "http://mcp-gateway.internal:8000/sse"
remoteTools <- listMcpTools httpMcp
```

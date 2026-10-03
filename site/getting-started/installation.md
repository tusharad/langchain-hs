---
title: Installation & Setup
description: Setting up langchain-hs in your Haskell project using Stack, Cabal, or Nix.
category: Getting Started
---

## Requirements

- **GHC**: `9.6.x` through `9.10.x` supported (GHC 9.6.7 recommended)
- **Stack** (`>= 2.9`) or **Cabal** (`>= 3.8`)
- Local LLM provider (e.g. [Ollama](https://ollama.com/)) or API keys (OpenAI, Gemini, OpenRouter)

---

## Adding to Your Project

`langchain-hs` is structured into modular, decoupled packages. Install the core framework and only the provider integrations you actually need:

| Package | Purpose | Typical Use Case |
|---|---|---|
| `langchain-hs` | Core Framework | Agents (`ReAct`, `PlanAndExecute`), Chains, Vector Stores, Observability |
| `langchain-hs-core` | Pure AST & Types | Zero-dependency pure ASTs (`RunnableTree`), `ChatModel`, `Tool` |
| `langchain-hs-graph` | Graph & Multi-Agent | `StateGraph`, Checkpointing, Time-Travel, Parallel Nodes |
| `langchain-hs-ollama` | Ollama Provider | Local/offline inference via Ollama |
| `langchain-hs-openai` | OpenAI Provider | OpenAI & OpenAI-compatible endpoints (OpenRouter, Fireworks) |
| `langchain-hs-gemini` | Gemini Provider | Google Gemini API with function calling and SSE streaming |
| `langchain-hs-mcp` | MCP Client | Connect to Model Context Protocol servers over stdio |

### Using Stack

Add the packages to your `stack.yaml`:

```yaml
resolver: lts-24.56 # Or nightly / lts-23+ / lts-22+

packages:
  - .

extra-deps:
  - langchain-hs-0.0.6.0
  - langchain-hs-core-0.0.6.0
  - langchain-hs-graph-0.0.6.0
  # Add providers as needed:
  - langchain-hs-ollama-0.0.6.0
  - langchain-hs-openai-0.0.6.0
  - langchain-hs-gemini-0.0.6.0
  - langchain-hs-mcp-0.0.6.0
```

In your `package.yaml` (or `.cabal` file):

```yaml
dependencies:
  - base >= 4.14 && < 5
  - text
  - langchain-hs
  # Add only what you use:
  - langchain-hs-ollama
  - langchain-hs-openai
```

### Using Cabal

Add to your `cabal.project` or `.cabal` build-depends:

```cabal
build-depends:
    base >= 4.14 && < 5,
    text,
    langchain-hs >= 0.0.6,
    langchain-hs-ollama >= 0.0.6
```

---

## Canonical Import: `Langchain.Prelude`

`langchain-hs` provides a unified prelude module that exposes all common constructors, operators, types, and runners:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Langchain.Prelude
```

<div class="admonition tip">
  <div class="admonition-title">💡 Pro Tip</div>
  <code>Langchain.Prelude</code> does not conflict with Haskell's standard <code>Prelude</code>. You can safely import both without name shadowing.
</div>

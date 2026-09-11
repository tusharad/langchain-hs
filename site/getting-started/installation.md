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

`langchain-hs` is structured into three composable packages. Depending on your needs, you can depend on the full umbrella package or just the pure core.

| Package | Purpose | Typical Use Case |
|---|---|---|
| `langchain-hs` | Full ecosystem | Providers, MCP, Vector Stores, Chains, Agents, Observability |
| `langchain-hs-graph` | Graph & Multi-Agent | `StateGraph`, Checkpointing, Time-Travel, Parallel Nodes |
| `langchain-hs-core` | Pure AST & Types | Zero-dependency pure ASTs (`RunnableTree`), `ChatModel`, `Tool` |

### Using Stack

Add the packages and required extra dependencies to your `stack.yaml`:

```yaml
resolver: lts-22.44 # Or nightly / lts-23+

packages:
  - .

extra-deps:
  # langchain-hs packages (or from Hackage)
  - langchain-hs-0.5.0.0
  - langchain-hs-graph-0.5.0.0
  - langchain-hs-core-0.2.0.0

  # Upstream dependencies
  - git: https://github.com/tusharad/ollama-haskell
    commit: e38a92529d83303c5d05c38b73d73a278721700e
  - git: https://github.com/MercuryTechnologies/openai
    commit: 2031a05135b892a82b461558d19b813ab67cfb8f
  - git: https://github.com/lbobylev/pystrformat.git
    commit: f603a296800ed3fb488eb9ba8976812bca4bb983
  - mcp-server-0.2.0.1
```

In your `package.yaml` (or `.cabal` file):

```yaml
dependencies:
  - base >= 4.14 && < 5
  - text
  - langchain-hs
```

### Using Cabal

Add to your `cabal.project` or `.cabal` build-depends:

```cabal
build-depends:
    base >= 4.14 && < 5,
    text,
    langchain-hs >= 0.5.0
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

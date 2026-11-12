# AGENTS.md — langchain-hs

This file is the primary orientation document for AI coding agents working on this codebase.
Read this fully before making any change. Update it when architectural decisions are made.

---

## What This Project Is

`langchain-hs` is a type-safe, modular Haskell monorepo for building LLM applications, RAG pipelines, tools, MCP integrations, and stateful multi-agent workflows.

**Current state**: Monorepo with 7 published packages (`langchain-hs-core`, `langchain-hs-graph`, `langchain-hs-mcp`, `langchain-hs-ollama`, `langchain-hs-openai`, `langchain-hs-gemini`, and the `langchain-hs` umbrella package) plus `examples/` and the Hakyll documentation `site/`.
**Active target**: `v0.0.6.2` (`0.0.6.1` was skipped globally because `langchain-hs-ollama` already claimed `0.0.6.1` for `ollama-haskell 0.4.2.0`).

The `v0.0.6.2` product roadmap and user stories live at:
`wiki/user-stories-0.0.6.2.md`

---

## Formal Verification Requirement

**Every new feature or bug fix MUST follow the `formal-spec-verifier` skill protocol:**

1. Read `.agents/skills/formal-spec-verifier/SKILL.md`
2. Fill a System Law Spec from `.agents/skills/formal-spec-verifier/resources/system-law-template.yaml`
3. Write HSpec / QuickCheck properties FIRST (leave implementation body as `undefined` until contract and tests are in place)
4. Implement minimum code to satisfy the oracle
5. Run: `./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell`

---

## Branching Strategy & Git Protocol

All agents and contributors working on this codebase MUST follow this branching model without exception:

### 1. Branch Roles & Hierarchy
- **`main`**: Protected production branch. Represents verified, stable releases tagged with SemVer (`vX.Y.Z` or `<pkg>-vX.Y.Z`). **NEVER commit or push directly to `main`.**
- **`develop`**: Primary integration branch and default development target. All feature and bugfix branches originate here and merge back here.
- **Supporting Branches**:
  - `feat/<story-or-feature>` (e.g., `feat/stategraph-streaming`, `feat/agent-tool-call-streaming`): Cut from `develop`, merged back to `develop` via PR.
  - `fix/<bug-id-or-description>` (e.g., `fix/stream-cancellation`): Cut from `develop`, merged back to `develop`.
  - `release/vX.Y.Z`: Release stabilization branches cut from `develop` when preparing milestone releases; merged into `main` (with release tag) and back into `develop`.
  - `hotfix/<description>`: Emergency production fixes cut from `main`, merged into both `main` and `develop`.
  - `chore/<task>`: Documentation, toolchain, or CI/CD updates cut from `develop`.

### 2. Mandatory Agent Workflow
1. **Always base work on up-to-date `develop`**:
   ```bash
   git checkout develop
   git pull origin develop
   git checkout -b feat/<feature-name>
   ```
2. **Never commit directly to `main`** or leave feature work unmerged on detached/ad-hoc branches.
3. **Execute Formatting, Linting & Verification Gate before committing/merging**:
   ```bash
   make format-check   # fourmolu -m check across all 7 packages + examples
   make lint           # hlint across all 7 packages + examples
   stack test --pedantic
   ./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell
   ```
4. **Use Conventional Commits**:
   - `feat:` for new capabilities
   - `fix:` for bug fixes
   - `refactor:` for code restructuring without behavior change
   - `test:` for test additions or updates
   - `docs:` for documentation updates
   - `chore:` for build/dependency/tool updates

---

## Monorepo Architecture

```
stack.yaml / cabal.project     -- Monorepo workspace configuration (resolver: nightly-2026-10-11)
fourmolu.yaml                  -- Repository formatting rules (2-space indent, 100 cols, leading commas)
makefile                       -- lint, format, format-check, docs, site-build, site-watch, clean

langchain-hs-core/             -- Pure typeclasses, AST primitives, agents, memory, tools, streaming, evals
  src/Langchain/
    Monad.hs                   -- LangchainT r m = ReaderT r (ExceptT LangchainError m), runLangchainT
    Error.hs                   -- LangchainError (..), ErrorContext (..), typed error constructors
    Prelude.hs                 -- Canonical umbrella re-export module
    Model.hs / Model/Types.hs  -- ChatModel (..), Message (..), Role, ContentBlock, ToolCall
    Stream.hs                  -- StreamEvent (..), ChatStream, EventStream, liftChatStream, consumeStreamWith
    Runnable.hs                -- RunnableTree (..), (|>>), (&>&), interpret
    Agent/ReAct.hs             -- ReActAgent, runReActAgent, streamReActAgent, runReActAgentStream
    Agent/PlanAndExecute.hs    -- Plan-and-Execute agent + streaming
    Tool/Core.hs               -- Tool m, StreamingTool m
    Tool/Binding.hs            -- ToolBinder typeclass
    Tool/FileSystem.hs         -- newFileSystemSuite, read/edit/write/grep/find/listDir tools
    Tool/Shell.hs              -- shellTool, streamingShellTool
    Tool/Process.hs            -- Process execution tools
    Tool/Checkpoint.hs         -- FileCheckpoint, ToolAuditEntry, undoFileEditTool
    Tool/Security.hs           -- SecurityPolicy, path/command validation
    Memory/                    -- BaseMemory, WindowBufferMemory, Summary, Entity, Combined, Session
    VectorStore/ & Retriever/  -- VectorStore, InMemory, BM25, Hybrid
    TextSplitter/              -- Character, Code, Markdown, RecursiveCharacter, Token
    DocumentLoader/            -- FileLoader, DirectoryLoader
    OutputParser/              -- Core, Structured (StructuredOutput, TypeSchema, structuredInvoke)
    Guardrail/Core.hs          -- Guardrail, contentSafetyGuardrail
    Resilience/                -- Retry, CircuitBreaker
    Cache/Core.hs              -- CacheableChatModel, cacheModelIdentity
    Callback/Manager.hs        -- CallbackEvent, CallbackManager
    Observability.hs           -- Structured logging & OTelTracer
langchain-hs-graph/            -- Langchain.Graph.{StateGraph, Checkpointer, HITL}
langchain-hs-mcp/              -- Langchain.MCP.Client (stdio + HTTP MCP JSON-RPC 2.0 client)
langchain-hs-openai/           -- Langchain.Provider.OpenAI, Langchain.Embeddings.OpenAI
langchain-hs-gemini/           -- Langchain.Provider.Gemini, Langchain.Embeddings.Gemini, Gemini.Client.*
langchain-hs-ollama/           -- Langchain.Provider.Ollama, Langchain.Embeddings.Ollama
langchain-hs/                  -- Re-export umbrella package (langchain-hs-core + graph) + integration tests
examples/                      -- Runnable example apps (package.yaml / examples.cabal)
site/                          -- Hakyll documentation website
wiki/                          -- Planning docs & user stories (e.g. wiki/user-stories-0.0.6.2.md)
```

---

## Core Monad & Capability Patterns

### `LangchainT` Monad Transformer Stack

All monadic framework interfaces run cleanly over `LangchainT` (`MonadIO m, MonadError LangchainError m`):

```haskell
-- Langchain/Monad.hs
type LangchainT r m = ReaderT r (ExceptT LangchainError m)

-- With a custom config
type App a = LangchainT MyConfig IO a

-- Without any config
runSimple :: LangchainT () IO a -> IO (Either LangchainError a)
runSimple = runLangchainT ()
```

### Provider & Capability Typeclass Pattern

Keep core abstractions in `langchain-hs-core` independent of provider SDKs. Providers live in their own packages and implement the core typeclasses:

- **`ChatModel model`**: `invoke` (non-streaming) and `stream` (via `ChatStream` / `StreamEvent` callbacks).
- **`ToolBinder model m`**: binds `[Tool m]` / `[StreamingTool m]` JSON schemas onto provider configs.
- **`CacheableChatModel model`**: exposes deterministic `cacheModelIdentity` for response caching.
- **`Tool m` & `StreamingTool m`**: typed tool execution, with `ToolChunk` progress emission in `streamReActAgent`.

---

## Code Quality & Formatting Rules

1. **Compiler strictness**: All code must compile cleanly with `--pedantic` (`-Wall -Werror` and strict warning flags in `package.yaml`). Avoid partial functions (`head`, `tail`, `fromJust`, `error`).
2. **Fourmolu formatting**: Always run `make format` or `fourmolu -m check` matching `fourmolu.yaml` (2-space indent, 100-column limit, leading commas and leading import/export lists).
3. **HLint cleanliness**: Always run `make lint` and address all HLint hints before committing.
4. **Cabal & Stack parity**: Whenever dependencies or exposed modules change in `package.yaml`, regenerate `.cabal` files (`hpack`) and verify both `stack build` and `cabal check`.
5. **Preserve public API backwards compatibility**: Keep existing exported functions and data constructors compatible unless a breaking change is explicitly approved for a minor/major bump.

---

## Testing Rules

1. **Pure invariants**: Add `QuickCheck` property tests for pure transformations, parsers, splitters, schema generation, and graph/state laws.
2. **IO & Provider boundaries**: Write `HSpec` unit tests using mock models (`TestChatModel` or local `Warp` HTTP/SSE mock servers) — **never require live network API keys in unit/CI test suites**.
3. **Streaming tests**: Verify incremental chunk emission (e.g., via gated `MVar`/`TVar` mock streams) and clean cancellation/error propagation, not just final aggregated output.
4. **Test paths mirror source structure**: Place package-specific tests in `<package>/test/Test/Langchain/...Spec.hs` and cross-cutting agent tests in `langchain-hs/test/Test/Langchain/...`.
5. **Verify exact build & test commands**:
   - `stack test --pedantic`
   - `./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell`
   - `make format-check && make lint`

---

## File-by-File Change Guide

When an AI agent implements a user story or fix:

1. Check the story in `wiki/user-stories-0.0.6.2.md` (or issue description) and list affected packages/files.
2. Read the relevant existing source and test files first (economically, targeted line ranges).
3. Write the HSpec / QuickCheck test cases FIRST (TDD).
4. Fill a System Law Specification per `.agents/skills/formal-spec-verifier/SKILL.md`.
5. Implement the minimum clean change to pass `--pedantic` compilation and tests.
6. Run `make format-check`, `make lint`, and `./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell`.
7. Check `cabal check` on modified packages if `package.yaml`/`.cabal` changed.


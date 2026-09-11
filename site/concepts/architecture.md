---
title: Architecture & Purity
description: The 3-tier monorepo architecture, effect polymorphism, and zero-unsafePerformIO guarantee.
category: Core Concepts
---

## Monorepo Architecture

`langchain-hs` is split into three independent layers to guarantee modularity and zero unnecessary runtime dependencies:

```
┌─────────────────────────────────────────────────────────────┐
│                       langchain-hs                          │
│  Providers (Ollama, OpenAI, Gemini) · MCP Client · RAG     │
│  Chains · Vector Stores · Agents · OpenTelemetry Observability│
└──────────────────────────────┬──────────────────────────────┘
                               │ depends on
┌──────────────────────────────▼──────────────────────────────┐
│                    langchain-hs-graph                       │
│  StateGraph s m · StateReducer s · Checkpointers · HITL     │
│  TimeTravel · Parallel Execution · Graphviz DOT Export      │
└──────────────────────────────┬──────────────────────────────┘
                               │ depends on
┌──────────────────────────────▼──────────────────────────────┐
│                    langchain-hs-core                        │
│  Zero-Dependency Pure GADT ASTs (RunnableTree)              │
│  ChatModel · ContentBlock · Tool m · StreamEvent Lifecycle  │
│  Zero HTTP / Network / Database Dependencies                │
└─────────────────────────────────────────────────────────────┘
```

---

## 1. Zero `unsafePerformIO` Guarantee

In many imperative AI frameworks, state mutation, network I/O, and tool executions are side-effecting operations scattered throughout code. In `langchain-hs`:

- **Construction is Pure**: Creating a `RunnableTree`, `StateGraph`, or `Chain` produces a pure data structure AST without executing any network or disk operations.
- **Execution is Explicit**: All effects are tracked in the monad `m` (e.g. `IO`, `LangchainT m`, or custom monad stacks).
- **Zero Hidden Global State**: State is explicitly passed or held in STM `TVar` cells with ACID properties.

---

## 2. Algebraic Laws & QuickCheck Properties

Components in `langchain-hs` are verified against standard algebraic laws:

### 1. StateReducer Monoid Associativity
State reducers must satisfy associativity so that concurrent parallel branches can be merged in any order without race conditions:

$$\forall a, b, c.\ (a \diamond b) \diamond c \equiv a \diamond (b \diamond c)$$

### 2. Runnable Identity & Composition Laws
Pure runnable pipelines satisfy Category / Arrow laws:

$$\text{interpret } (\text{id} \mathbin{|\!\gg} f) \equiv \text{interpret } f$$
$$\text{interpret } (f \mathbin{|\!\gg} \text{id}) \equiv \text{interpret } f$$

---

## 3. Effect Polymorphism

All models and tools are polymorphic over their execution monad `m`:

```haskell
-- Effect-polymorphic ChatModel
class (MonadIO m, MonadError LangchainError m) => ChatModel model m where
  invoke :: model -> [Message] -> m Message
  streamModel :: model -> [Message] -> (StreamEvent -> m ()) -> m ()
```

This allows you to run pipelines inside unit test harnesses with `Identity` or `State` monads, or in production web servers inside custom `ReaderT AppEnv (ExceptT AppError IO)`.

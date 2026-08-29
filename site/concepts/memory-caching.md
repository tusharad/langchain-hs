---
title: Memory Systems & Caching
description: Conversational memory backends, sliding window buffers, and SQLite prompt caching.
category: Core Concepts
---

## Memory Systems

`langchain-hs` provides modular memory implementations implementing the `BaseMemory` interface:

### 1. `WindowBufferMemory`
Maintains a sliding window of the last $k$ conversation turns:

```haskell
-- Retain only the last 5 conversation turns
memory <- newWindowBufferMemory 5
```

### 2. `SummaryMemory`
Maintains a running natural language summary of the ongoing dialogue, updating it after each turn to avoid context window explosion:

```haskell
summaryMem <- newSummaryMemory summarizerModel
```

### 3. `EntityMemory`
Extracts and maintains dynamic key-value entity profiles (e.g. user preferences, facts) across multi-turn conversations:

```haskell
entityMem <- newEntityMemory model
```

---

## Semantic & Exact LLM Caching

Avoid expensive redundant LLM API calls with deterministic response caching:

```haskell
-- In-Memory Cache
cache <- newInMemoryCache
let cachedModel = withCaching cache model

-- Persistent SQLite Cache
sqliteCache <- newSQLiteCache "llm_cache.db"
let persistentModel = withCaching sqliteCache model
```

When `invoke` or `streamModel` is called with identical messages and generation parameters, cached responses are returned instantly without hitting network APIs.

---
title: Memory Systems
description: Conversational memory buffers, STM transactional memory, multi-session management, and persistent storage.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Memory</span> <span class="badge badge-primary">Langchain.Memory.Variable</span> <span class="badge badge-primary">Langchain.Memory.Session</span> <span class="badge badge-primary">Langchain.Memory.File</span>
</div>

Conversational memory buffers and context tracking across interaction cycles.

## Key Concepts

- **Window Buffer Memory (`WindowBufferMemory`)**: Stores the most recent $k$ interaction turns with an optional initial system prompt, bounding context size to avoid context exhaustion.
- **STM Transactional Memory (`InVariableMemory`)**: Thread-safe transactional memory using `TVar` with atomic snapshots, rollback, and reset capabilities.
- **Multi-Session Manager (`SessionMemory`)**: Multi-user session router managing isolated per-session conversation contexts with automatic TTL pruning.
- **Persistent Memory (`FileMemory`)**: Persistent file-backed conversation storage with atomic disk writes, metadata preservation, and cross-restart continuity.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="Ollama (Local)" data-exe="stack run memoryollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Memory (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  mem <- newWindowBufferMemory 5 [systemMessage "You are a helpful assistant."]
  res <- runExceptT $ do
    chat_ o mem "Hi, my name is Alice."
    chat_ o mem "What is my name?"
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right _ -> pure ()

chat_ :: Ollama -> WindowBufferMemory -> Text -> ExceptT LangchainError IO ()
chat_ model_ mem prompt = do
  addUserMessage mem prompt
  history <- messages mem
  resp <- invoke model_ history Nothing
  let answer = extractMessageText resp
  addAiMessage mem answer
  liftIO $ do
    T.putStrLn $ "User: " <> prompt
    T.putStrLn $ "AI: " <> answer
```
:::

::: {.provider-panel data-provider="openai" data-label="OpenAI / OpenRouter" data-exe="stack run memoryopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Memory (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  mem <- newWindowBufferMemory 5 [systemMessage "You are a helpful assistant."]
  res <- runExceptT $ do
    chat_ o mem "Hi, my name is Alice."
    chat_ o mem "What is my name?"
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right _ -> pure ()

chat_ :: OpenAI -> WindowBufferMemory -> Text -> ExceptT LangchainError IO ()
chat_ model_ mem prompt = do
  addUserMessage mem prompt
  history <- messages mem
  resp <- invoke model_ history Nothing
  let answer = extractMessageText resp
  addAiMessage mem answer
  liftIO $ do
    T.putStrLn $ "User: " <> prompt
    T.putStrLn $ "AI: " <> answer
```
:::
:::

## Expected Output

```text
User: Hi, my name is Alice.
AI: Hello Alice! How can I help you today?
User: What is my name?
AI: Your name is Alice, as you mentioned earlier. How can I assist you further?
```

## Production Memory Backends

`langchain-hs` provides multiple production-grade memory implementations:

### STM Transactional Memory (`InVariableMemory`)

```haskell
import Langchain.Memory.Variable

-- Create thread-safe in-memory storage backed by TVar
mem <- newInVariableMemory [systemMessage "System initialized."]

-- Atomically snapshot memory state in STM
snapshot <- atomically $ snapshotInVariableMemory mem

-- Atomically reset memory state in STM
atomically $ resetInVariableMemorySTM mem [systemMessage "Reset to baseline."]
```

### Multi-Session Management (`SessionMemory`)

```haskell
import Langchain.Memory.Session

-- Create multi-session coordinator with default factory
mgr <- newSessionMemory (newWindowBufferMemory 10 [])

-- Retrieve or create isolated session for a user/channel
sessionMem <- getOrCreateSession mgr "user-session-1234"

-- Prune inactive sessions older than 3600 seconds
prunedCount <- pruneExpiredSessions mgr 3600
```

### Persistent File Memory (`FileMemory`)

```haskell
import Langchain.Memory.File

-- Persistent conversation history stored to disk atomically
fileMem <- newFileMemory "./conversations/session_123.json" [systemMessage "Durable state."]
```

## Running This Example

### Local Ollama
```bash
ollama run gemma3
stack run memoryollama
```

### OpenAI / OpenRouter
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run memoryopenai
```

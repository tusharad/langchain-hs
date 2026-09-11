---
title: Memory Systems
description: Conversational memory buffers and context tracking across interaction cycles.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Memory.Class</span> <span class="badge badge-primary">Langchain.Memory.Buffer</span>
</div>

Conversational memory buffers and context tracking across interaction cycles.

## Key Concepts

- **Buffer Memory**: Store ordered user and AI turns to provide continuous context for multi-turn chats.
- **Context Injection**: Automatically extract memory variables and inject them into prompt templates.
- **Window Management**: Bound conversation length to stay within token limits.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run memoryollama"}
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

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run memoryopenai"}
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

## Core Types & Functions

```haskell
Memory m => m -> IO (Map Text Value)
```
```haskell
Memory m => m -> Map Text Text -> Map Text Text -> IO ()
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run memoryollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run memoryopenai
```

---
title: Chat Models
description: Fundamental chat model interfaces for invocation, message generation, batching, and configuration.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Core.Model</span> <span class="badge badge-primary">Langchain.Provider.Ollama</span> <span class="badge badge-primary">Langchain.Provider.OpenAI</span>
</div>

Fundamental chat model interfaces for invocation, message generation, batching, and configuration.

## Key Concepts

- **ChatModel Abstraction**: Unified interface for interacting with LLM providers supporting invocation, batch requests, and parameter customization.
- **Multi-Turn Messages**: Structured message sequence (`userMessage`, `assistantMessage`, `systemMessage`) representing conversation history.
- **Batch Processing**: Execute multiple prompt collections concurrently with optimal throughput.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run simpleollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Simple (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt

runPromptTemplateExample :: IO ()
runPromptTemplateExample = do
  let items = Map.fromList [("name", "John"), ("age", "25")]
  let inputText = "{name} is of age of {age}, he is only {age}!"
  case renderFStringTemplate items inputText of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn r

runApp :: IO ()
runApp = do
  runPromptTemplateExample
  let prompt = "Write a poem about functional programming"
  let splittedChars = splitTextRecursive defaultRecursiveCharacterSplitterOps prompt
  mapM_ (T.putStrLn . T.toStrict) splittedChars
  o <- newOllama "gemma3" defaultConfig
  let msg = [(userMessage . T.toStrict) prompt]
  res <- runExceptT $ invoke o msg Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  let reqWithOptions =
        withOptions
          (defaultOptions {optTemperature = Just 0.7, optNumCtx = Just 4096})
          (chatRequestFor o msg)
  resWithOptions <- runExceptT $ invoke o msg (Just reqWithOptions)
  case resWithOptions of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  let ques =
        [ "What is Functor in Haskell?"
        , "What is applicative in Haskell?"
        , "What is Monad in Haskell?"
        , "A really long question"
        ]
      msgs = map (\q -> [userMessage q]) ques
  batchRes <- runExceptT $ batch o (take 3 msgs) Nothing
  case batchRes of
    Left err -> T.putStrLn $ errorMessage err
    Right ms -> mapM_ (T.putStrLn . extractMessageText) ms
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run simpleopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Simple (runApp) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (object, (.=))
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runPromptTemplateExample :: IO ()
runPromptTemplateExample = do
  let items = Map.fromList [("name", "John"), ("age", "25")]
  let inputText = "{name} is of age of {age}, he is only {age}!"
  case renderFStringTemplate items inputText of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn r

runApp :: IO ()
runApp = do
  runPromptTemplateExample
  let prompt = "Write a poem about functional programming"
  let splittedChars = splitTextRecursive defaultRecursiveCharacterSplitterOps prompt
  mapM_ (T.putStrLn . T.toStrict) splittedChars
  o <- getOpenRouterModel defaultModelName
  let msg = [(userMessage . T.toStrict) prompt]
  res <- runExceptT $ invoke o msg Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  let reqWithOptions =
        object
          [ "temperature" .= (0.7 :: Double)
          , "max_tokens" .= (1024 :: Int)
          ]
  resWithOptions <- runExceptT $ invoke o msg (Just reqWithOptions)
  case resWithOptions of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  let ques =
        [ "What is Functor in Haskell?"
        , "What is applicative in Haskell?"
        , "What is Monad in Haskell?"
        , "A really long question"
        ]
      msgs = map (\q -> [userMessage q]) ques
  batchRes <- runExceptT $ batch o (take 3 msgs) Nothing
  case batchRes of
    Left err -> T.putStrLn $ errorMessage err
    Right ms -> mapM_ (T.putStrLn . extractMessageText) ms
```
:::
:::

## Core Types & Functions

```haskell
ChatModel m => m -> [Message] -> Maybe RequestOptions -> ExceptT LangchainError IO Message
```
```haskell
ChatModel m => m -> [[Message]] -> Maybe RequestOptions -> ExceptT LangchainError IO [Message]
```
```haskell
Text -> Message
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run simpleollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run simpleopenai
```

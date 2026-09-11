---
title: Langchain Monad
description: The LangchainT monad transformer providing unified environment configuration and error management.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Core.Monad</span> <span class="badge badge-primary">Control.Monad.Reader</span>
</div>

The LangchainT monad transformer providing unified environment configuration and error management.

## Key Concepts

- **LangchainT Transformer**: Newtype wrapper around `ReaderT env (ExceptT LangchainError m) a` for compositional execution.
- **Decoupled Environment**: Parametric over `env`, allowing providers and end-developers to supply their own specialized configs.
- **Typeclass Compatibility**: Implements `MonadReader`, `MonadError`, `MonadIO`, and `MonadTrans` for smooth integration with your existing application monad stack.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run monadollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Monad (runApp) where

import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Provider.Ollama

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runLangchainT () $ do
    let chatReq =
          withOptions
            (defaultOptions {optTemperature = Just 0.7, optTopP = Just 0.9, optNumCtx = Just 100096})
            (chatRequestFor o msg)
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run monadopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Monad (runApp) where

import Data.Aeson (object, (.=))
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runLangchainT () $ do
    let chatReq =
          object
            [ "temperature" .= (0.7 :: Double)
            , "top_p" .= (0.9 :: Double)
            , "max_tokens" .= (2048 :: Int)
            ]
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m
```
:::
:::

## Core Types & Functions

```haskell
LangchainT { runLangchainT :: ReaderT env (ExceptT LangchainError m) a }
```
```haskell
env -> LangchainT env m a -> m (Either LangchainError a)
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run monadollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run monadopenai
```

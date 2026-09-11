---
title: Resilience & Fault Tolerance
description: Exponential backoff, jittered retries, fallback models, and circuit breaker patterns.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Resilience</span>
</div>

Exponential backoff, jittered retries, fallback models, and circuit breaker patterns.

## Key Concepts

- **Retry Policies**: Configurable retry strategies with exponential backoff and randomized jitter.
- **Fallback Providers**: Seamlessly failover from a primary provider to a secondary backup upon rate limits or outages.
- **Circuit Breakers**: Prevent cascading failures by failing fast when downstream services become degraded.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run resilienceollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Resilience (runApp) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  cache <- newInMemoryCache
  cb <- newCircuitBreaker "OllamaBreaker" defaultCircuitConfig
  limiter <- newRateLimiter 5 2
  o <- newOllama "qwen3.5:2b" defaultConfig
  let cachedModel = withCaching o cache
      query =
        "List 3 core Haskell language features as a comma-separated list. Output only the comma-separated items, nothing else."

  res <- runExceptT $ do
    firstResult <- askWithResilience cb limiter cachedModel query
    liftIO $ do
      T.putStrLn "First call (cache miss):"
      printItems firstResult

    secondResult <- askWithResilience cb limiter cachedModel query
    liftIO $ do
      T.putStrLn "\nSecond call (cache hit):"
      printItems secondResult

  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right () -> pure ()

askWithResilience ::
  CircuitBreaker ->
  RateLimiter ->
  CachedModel Ollama InMemoryCache ->
  Text ->
  ExceptT LangchainError IO [Text]
askWithResilience cb limiter model_ prompt =
  withCircuitBreaker cb $
    withRetry defaultRetryPolicy $
      withRateLimit limiter $ do
        resp <- invoke model_ [userMessage prompt] Nothing
        case parse (extractMessageText resp) of
          Left err -> throwError err
          Right (CommaSeparatedList items) -> pure items

printItems :: [Text] -> IO ()
printItems = mapM_ (\item -> T.putStrLn $ "- " <> item)
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run resilienceopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Resilience (runApp) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  cache <- newInMemoryCache
  cb <- newCircuitBreaker "OpenAIBreaker" defaultCircuitConfig
  limiter <- newRateLimiter 5 2
  o <- getOpenRouterModel defaultModelName
  let cachedModel = withCaching o cache
      query =
        "List 3 core Haskell language features as a comma-separated list. Output only the comma-separated items, nothing else."

  res <- runExceptT $ do
    firstResult <- askWithResilience cb limiter cachedModel query
    liftIO $ do
      T.putStrLn "First call (cache miss):"
      printItems firstResult

    secondResult <- askWithResilience cb limiter cachedModel query
    liftIO $ do
      T.putStrLn "\nSecond call (cache hit):"
      printItems secondResult

  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right () -> pure ()

askWithResilience ::
  CircuitBreaker ->
  RateLimiter ->
  CachedModel OpenAI InMemoryCache ->
  Text ->
  ExceptT LangchainError IO [Text]
askWithResilience cb limiter model_ prompt =
  withCircuitBreaker cb $
    withRetry defaultRetryPolicy $
      withRateLimit limiter $ do
        resp <- invoke model_ [userMessage prompt] Nothing
        case parse (extractMessageText resp) of
          Left err -> throwError err
          Right (CommaSeparatedList items) -> pure items

printItems :: [Text] -> IO ()
printItems = mapM_ (\item -> T.putStrLn $ "- " <> item)
```
:::
:::

## Core Types & Functions

```haskell
RetryConfig -> IO (Either LangchainError a) -> IO (Either LangchainError a)
```
```haskell
IO (Either LangchainError a) -> IO (Either LangchainError a) -> IO (Either LangchainError a)
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run resilienceollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run resilienceopenai
```

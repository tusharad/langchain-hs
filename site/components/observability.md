---
title: Observability & Tracing
description: Telemetry callbacks, latency measurement, token usage tracking, and audit trails.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Observability.Class</span> <span class="badge badge-primary">Langchain.Observability.Simple</span>
</div>

Telemetry callbacks, latency measurement, token usage tracking, and audit trails.

## Key Concepts

- **Callback Hooks**: Attach event handlers for `onLLMStart`, `onLLMEnd`, `onToolStart`, and `onError`.
- **Performance Telemetry**: Accurately capture latency distributions, time-to-first-token, and execution durations.
- **OpenTelemetry Ready**: Export structured traces and spans to Jaeger, Datadog, or Honeycomb.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run observabilityollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Observability (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  tracer <- newOTelTracer (Just "trace-ollama")
  res <- runExceptT $ do
    chat_ o tracer "Why is functional programming useful?"
    spans <- getSpans tracer
    liftIO $ mapM_ printSpan spans
    json <- exportSpansJson tracer
    liftIO $ do
      T.putStrLn "Spans JSON:"
      T.putStrLn json
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right _ -> pure ()

chat_ :: Ollama -> OTelTracer -> Text -> ExceptT LangchainError IO ()
chat_ model_ tracer prompt = do
  sp <-
    startSpan
      tracer
      "llm_invoke"
      Nothing
      ClientSpan
      ( Map.fromList
          [ ("provider", "ollama")
          , ("model", "gemma3")
          , ("input_length", T.pack (show (T.length prompt)))
          ]
      )
  res <- invoke model_ [userMessage prompt] Nothing
  let answer = extractMessageText res
  addSpanAttribute tracer (spanId sp) "output_length" (T.pack (show (T.length answer)))
  endSpan tracer (spanId sp) StatusOk
  liftIO $ T.putStrLn $ "AI: " <> answer

printSpan :: Span -> IO ()
printSpan sp = do
  T.putStrLn $ "Span: " <> spanName sp <> " (" <> spanId sp <> ")"
  T.putStrLn $ "Trace ID: " <> spanTraceId sp
  T.putStrLn $ "Duration: " <> maybe "0" (T.pack . show) (spanDurationMicros sp) <> "us"
  mapM_ (\(k, v) -> T.putStrLn $ "  " <> k <> ": " <> v) (Map.toList (spanAttributes sp))
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run observabilityopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Observability (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  tracer <- newOTelTracer (Just "trace-openai")
  res <- runExceptT $ do
    chat_ o tracer "Why is functional programming useful?"
    spans <- getSpans tracer
    liftIO $ mapM_ printSpan spans
    json <- exportSpansJson tracer
    liftIO $ do
      T.putStrLn "Spans JSON:"
      T.putStrLn json
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right _ -> pure ()

chat_ :: OpenAI -> OTelTracer -> Text -> ExceptT LangchainError IO ()
chat_ model_ tracer prompt = do
  sp <-
    startSpan
      tracer
      "llm_invoke"
      Nothing
      ClientSpan
      ( Map.fromList
          [ ("provider", "openai-compatible")
          , ("model", defaultModelName)
          , ("input_length", T.pack (show (T.length prompt)))
          ]
      )
  res <- invoke model_ [userMessage prompt] Nothing
  let answer = extractMessageText res
  addSpanAttribute tracer (spanId sp) "output_length" (T.pack (show (T.length answer)))
  endSpan tracer (spanId sp) StatusOk
  liftIO $ T.putStrLn $ "AI: " <> answer

printSpan :: Span -> IO ()
printSpan sp = do
  T.putStrLn $ "Span: " <> spanName sp <> " (" <> spanId sp <> ")"
  T.putStrLn $ "Trace ID: " <> spanTraceId sp
  T.putStrLn $ "Duration: " <> maybe "0" (T.pack . show) (spanDurationMicros sp) <> "us"
  mapM_ (\(k, v) -> T.putStrLn $ "  " <> k <> ": " <> v) (Map.toList (spanAttributes sp))
```
:::
:::

## Core Types & Functions

```haskell
data CallbackHandler = CallbackHandler { onStart :: Text -> IO (), onEnd :: Text -> IO (), onError :: Text -> IO () }
```
```haskell
[CallbackHandler] -> IO a -> IO a
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run observabilityollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run observabilityopenai
```

---
title: Guardrails & Safety
description: Input validation, output moderation, PII redaction, and semantic policy enforcement.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Guardrails</span>
</div>

Input validation, output moderation, PII redaction, and semantic policy enforcement.

## Key Concepts

- **Pre-Execution Guardrails**: Inspect and sanitize user inputs before dispatching requests to LLMs (e.g. blocking prompt injections).
- **Post-Execution Guardrails**: Verify generated responses against safety rules, redact PII, or check for banned tokens.
- **Composable Filters**: Combine multiple pure and monadic validation rules with standard Haskell combinators.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run guardrailollama"}
```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Guardrail (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  let rail =
        composeGuardrails
          [ contentSafetyGuardrail ["hack", "exploit", "password"]
          , outputLengthGuardrail 500
          ]
  ask_ o rail "Explain pure functions in Haskell in 2 sentences."
  ask_ o rail "How to hack into a system?"

ask_ :: Ollama -> Guardrail (ExceptT LangchainError IO) -> Text -> IO ()
ask_ model_ rail prompt = do
  res <- runExceptT $ withGuardrails rail action prompt
  case res of
    Left err -> T.putStrLn $ "Blocked: " <> errorMessage err
    Right ans -> T.putStrLn $ "AI: " <> ans
  where
    action :: Text -> ExceptT LangchainError IO Text
    action q = do
      resp <- invoke model_ [userMessage q] Nothing
      pure (extractMessageText resp)
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run guardrailopenai"}
```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Guardrail (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let rail =
        composeGuardrails
          [ contentSafetyGuardrail ["hack", "exploit", "password"]
          , outputLengthGuardrail 500
          ]
  ask_ o rail "Explain pure functions in Haskell in 2 sentences."
  ask_ o rail "How to hack into a system?"

ask_ :: OpenAI -> Guardrail (ExceptT LangchainError IO) -> Text -> IO ()
ask_ model_ rail prompt = do
  res <- runExceptT $ withGuardrails rail action prompt
  case res of
    Left err -> T.putStrLn $ "Blocked: " <> errorMessage err
    Right ans -> T.putStrLn $ "AI: " <> ans
  where
    action :: Text -> ExceptT LangchainError IO Text
    action q = do
      resp <- invoke model_ [userMessage q] Nothing
      pure (extractMessageText resp)
```
:::
:::

## Core Types & Functions

```haskell
type Guardrail m a = a -> m (Either GuardrailViolation a)
```
```haskell
Guardrail m a -> a -> m (Either GuardrailViolation a)
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run guardrailollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run guardrailopenai
```

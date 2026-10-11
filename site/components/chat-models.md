---
title: Chat Models
description: Fundamental chat model interfaces for invocation, message generation, batching, and configuration.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Model</span> <span class="badge badge-primary">Langchain.Provider.Ollama</span> <span class="badge badge-primary">Langchain.Provider.OpenAI</span> <span class="badge badge-primary">Langchain.Provider.Gemini</span>
</div>

Fundamental chat model interfaces for invocation, message generation, batching, and configuration.

## Key Concepts

- **ChatModel Abstraction**: Unified interface for interacting with LLM providers supporting invocation, batch requests, and parameter customization.
- **Multi-Turn Messages**: Structured message sequence (`userMessage`, `assistantMessage`, `systemMessage`) representing conversation history.
- **Batch Processing**: Execute multiple prompt collections concurrently with optimal throughput.

## Working Code Example

Compare local execution via **Ollama**, cloud API execution via **OpenAI / OpenRouter**, and Google's **Gemini Interactions v1 API**. Use the toggle tabs or the global provider switcher in the header to switch:

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

  let cfgWithOptions =
        withOptions
          (defaultOptions {optTemperature = Just 0.7, optNumCtx = Just 4096})
          defaultOllamaConfig
  resWithOptions <- runExceptT $ invoke o msg (Just cfgWithOptions)
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

::: {.provider-panel data-provider="gemini" data-label="✨ Google Gemini" data-exe="cabal run simplegemini"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Gemini.Simple (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Gemini.Client (GenerationConfig (..))
import Gemini.Common (defaultGeminiModelName, getGeminiModel)
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt
import Langchain.Provider.Gemini (GeminiConfig (..), defaultGeminiConfig)

runPromptTemplateExample :: IO ()
runPromptTemplateExample = do
  let items = Map.fromList [("name", "Ada Lovelace"), ("field", "Computer Science")]
  let inputText = "{name} made pioneering contributions to {field}!"
  case renderFStringTemplate items inputText of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn r

runApp :: IO ()
runApp = do
  T.putStrLn "--- Gemini Prompt Template ---"
  runPromptTemplateExample

  let prompt = "Explain the difference between pure and impure functions in 2 sentences"
  let splittedChars = splitTextRecursive defaultRecursiveCharacterSplitterOps prompt
  mapM_ (T.putStrLn . T.toStrict) splittedChars

  T.putStrLn "\n--- Gemini Synchronous Invocation ---"
  g <- getGeminiModel defaultGeminiModelName
  let msg = [(userMessage . T.toStrict) prompt]
  res <- runExceptT $ invoke g msg Nothing
  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  T.putStrLn "\n--- Gemini With System Instruction & Generation Config ---"
  let customCfg =
        defaultGeminiConfig
          { configSystemInstruction = Just "You are a concise Haskell tutor."
          , configGenerationConfig =
              Just
                GenerationConfig
                  { maxOutputTokens = Just 128
                  , seed = Just 42
                  , stopSequences = Nothing
                  , thinkingLevel = Nothing
                  , thinkingSummaries = Nothing
                  , toolChoice = Nothing
                  }
          }
  resConfig <- runExceptT $ invoke g msg (Just customCfg)
  case resConfig of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  T.putStrLn "\n--- Gemini Batch Processing ---"
  let questions =
        [ "What is a Functor in Haskell in one sentence?"
        , "What is a Monad in Haskell in one sentence?"
        ]
      msgs = map (\q -> [userMessage q]) questions
  batchRes <- runExceptT $ batch g msgs Nothing
  case batchRes of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right ms -> mapM_ (\m -> T.putStrLn $ "- " <> extractMessageText m) ms
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

### Google Gemini
Set your `GEMINI_API_KEY` in environment or store in `~/.gemini_api_key`:
```bash
export GEMINI_API_KEY="AIzaSy..."
cabal run simplegemini
```

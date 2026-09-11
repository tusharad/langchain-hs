---
title: Structured Outputs
description: Extracting typed, validated JSON structures matching Haskell data types via Aeson.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.OutputParser</span> <span class="badge badge-primary">Data.Aeson</span>
</div>

Extracting typed, validated JSON structures matching Haskell data types via Aeson.

## Key Concepts

- **Schema Enforcement**: Supply JSON Schema definitions or format instructions ensuring the model responds strictly in valid JSON.
- **Aeson Integration**: Deserialize outputs directly into Haskell records using standard `FromJSON` instances with total type safety.
- **Self-Correction & Parsing**: Handle malformed outputs gracefully with typed error reporting.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run jsonollama"}
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ollama.StructuredOutput (runApp) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import Langchain.Prelude
import Langchain.Provider.Ollama

data Person = Person
  { name :: T.Text
  , age :: Int
  , location :: T.Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToSchema)

inputPrompt :: T.Text
inputPrompt =
  T.unlines
    [ "For the given below information, extract information about Jesse."
    , "the 24 year old Jesse was staying in New York due to his work."
    ]

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  let msg = [userMessage inputPrompt]
  let chatReq = withStructuredOutput @Person (chatRequestFor o msg)
  res <- runLangchainT () $ do
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run jsonopenai"}
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpenAI.StructuredOutput (runApp) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

data Person = Person
  { name :: T.Text
  , age :: Int
  , location :: T.Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, StructuredOutput)

inputPrompt :: T.Text
inputPrompt =
  T.unlines
    [ "For the given below information, extract information about Jesse."
    , "the 24 year old Jesse was staying in New York due to his work."
    ]

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let msg = [userMessage inputPrompt]
  let rawSchema = outputSchema (Proxy @Person)
      schema = case rawSchema of
        Object km -> Object (KM.insert "additionalProperties" (Bool False) km)
        other -> other
      chatReq =
        object
          [ "response_format"
              .= object
                [ "type" .= ("json_schema" :: T.Text)
                , "json_schema"
                    .= object
                      [ "name" .= ("person" :: T.Text)
                      , "strict" .= True
                      , "schema" .= schema
                      ]
                ]
          ]
  res <- runLangchainT () $ do
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
```
:::
:::

## Core Types & Functions

```haskell
FromJSON a => Value -> Result a
```
```haskell
LLM response mapped into strongly typed data models
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run jsonollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run jsonopenai
```

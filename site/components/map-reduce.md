---
title: Map-Reduce Document Processing
description: Hierarchical summarization and chunk reduction over large text corpora.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Chain.MapReduce</span>
</div>

Hierarchical summarization and chunk reduction over large text corpora.

## Key Concepts

- **Map Stage**: Process individual document segments or chapters concurrently using LLM summarization.
- **Reduce Stage**: Combine partial summaries into a final synthesis, recursively condensing if necessary.
- **Scalable Document Handling**: Process books, transcripts, and repositories without exceeding model token limits.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run mapreduceollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.MapReduce (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  let docs =
        [ Document
            "Haskell uses lazy evaluation, deferring expression evaluation until values are explicitly required."
            mempty
        , Document
            "The Haskell type system enforces strong static typing with powerful global type inference."
            mempty
        , Document
            "Immutability is default across Haskell data structures, preventing hidden state mutations."
            mempty
        ]
      embed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing Nothing
      dbPath = "/tmp/map_reduce.db"

  res <- runExceptT $ do
    store_ <- newSqliteVecStore dbPath embed
    _ <- addDocuments store_ docs
    retrievedDocs <- similaritySearch store_ "core Haskell features" 3
    o <- newOllama "qwen3.5:2b" defaultConfig
    let chain = newMapReduceChain o
    resp <- runMapReduceChain chain retrievedDocs Map.empty
    pure (extractMessageText resp)

  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run mapreduceopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.MapReduce (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterEmbeddings, getOpenRouterModel)

runApp :: IO ()
runApp = do
  let docs =
        [ Document
            "Haskell uses lazy evaluation, deferring expression evaluation until values are explicitly required."
            mempty
        , Document
            "The Haskell type system enforces strong static typing with powerful global type inference."
            mempty
        , Document
            "Immutability is default across Haskell data structures, preventing hidden state mutations."
            mempty
        ]
      dbPath = "/tmp/map_reduce_openai.db"

  res <- runExceptT $ do
    embed <- liftIO $ getOpenRouterEmbeddings "text-embedding-3-small"
    store_ <- newSqliteVecStore dbPath embed
    _ <- addDocuments store_ docs
    retrievedDocs <- similaritySearch store_ "core Haskell features" 3
    o <- liftIO $ getOpenRouterModel defaultModelName
    let chain = newMapReduceChain o
    resp <- runMapReduceChain chain retrievedDocs Map.empty
    pure (extractMessageText resp)

  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
```
:::
:::

## Core Types & Functions

```haskell
ChatModel m => m -> [Document] -> Text -> IO (Either LangchainError Text)
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run mapreduceollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run mapreduceopenai
```

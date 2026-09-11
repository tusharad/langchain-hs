---
title: Retrieval QA Chains
description: End-to-end question answering synthesizing document retrieval and model generation.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Chain.RetrievalQA</span>
</div>

End-to-end question answering synthesizing document retrieval and model generation.

## Key Concepts

- **Unified Retrieval Pipeline**: Coordinate retrieval of relevant chunks and synthesize them into a grounded response.
- **Context Formatting**: Format retrieved chunks cleanly into the LLM prompt instructions.
- **Zero Hallucination Grounding**: Constrain LLM answers to verifiable facts found within the retrieved context.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run retrievalqaollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.RetrievalQA (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  let docs =
        [ Document
            "Pure functions in Haskell return identical outputs for identical inputs and have no side effects."
            mempty
        , Document
            "Typeclasses provide ad-hoc polymorphism, allowing functions to operate on different types."
            mempty
        , Document
            "Monads structure computations as sequences of steps while isolating effects like state or IO."
            mempty
        ]
      embed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing Nothing
      dbPath = "/tmp/retrieval_qa.db"

  res <- runExceptT $ do
    store_ <- newSqliteVecStore dbPath embed
    _ <- addDocuments store_ docs
    let retriever = VectorStoreRetriever store_
    o <- newOllama "qwen3.5:2b" defaultConfig
    let qa = newRetrievalQA o retriever
    resp <- runRetrievalQA qa "What is a pure function in Haskell?"
    pure (extractMessageText resp)

  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run retrievalqaopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.RetrievalQA (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterEmbeddings, getOpenRouterModel)

runApp :: IO ()
runApp = do
  let docs =
        [ Document
            "Pure functions in Haskell return identical outputs for identical inputs and have no side effects."
            mempty
        , Document
            "Typeclasses provide ad-hoc polymorphism, allowing functions to operate on different types."
            mempty
        , Document
            "Monads structure computations as sequences of steps while isolating effects like state or IO."
            mempty
        ]
      dbPath = "/tmp/retrieval_qa_openai.db"

  res <- runExceptT $ do
    embed <- liftIO $ getOpenRouterEmbeddings "text-embedding-3-small"
    store_ <- newSqliteVecStore dbPath embed
    _ <- addDocuments store_ docs
    let retriever = VectorStoreRetriever store_
    o <- liftIO $ getOpenRouterModel defaultModelName
    let qa = newRetrievalQA o retriever
    resp <- runRetrievalQA qa "What is a pure function in Haskell?"
    pure (extractMessageText resp)

  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
```
:::
:::

## Core Types & Functions

```haskell
Retriever r => ChatModel m => m -> r -> Text -> IO (Either LangchainError Text)
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run retrievalqaollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run retrievalqaopenai
```

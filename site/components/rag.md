---
title: RAG & Embeddings
description: Vector embeddings, cosine similarity calculation, and document chunking for retrieval.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Embeddings.Core</span> <span class="badge badge-primary">Langchain.VectorStore.Core</span>
</div>

Vector embeddings, cosine similarity calculation, and document chunking for retrieval.

## Key Concepts

- **Embeddings Abstraction**: Generate dense semantic vectors from text using local models (e.g. `nomic-embed-text`) or cloud embeddings (`text-embedding-3-small`).
- **Document Chunking**: Split large documents using recursive character splitters preserving semantic boundaries.
- **Similarity Search**: Compute cosine similarity over vector collections to identify top-K most relevant chunks.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="Ollama (Local)" data-exe="stack run ragollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.RAG (runApp) where

import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Langchain.Embeddings.Ollama
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt

runApp :: IO ()
runApp = do
  let systemPrompt = "Answer the question based on the Context provided it to you."
      userQuestion = "Compare the feautres of Langchain-Rust and Langchain-Haskell."
  let fPath = FileLoader "/Users/tusharadhatrao/work/langchain-clients/langchain-hs/README.md"
  res <- runLangchainT () $ do
    docs <- load fPath
    let ollamaEmbed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing Nothing
    vs1 <- fromDocuments ollamaEmbed docs
    relevantDocs <- similaritySearch vs1 userQuestion 1
    o <- newOllama "qwen3.5:2b" defaultConfig
    let ragTemplate = "{userQuestion} CONTEXT: {context}"
    let x = (T.toStrict . pageContent) $ mconcat relevantDocs
    let vars = Map.fromList [("userQuestion", userQuestion), ("context", x)]
    let eFinalQ = renderFStringTemplate vars ragTemplate
    case eFinalQ of
      Right finalQ -> do
        let msgs =
              zipWith
                id
                [systemMessage, userMessage]
                [systemPrompt, finalQ]
        let cfg = withOptions (defaultOptions {optNumCtx = Just 100096}) defaultOllamaConfig
        invoke o msgs (Just cfg)
      Left _ -> throwError $ internalError "Rendering of vars failed" Nothing Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
```
:::

::: {.provider-panel data-provider="openai" data-label="OpenAI / OpenRouter" data-exe="stack run ragopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.RAG (runApp) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt
import OpenAI.Common (defaultModelName, getOpenRouterEmbeddings, getOpenRouterModel)

runApp :: IO ()
runApp = do
  let systemPrompt = "Answer the question based on the Context provided it to you."
      userQuestion = "Compare the features of Langchain-Rust and Langchain-Haskell."
  let fPath = FileLoader "README.md"
  res <- runLangchainT () $ do
    docs <- load fPath
    openAIEmbed <- liftIO $ getOpenRouterEmbeddings "text-embedding-3-small"
    vs1 <- fromDocuments openAIEmbed docs
    relevantDocs <- similaritySearch vs1 userQuestion 1
    o <- liftIO $ getOpenRouterModel defaultModelName
    let ragTemplate = "{userQuestion} CONTEXT: {context}"
    let x = (T.toStrict . pageContent) $ mconcat relevantDocs
    let vars = Map.fromList [("userQuestion", userQuestion), ("context", x)]
    let eFinalQ = renderFStringTemplate vars ragTemplate
    case eFinalQ of
      Right finalQ -> do
        let msgs =
              zipWith
                id
                [systemMessage, userMessage]
                [systemPrompt, finalQ]
        invoke o msgs Nothing
      Left _ -> throwError $ internalError "Rendering of vars failed" Nothing Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
```
::: {.provider-panel data-provider="gemini" data-label="Google Gemini" data-exe="cabal run raggemini"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Gemini.RAG (runApp) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Gemini.Common (defaultGeminiModelName, getGeminiEmbeddings, getGeminiModel)
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt

runApp :: IO ()
runApp = do
  T.putStrLn "--- Gemini RAG with GeminiEmbeddings ---"
  let systemPrompt = "Answer the question based on the Context provided to you."
      userQuestion = "What are the features of LangChain Haskell?"
  let fPath = FileLoader "README.md"
  res <- runLangchainT () $ do
    docs <- load fPath
    geminiEmbed <- liftIO getGeminiEmbeddings
    vs1 <- fromDocuments geminiEmbed docs
    relevantDocs <- similaritySearch vs1 userQuestion 1
    g <- liftIO $ getGeminiModel defaultGeminiModelName
    let ragTemplate = "{userQuestion} CONTEXT: {context}"
    let x = (T.toStrict . pageContent) $ mconcat relevantDocs
    let vars = Map.fromList [("userQuestion", userQuestion), ("context", x)]
    let eFinalQ = renderFStringTemplate vars ragTemplate
    case eFinalQ of
      Right finalQ -> do
        let msgs =
              zipWith
                id
                [systemMessage, userMessage]
                [systemPrompt, finalQ]
        invoke g msgs Nothing
      Left _ -> throwError $ internalError "Rendering of vars failed" Nothing Nothing
  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
```
:::
:::

## Core Types & Functions

```haskell
Embeddings e => e -> [Text] -> ExceptT LangchainError IO [[Double]]
```
```haskell
Embeddings e => e -> Text -> ExceptT LangchainError IO [Double]
```
```haskell
[Double] -> [Double] -> Double
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run ragollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run ragopenai
```

### Google Gemini
Set your `GEMINI_API_KEY` in environment or store in `~/.gemini_api_key`:
```bash
export GEMINI_API_KEY="AIzaSy..."
cabal run raggemini
```

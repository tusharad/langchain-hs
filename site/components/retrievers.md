---
title: Retrievers & Ensembles
description: Combining BM25 keyword search, vector stores, and Reciprocal Rank Fusion (RRF).
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Retriever.Class</span> <span class="badge badge-primary">Langchain.Retriever.BM25</span> <span class="badge badge-primary">Langchain.Retriever.Ensemble</span>
</div>

Combining BM25 keyword search, vector stores, and Reciprocal Rank Fusion (RRF).

## Key Concepts

- **Retriever Typeclass**: Uniform interface `getRelevantDocuments` decoupling retrieval algorithms from consumption.
- **BM25 Keyword Matching**: Deterministic inverted-index keyword scoring for exact term matches and domain identifiers.
- **Ensemble Rank Fusion**: Reciprocal Rank Fusion (RRF) algorithm combining lexical and semantic results for superior accuracy.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run retrieverollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Retriever (runApp) where

import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Langchain.Prelude

runApp :: IO ()
runApp = do
  cb <- newCallbackManager
  (handler, logsVar) <- newLoggingCallbackHandler "RetrieverLogger"
  registerHandler cb handler

  let docs =
        [ Document
            "Haskell features pure functional programming, strong static typing, and immutability."
            mempty
        , Document "GHC-9.8 introduces improved compiler error messages and new typechecker features." mempty
        , Document "Reciprocal Rank Fusion fuses ranked results from sparse and dense retrievers." mempty
        ]
      bm25 = newBM25Index docs

  res <- runLangchainT () $ do
    let embed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing Nothing
    vecStore <- fromDocuments embed docs
    let vecSearch q k = fromRight [] <$> runLangchainT () (similaritySearch vecStore q k)
        hybrid = newHybridRetriever bm25 vecSearch

    matchedBM25 <- retrieveWithCallbacks cb "BM25" bm25 "GHC-9.8"
    liftIO $ T.putStrLn $ "BM25 match: " <> firstDoc matchedBM25

    matchedHybrid <- retrieveWithCallbacks cb "Hybrid" hybrid "pure functional language features"
    liftIO $ T.putStrLn $ "Hybrid match: " <> firstDoc matchedHybrid

    o <- newOllama "gemma3" defaultConfig
    ask_ o cb "Explain the compiler improvements in GHC-9.8." (firstDoc matchedBM25)

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()

  logs <- getCallbackLogs logsVar
  T.putStrLn "\n--- Callback Logs ---"
  mapM_ T.putStrLn logs

ask_ :: Ollama -> CallbackManager -> Text -> Text -> LangchainT () IO ()
ask_ llm cb query context = do
  start <- liftIO getCurrentTime
  let prompt = "Context: " <> context <> "\nQuestion: " <> query
  liftIO $ dispatchEvent cb (OnLLMStart "gemma3" [prompt] start)
  resp <- invoke llm [userMessage prompt] Nothing
  end <- liftIO getCurrentTime
  let durMicros = round (diffUTCTime end start * 1000000)
      ans = extractMessageText resp
  liftIO $ dispatchEvent cb (OnLLMEnd "gemma3" ans durMicros end)
  liftIO $ T.putStrLn $ "AI: " <> ans

firstDoc :: [Document] -> Text
firstDoc [] = ""
firstDoc (d : _) = TL.toStrict (pageContent d)
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run retrieveropenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Retriever (runApp) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterEmbeddings, getOpenRouterModel)

runApp :: IO ()
runApp = do
  cb <- newCallbackManager
  (handler, logsVar) <- newLoggingCallbackHandler "RetrieverLogger"
  registerHandler cb handler

  let docs =
        [ Document
            "Haskell features pure functional programming, strong static typing, and immutability."
            mempty
        , Document "GHC-9.8 introduces improved compiler error messages and new typechecker features." mempty
        , Document "Reciprocal Rank Fusion fuses ranked results from sparse and dense retrievers." mempty
        ]
      bm25 = newBM25Index docs

  res <- runLangchainT () $ do
    embed <- liftIO $ getOpenRouterEmbeddings "text-embedding-3-small"
    vecStore <- fromDocuments embed docs
    let vecSearch q k = fromRight [] <$> runLangchainT () (similaritySearch vecStore q k)
        hybrid = newHybridRetriever bm25 vecSearch

    matchedBM25 <- retrieveWithCallbacks cb "BM25" bm25 "GHC-9.8"
    liftIO $ T.putStrLn $ "BM25 match: " <> firstDoc matchedBM25

    matchedHybrid <- retrieveWithCallbacks cb "Hybrid" hybrid "pure functional language features"
    liftIO $ T.putStrLn $ "Hybrid match: " <> firstDoc matchedHybrid

    o <- liftIO $ getOpenRouterModel defaultModelName
    ask_ o cb "Explain the compiler improvements in GHC-9.8." (firstDoc matchedBM25)

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()

  logs <- getCallbackLogs logsVar
  T.putStrLn "\n--- Callback Logs ---"
  mapM_ T.putStrLn logs

ask_ :: OpenAI -> CallbackManager -> Text -> Text -> LangchainT () IO ()
ask_ llm cb query context = do
  start <- liftIO getCurrentTime
  let prompt = "Context: " <> context <> "\nQuestion: " <> query
  liftIO $ dispatchEvent cb (OnLLMStart defaultModelName [prompt] start)
  resp <- invoke llm [userMessage prompt] Nothing
  end <- liftIO getCurrentTime
  let durMicros = round (diffUTCTime end start * 1000000)
      ans = extractMessageText resp
  liftIO $ dispatchEvent cb (OnLLMEnd defaultModelName ans durMicros end)
  liftIO $ T.putStrLn $ "AI: " <> ans

firstDoc :: [Document] -> Text
firstDoc [] = ""
firstDoc (d : _) = TL.toStrict (pageContent d)
```
:::
:::

## Core Types & Functions

```haskell
Retriever r => r -> Text -> IO [Document]
```
```haskell
[[Document]] -> [Document]
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run retrieverollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run retrieveropenai
```

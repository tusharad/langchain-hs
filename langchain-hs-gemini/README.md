# langchain-hs-gemini

Google Gemini Interactions v1 API client and LangChain provider for [langchain-hs](https://hackage.haskell.org/package/langchain-hs).

Built using type-safe **Servant bindings** over the official Google GenAI OpenAPI specification ([Interactions v1 API](https://ai.google.dev/api/interactions-api-v1)), following the design patterns of [`MercuryTechnologies/openai`](https://github.com/MercuryTechnologies/openai).

## Features

- **Type-Safe Servant Bindings (`Gemini.Client`)**:
  - `createInteraction` (`POST /v1/interactions`)
  - `getInteraction` (`GET /v1/interactions/{id}`)
  - `cancelInteraction` (`POST /v1/interactions/{id}/cancel`)
  - `deleteInteraction` (`DELETE /v1/interactions/{id}`)
  - `embedContent` (`POST /v1/models/{model}:embedContent`)
  - `batchEmbedContents` (`POST /v1/models/{model}:batchEmbedContents`)
  - SSE Streaming (`streamGeminiInteraction`)
- **LangChain ChatModel Provider (`Langchain.Provider.Gemini`)**:
  - Full `ChatModel` instance supporting synchronous `invoke` and streaming `stream`.
  - Multi-turn conversation mapping and system instructions.
  - Multi-modal content support (Base64 images, audio, documents).
  - Function calling & tool binding via `ToolBinder`.
  - Built-in tools: Code Execution, Google Search, Google Maps, File Search, URL Context.
- **LangChain Embeddings (`Langchain.Embeddings.Gemini`)**:
  - Effect-polymorphic `Embeddings` instance (`embedQuery` and `embedDocuments`).

## Installation

```yaml
# package.yaml / .cabal
dependencies:
  - langchain-hs-gemini
  - langchain-hs-core
```

## Quick Start

### 1. ChatModel Provider

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)
import Langchain.Model
import Langchain.Provider.Gemini

main :: IO ()
main = do
  let provider = newGemini "AIzaSy..." "gemini-3.6-flash" Nothing
  res <- runExceptT $ invoke provider [userMessage "Why is the sky blue?"] Nothing
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right msg -> putStrLn $ extractMessageText msg
```

### 2. Streaming with Conduit

```haskell
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Except (runExceptT)
import Langchain.Model
import Langchain.Provider.Gemini
import Langchain.Stream (collectEvents)

main :: IO ()
main = do
  let provider = newGemini "AIzaSy..." "gemini-3.6-flash" Nothing
  eEvents <- runResourceT $ runExceptT $ collectEvents $ stream provider [userMessage "Tell me a short story"] Nothing
  case eEvents of
    Left err -> print err
    Right events -> mapM_ print events
```

### 3. Embeddings

```haskell
import Control.Monad.Except (runExceptT)
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.Embeddings.Gemini (defaultGeminiEmbeddings)

main :: IO ()
main = do
  let embeddings = defaultGeminiEmbeddings "AIzaSy..."
  res <- runExceptT $ embedQuery embeddings "Functional programming in Haskell"
  case res of
    Left err -> print err
    Right vec -> putStrLn $ "Generated vector with dimension: " ++ show (length vec)
```

### 4. Direct Servant Client

```haskell
import Gemini.Client

main :: IO ()
main = do
  env <- newGeminiClientEnv "AIzaSy..."
  let req = createModelInteraction "gemini-3.6-flash" (TextInput "Explain quantum computing in one sentence")
  res <- runGeminiClient env (createInteraction "AIzaSy..." req)
  case res of
    Left clientErr -> print clientErr
    Right interaction -> print (interactionSteps interaction)
```

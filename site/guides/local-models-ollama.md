---
title: Local AI with Ollama
description: Run local open-source models (DeepSeek-R1, Llama 3.2, Qwen 2.5, nomic-embed-text) privately and with zero API fees.
category: Guides & Recipes
---

## Why Local AI with Haskell?

Running models locally with [Ollama](https://ollama.com/) guarantees privacy, zero token costs, and offline reproducibility. `langchain-hs` provides native, first-class support for Ollama generation, chat, tool calling, and embeddings.

---

## 1. Setting Up Ollama

Install Ollama and pull your desired models:

```bash
# Pull LLM for reasoning and tool calling
ollama run qwen2.5:7b

# Pull lightweight model for quick classification / fast tests
ollama run qwen2.5:1.5b

# Pull dense text embedding model
ollama pull nomic-embed-text
```

---

## 2. Instantiating the Model

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as TIO
import Langchain.Prelude

main :: IO ()
main = do
  -- Connect to local Ollama daemon
  model <- newOllama "qwen2.5:7b" defaultConfig

  let prompt = [ userMessage "Write a pure Haskell function that computes Fibonacci numbers using zipWith." ]

  res <- runExceptT $ invoke model prompt
  case res of
    Left err -> putStrLn ("Error: " ++ show err)
    Right msg -> TIO.putStrLn (extractMessageText msg)
```

---

## 3. Local Embeddings with `nomic-embed-text`

```haskell
import Langchain.Embeddings.Core
import Langchain.Provider.Ollama

embedExample :: IO ()
embedExample = do
  let embedModel = OllamaEmbeddings "nomic-embed-text" "http://localhost:11434"
  
  -- Generate 768-dimensional float vectors
  vectors <- embedDocuments embedModel ["Haskell AST pipelines", "Category theory in computer science"]
  putStrLn ("Generated embeddings for " ++ show (length vectors) ++ " documents.")
```

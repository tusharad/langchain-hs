---
sidebar_position: 8
---

# VectorStore

VectorStore is a core component of Langchain that allows you to store and retrieve documents based on their vector representations. It is designed to work with various embedding models, enabling efficient similarity searches and retrieval of relevant documents.

## Supported Integrations
At this moment, following integrations available,

- InMemory
- More to come...

## InMemory VectorStore

```haskell
data Embeddings m => InMemory m = InMemory
  { embeddingModel :: m
  , store :: Map.Map Int64 (Document, [Float])
  }
  deriving (Show, Eq)
```

The `InMemory` vector store is a simple implementation that stores documents and their corresponding embeddings in memory. It uses a map to associate document IDs with their embeddings, allowing for efficient retrieval and similarity searches.

## Example 

Below is an example that:
 - Reads a 20 pages long pdf file and generates a list of documents.
 - Sets up an OllamaEmbeddings model.
 - Creates an InMemoryVectorStore with model and documents.
 - Fetches top 2 documents that are closest to the given query.

 ```haskell
 {-# LANGUAGE OverloadedStrings #-}

module VectorStoreExample (runApp) where

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.PdfLoader
import Langchain.Embeddings.Ollama
import Langchain.VectorStore.InMemory (fromDocuments)
import Langchain.VectorStore.Core (VectorStore(similaritySearch))

runApp :: IO ()
runApp = do
    let pdf = PdfLoader "/home/user/Documents/TS/langchain/SOP.pdf"
    eDocs <- load pdf
    case eDocs of
        Left err -> putStrLn $ "Error while reading docs: " <> err
        Right docs -> do
            let ollamaEmb =
                    OllamaEmbeddings
                        { model = "nomic-embed-text:latest"
                        , defaultTruncate = Just True
                        , defaultKeepAlive = Just "5m"
                        }
            eInMemVS <- fromDocuments ollamaEmb docs
            case eInMemVS of
                Left err -> putStrLn $ "Error while initiating Vector Store: " <> err
                Right inMemVS -> do
                    eRes <- similaritySearch inMemVS "I am having with accessing the vendor portal. I have been unable to log in to my account." 2
                    print eRes
 ```

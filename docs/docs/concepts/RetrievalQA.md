---
sidebar_position: 13
---

# RetrievalQA

Chain for question-answering against an index.

## Example 

```haskell
{-# LANGUAGE OverloadedStrings #-}

module RetrievalQAExample (runApp) where

import Langchain.Chain.RetrievalQA

import Data.Aeson
import Langchain.Callback (stdOutCallback)
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.PdfLoader
import Langchain.Embeddings.Ollama
import Langchain.LLM.Ollama (Ollama (..))
import Langchain.Retriever.Core (VectorStoreRetriever (..))
import Langchain.Runnable.Core (Runnable (invoke))
import Langchain.VectorStore.InMemory (fromDocuments)

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
                    let vsRetriever = VectorStoreRetriever inMemVS
                    let ollamaLLM =
                            Ollama
                                { modelName = "qwen3:4b"
                                , callbacks = [stdOutCallback]
                                }
                    let myQA =
                            RetrievalQA
                                { llm = ollamaLLM
                                , llmParams = Nothing
                                , retriever = vsRetriever
                                , prompt = defaultQAPrompt
                                }
                    eRes <-
                        invoke
                            myQA
                            "I am having with accessing the vendor portal. I have been unable to log in to my account. /no_think"
                    print eRes
```

## output 

```bash
Model operation started
Model completed with
Right "<think>\n\n</think>\n\nYou may need to check your login credentials or reset your password. If you still cannot access, contact the helpdesk for assistance. Vendor portal access is essential for tracking orders and payments."
```

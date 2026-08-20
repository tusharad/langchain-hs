---
sidebar_position: 3
---

# Embeddings models

`Embeddings` typeclass provides interface over various Embeddings models.

As of now, below models are provided out of the box:

- OllamaEmbeddings
- OpenAIEmbeddings

# Example for OpenAI Embeddings

```haskell 
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.Embeddings.OpenAI
import Langchain.Embeddings.Core
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.PdfLoader

runApp :: IO ()
runApp = do
  let oEmbed = defaultOpenAIEmbeddings {
    apiKey = "api-key"
  }
  let p = PdfLoader "/home/user/Documents/langchain/SOP.pdf"
  eDocs <- load p
  case eDocs of
    Left err -> error err
    Right docs -> do 
      eRes <- embedDocuments oEmbed docs
      print eRes
```

# Custom embedding model

It is also possible to create your own type and implement Embeddings typeclass.

for e.g

```haskell
data CustomEmbedding = CustomEmbedding {
  apiKey :: Text,
  apiUrl :: Text,
  model :: Text
}

instance Embeddings CustomEmbedding where
  embedDocuments CustomEmbedding{..} docs = do
    -- Your implementation here
    return $ Right []
  embedQuery CustomEmbedding{..} query = do
    -- Your implementation here
    return $ Right []
```

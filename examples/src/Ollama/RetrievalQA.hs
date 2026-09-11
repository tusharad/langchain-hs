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

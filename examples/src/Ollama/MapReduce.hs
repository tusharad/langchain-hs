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

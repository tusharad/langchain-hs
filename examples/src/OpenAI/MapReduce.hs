{-# LANGUAGE OverloadedStrings #-}

module OpenAI.MapReduce (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterEmbeddings, getOpenRouterModel)

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
      dbPath = "/tmp/map_reduce_openai.db"

  res <- runExceptT $ do
    embed <- liftIO $ getOpenRouterEmbeddings "text-embedding-3-small"
    store_ <- newSqliteVecStore dbPath embed
    _ <- addDocuments store_ docs
    retrievedDocs <- similaritySearch store_ "core Haskell features" 3
    o <- liftIO $ getOpenRouterModel defaultModelName
    let chain = newMapReduceChain o
    resp <- runMapReduceChain chain retrievedDocs Map.empty
    pure (extractMessageText resp)

  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans

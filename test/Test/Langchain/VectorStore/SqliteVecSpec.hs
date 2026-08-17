{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.VectorStore.SqliteVecSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.VectorStore.Core (VectorStore (..))
import Langchain.VectorStore.SqliteVec

data DeterministicMockEmbeddings = DeterministicMockEmbeddings

instance Embeddings DeterministicMockEmbeddings where
  embedDocuments _ docs = pure $ map (mockEmbed . TL.toStrict . pageContent) docs
  embedQuery _ q = pure $ mockEmbed q

mockEmbed :: Text -> [Float]
mockEmbed t =
  let len = fromIntegral (T.length t) :: Float
      isHaskell = if "Haskell" `T.isInfixOf` t then 1.0 else 0.0
   in [isHaskell, len / 100.0, 0.5]

tests :: TestTree
tests =
  testGroup
    "Langchain.VectorStore.SqliteVecSpec"
    [ testCase "SqliteVecStore adds documents and performs similarity search" $ do
        withSystemTempDirectory "sqlite-vec-test" $ \tmpDir -> do
          let dbPath = tmpDir </> "vectors.db"
              emb = DeterministicMockEmbeddings
          res <- runExceptT $ do
            store <- newSqliteVecStore dbPath emb
            let doc1 = Document "Haskell is purely functional" Map.empty
                doc2 = Document "Python is dynamically typed" Map.empty
            _ <- addDocuments store [doc1, doc2]
            similaritySearch store "Haskell programming" 1
          case res of
            Left err -> assertFailure ("SqliteVecStore failed: " ++ show err)
            Right matchedDocs -> do
              length matchedDocs @?= 1
              pageContent (head matchedDocs) @?= "Haskell is purely functional"
    ]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.VectorStore.Core (tests) where

import Control.Monad.Except (runExceptT)
import Data.Either (fromRight, isRight)
import Data.Int (Int64)
import Data.Map (empty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core
import Langchain.VectorStore.Core
import Langchain.VectorStore.InMemory

data MockEmbeddings = MockEmbeddings
  deriving (Show, Eq)

instance Embeddings MockEmbeddings where
  embedQuery _ "World" = pure [1.0, 0.1, 0.1]
  embedQuery _ "Meet you" = pure [0.1, 0.1, 1.0]
  embedQuery _ "Both" = pure [0.5, 0.5, 0.5]
  embedQuery _ _ = pure [0.0, 0.0, 0.0]

  embedDocuments _ docs = pure $ map determineEmbedding docs
    where
      determineEmbedding doc
        | doc == Document "Hello World" empty = [1.0, 0.1, 0.1]
        | doc == Document "Nice to meet you" empty = [0.1, 0.1, 1.0]
        | doc == Document "Something completely different" empty = [0.3, 0.3, 0.3]
        | otherwise = [0.0, 0.0, 0.0]

createTestDocs :: [Document]
createTestDocs =
  [ Document "Hello World" empty
  , Document "Nice to meet you" empty
  ]

utilityTests :: TestTree
utilityTests =
  testGroup
    "Utility Functions Tests"
    [ testCase "dotProduct should compute correct dot product" $ do
        dotProduct [1.0, 2.0, 3.0] [4.0, 5.0, 6.0] @?= 32.0
    , testCase "norm should compute correct Euclidean norm" $ do
        norm [3.0, 4.0] @?= 5.0
    , testCase "cosineSimilarity should compute correct similarity" $ do
        assertBool
          "Expected near same similarity"
          (cosineSimilarity [1.0, 2.0, 3.0] [1.0, 2.0, 3.0] >= 0.999999)

        let similarity = cosineSimilarity [1.0, 0.0, 0.0] [0.0, 1.0, 0.0]
        assertBool "Expected near 0" (abs similarity < 0.000001)

        let oppSimilarity = cosineSimilarity [1.0, 2.0, 3.0] [-1.0, -2.0, -3.0]
        assertBool "Expected near -1" (abs (oppSimilarity + 1.0) < 0.000001)
    ]

inMemoryTests :: TestTree
inMemoryTests =
  testGroup
    "InMemory VectorStore Tests"
    [ testCase "emptyInMemoryVectorStore should create empty store" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
        Map.size (store vs) @?= 0
        embeddingModel vs @?= model
    , testCase "fromDocuments should create store with documents" $ do
        let model = MockEmbeddings
            docs = createTestDocs
        result <- runExceptT $ fromDocuments model docs
        assertBool "Expected Right result" (isRight result)
        let vs = fromRight (emptyInMemoryVectorStore model) result
        Map.size (store vs) @?= 2
    , testCase "addDocuments should add documents to store" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
            docs = createTestDocs
        result <- runExceptT $ addDocuments vs docs
        assertBool "Expected Right result" (isRight result)
        let updatedVs = fromRight vs result
        Map.size (store updatedVs) @?= 2

        let newDoc = Document "Something completely different" empty
        result2 <- runExceptT $ addDocuments updatedVs [newDoc]
        assertBool "Expected Right result" (isRight result2)
        let finalVs = fromRight updatedVs result2
        Map.size (store finalVs) @?= 3
    , testCase "delete should remove documents from store" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
            docs = createTestDocs
        result <- runExceptT $ addDocuments vs docs
        let updatedVs = fromRight vs result

        deleteResult <- runExceptT $ delete updatedVs [1]
        assertBool "Expected Right result" (isRight deleteResult)
        let afterDeleteVs = fromRight updatedVs deleteResult
        Map.size (store afterDeleteVs) @?= 1
        Map.member (1 :: Int64) (store afterDeleteVs) @?= False
        Map.member (2 :: Int64) (store afterDeleteVs) @?= True
    , testCase "similaritySearch should find similar documents" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
            docs = createTestDocs
        result <- runExceptT $ do
          uVs <- addDocuments vs docs
          similaritySearch uVs "World" 1
        assertBool "Expected Right result" (isRight result)
        let docs1 = fromRight [] result
        length docs1 @?= 1
        fromMaybe (Document "" empty) (listToMaybe docs1) @?= Document "Hello World" empty
    , testCase "similaritySearchByVector should find similar documents" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
            docs = createTestDocs
        result <- runExceptT $ do
          uVs <- addDocuments vs docs
          similaritySearchByVector uVs [1.0, 0.1, 0.1] 1
        assertBool "Expected Right result" (isRight result)
        let docs1 = fromRight [] result
        length docs1 @?= 1
        fromMaybe (Document "" empty) (listToMaybe docs1) @?= Document "Hello World" empty
    ]

tests :: TestTree
tests =
  testGroup
    "Langchain.VectorStore Tests"
    [ utilityTests
    , inMemoryTests
    ]

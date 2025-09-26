{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.VectorStore.Core (tests) where

import Data.Either (fromRight, isRight)
import Data.Int (Int64)
import Data.Map (empty)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe (fromMaybe, listToMaybe)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core
import Langchain.VectorStore.Core
import Langchain.VectorStore.InMemory

data MockEmbeddings = MockEmbeddings
  deriving (Show, Eq)

instance Embeddings MockEmbeddings where
  embedQuery _ "World" = pure $ Right [1.0, 0.1, 0.1]
  embedQuery _ "Meet you" = pure $ Right [0.1, 0.1, 1.0]
  embedQuery _ "Both" = pure $ Right [0.5, 0.5, 0.5]
  embedQuery _ _ = pure $ Right [0.0, 0.0, 0.0]

  embedDocuments _ docs = pure $ Right $ map determineEmbedding docs
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
        result <- fromDocuments model docs
        assertBool "Expected Right result" (isRight result)
        let vs = fromRight (emptyInMemoryVectorStore model) result
        Map.size (store vs) @?= 2
    , testCase "addDocuments should add documents to store" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
            docs = createTestDocs
        result <- addDocuments vs docs
        assertBool "Expected Right result" (isRight result)
        let updatedVs = fromRight vs result
        Map.size (store updatedVs) @?= 2

        let newDoc = Document "Something completely different" empty
        result2 <- addDocuments updatedVs [newDoc]
        assertBool "Expected Right result" (isRight result2)
        let finalVs = fromRight updatedVs result2
        Map.size (store finalVs) @?= 3
    , testCase "delete should remove documents from store" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
            docs = createTestDocs
        result <- addDocuments vs docs
        let updatedVs = fromRight vs result

        deleteResult <- delete updatedVs [1]
        assertBool "Expected Right result" (isRight deleteResult)
        let afterDeleteVs = fromRight updatedVs deleteResult
        Map.size (store afterDeleteVs) @?= 1
        Map.member (1 :: Int64) (store afterDeleteVs) @?= False
        Map.member (2 :: Int64) (store afterDeleteVs) @?= True
    , testCase "similaritySearch should find similar documents" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
            docs = createTestDocs
        result <- addDocuments vs docs
        let updatedVs = fromRight vs result

        -- Search for "World" - should return "Hello World"
        searchResult1 <- similaritySearch updatedVs "World" 1
        assertBool "Expected Right result" (isRight searchResult1)
        let docs1 = fromRight [] searchResult1
        length docs1 @?= 1
        fromMaybe (Document "" empty) (listToMaybe docs1) @?= Document "Hello World" empty

        -- Search for "Meet you" - should return "Nice to meet you"
        searchResult2 <- similaritySearch updatedVs "Meet you" 1
        assertBool "Expected Right result" (isRight searchResult2)
        let docs2 = fromRight [] searchResult2
        length docs2 @?= 1
        fromMaybe (Document "" empty) (listToMaybe docs2) @?= Document "Nice to meet you" empty

        -- Search for both documents
        searchResult3 <- similaritySearch updatedVs "Both" 2
        assertBool "Expected Right result" (isRight searchResult3)
        let docs3 = fromRight [] searchResult3
        length docs3 @?= 2
    , testCase "similaritySearchByVector should find similar documents" $ do
        let model = MockEmbeddings
            vs = emptyInMemoryVectorStore model
            docs = createTestDocs
        result <- addDocuments vs docs
        let updatedVs = fromRight vs result

        -- Search with vector similar to "Hello World"
        searchResult1 <- similaritySearchByVector updatedVs [1.0, 0.1, 0.1] 1
        assertBool "Expected Right result" (isRight searchResult1)
        let docs1 = fromRight [] searchResult1
        length docs1 @?= 1
        fromMaybe (Document "" empty) (listToMaybe docs1) @?= Document "Hello World" empty

        -- Search with vector similar to "Nice to meet you"
        searchResult2 <- similaritySearchByVector updatedVs [0.1, 0.1, 1.0] 1
        assertBool "Expected Right result" (isRight searchResult2)
        let docs2 = fromRight [] searchResult2
        length docs2 @?= 1
        fromMaybe (Document "" empty) (listToMaybe docs2) @?= Document "Nice to meet you" empty
    ]

tests :: TestTree
tests =
  testGroup
    "Langchain.VectorStore Tests"
    [ utilityTests
    , inMemoryTests
    ]

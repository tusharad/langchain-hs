{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Retriever.AdvancedRetrieversSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (newMockModel)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.Retriever.ContextualCompression
import Langchain.Retriever.Core
import Langchain.Retriever.ParentDocument
import Langchain.VectorStore.InMemory

data MockEmbeddings = MockEmbeddings

instance Embeddings MockEmbeddings where
  embedDocuments _ docs = pure $ map (\d -> [fromIntegral (TL.length (pageContent d)) :: Float]) docs
  embedQuery _ q = pure [fromIntegral (T.length q) :: Float]

tests :: TestTree
tests =
  testGroup
    "Langchain.Retriever.AdvancedRetrieversSpec"
    [ testCase "ContextualCompressionRetriever compresses document context" $ do
        let mockModel = newMockModel "Only this relevant sentence."
            baseDocs = [Document "Noise header. Only this relevant sentence. Noise footer." Map.empty]
            mockRetriever = MockBaseRetriever baseDocs
            compressor = newContextualCompressionRetriever mockRetriever mockModel
        res <- runExceptT $ getRelevantDocuments compressor "Find relevant sentence"
        case res of
          Left err -> assertFailure ("Compression failed: " ++ show err)
          Right [topDoc] ->
            pageContent topDoc @?= "Only this relevant sentence."
          Right docs ->
            assertFailure ("Expected 1 doc, got " ++ show (length docs))
    , testCase "ParentDocumentRetriever returns full parent when child matches" $ do
        let emb = MockEmbeddings
            initialVs = emptyInMemoryVectorStore emb
        res <- runExceptT $ do
          pRetriever <- newParentDocumentRetriever initialVs
          let parent1 = Document "Large Parent Document 1 with extensive detailed chapters" Map.empty
              parent2 = Document "Large Parent Document 2 with other contents" Map.empty
          updatedPR <- addParentDocuments pRetriever [parent1, parent2]
          getRelevantDocuments updatedPR "Parent Document 1"
        case res of
          Left err -> assertFailure ("ParentDocumentRetriever failed: " ++ show err)
          Right (topDoc : _) ->
            assertBool
              "Contains parent 1 content"
              ("Large Parent Document 1" `TL.isInfixOf` pageContent topDoc)
          Right [] -> assertFailure "Expected at least one parent document"
    ]

newtype MockBaseRetriever = MockBaseRetriever [Document]

instance Retriever MockBaseRetriever where
  getRelevantDocuments (MockBaseRetriever docs) _ = pure docs

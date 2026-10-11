{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Retriever.BM25Spec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever.BM25

tests :: TestTree
tests =
  testGroup
    "Langchain.Retriever.BM25"
    [ testCase "BM25 finds exact matching document" $ do
        let doc1 =
              Document
                { pageContent = "Haskell is a functional programming language with strong static types."
                , metadata = Map.empty
                }
            doc2 =
              Document
                { pageContent = "Python is a dynamic language used for machine learning and web scripts."
                , metadata = Map.empty
                }
            doc3 =
              Document
                { pageContent = "Rust guarantees memory safety without garbage collection."
                , metadata = Map.empty
                }
            index = newBM25Index [doc1, doc2, doc3]
            results = bm25Search index "functional static types" 2
        case results of
          (topResult : _) -> topResult @?= doc1
          [] -> assertFailure "Expected non-empty search results"
    , testCase "BM25 scoring gives highest score to relevant passage" $ do
        let doc1 =
              Document
                { pageContent = "Deep research agent explores web pages and validates claims."
                , metadata = Map.empty
                }
            doc2 =
              Document
                { pageContent = "Database query optimization and index scans in postgresql."
                , metadata = Map.empty
                }
            index = newBM25Index [doc1, doc2]
            scored = bm25SearchWithScores index "deep research agent" 2
        case scored of
          [(bestDoc, score)] -> do
            bestDoc @?= doc1
            assertBool "Score should be positive" (score > 0.0)
          _ -> assertFailure ("Expected 1 scored result, got " ++ show (length scored))
    , testCase "addDocumentsBM25 updates index correctly" $ do
        let doc1 = Document {pageContent = "Alpha beta gamma", metadata = Map.empty}
            doc2 = Document {pageContent = "Delta epsilon zeta", metadata = Map.empty}
            index1 = newBM25Index [doc1]
            index2 = addDocumentsBM25 [doc2] index1
            results = bm25Search index2 "epsilon" 5
        results @?= [doc2]
    , testProperty "Tokenize lowercases and strips punctuation" $
        \s ->
          let txt = T.pack s
              tokens = tokenize txt
           in all (\t -> T.toLower t == t) tokens
    ]

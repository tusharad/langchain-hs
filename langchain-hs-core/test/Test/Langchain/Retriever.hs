{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Retriever (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever

data DummyRetriever = DummyRetriever
  deriving (Show, Eq)

instance Retriever DummyRetriever where
  getRelevantDocuments _ query =
    pure [Document (TL.fromStrict $ query <> " result") Map.empty]

tests :: TestTree
tests =
  testGroup
    "Langchain.Retriever"
    [ testGroup
        "Core"
        [ testCase "DummyRetriever retrieves documents" $ do
            res <- runExceptT $ getRelevantDocuments DummyRetriever "test"
            case res of
              Left err -> assertFailure ("Error: " ++ show err)
              Right docs -> map pageContent docs @?= ["test result"]
        ]
    , testGroup
        "BM25"
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
        ]
    , testGroup
        "Hybrid"
        [ testCase "reciprocalRankFusion prioritizes documents appearing in both lists" $ do
            let docA = Document {pageContent = "Document A about quantum algorithms", metadata = Map.empty}
                docB = Document {pageContent = "Document B about classical physics", metadata = Map.empty}
                docC = Document {pageContent = "Document C about neural networks", metadata = Map.empty}
                denseList = [docA, docB]
                sparseList = [docC, docA]
                fused = reciprocalRankFusion 60.0 [(denseList, 1.0), (sparseList, 1.0)]
            length fused @?= 3
            case fused of
              ((topDoc, _) : _) -> topDoc @?= docA
              [] -> assertFailure "Expected non-empty fused results"
        , testCase "searchHybrid executes dense and sparse searches" $ do
            let doc1 = Document {pageContent = "Haskell state monad and effects", metadata = Map.empty}
                doc2 = Document {pageContent = "Rust borrow checker and lifetimes", metadata = Map.empty}
                bm25 = newBM25Index [doc1, doc2]
                mockVecSearch _ _ = pure [doc2, doc1]
                hybrid = newHybridRetriever bm25 mockVecSearch
            results <- searchHybrid hybrid "Haskell" 2
            length results @?= 2
            case results of
              (topDoc : _) -> topDoc @?= doc1
              [] -> assertFailure "Expected non-empty results"
        ]
    ]

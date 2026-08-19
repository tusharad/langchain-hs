{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Retriever.HybridSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever.BM25 (newBM25Index)
import Langchain.Retriever.Hybrid

tests :: TestTree
tests =
  testGroup
    "Langchain.Retriever.Hybrid"
    [ testCase "reciprocalRankFusion prioritizes documents appearing in both lists" $ do
        let docA = Document {pageContent = "Document A about quantum algorithms", metadata = Map.empty}
            docB = Document {pageContent = "Document B about classical physics", metadata = Map.empty}
            docC = Document {pageContent = "Document C about neural networks", metadata = Map.empty}
            denseList = [docA, docB]
            sparseList = [docC, docA]
            fused = reciprocalRankFusion 60.0 [(denseList, 1.0), (sparseList, 1.0)]
        -- docA appears in both dense (rank 1) and sparse (rank 2) -> highest combined score
        length fused @?= 3
        fst (head fused) @?= docA
    , testCase "searchHybrid executes dense and sparse searches" $ do
        let doc1 = Document {pageContent = "Haskell state monad and effects", metadata = Map.empty}
            doc2 = Document {pageContent = "Rust borrow checker and lifetimes", metadata = Map.empty}
            bm25 = newBM25Index [doc1, doc2]
            mockVecSearch _ _ = pure [doc2, doc1]
            hybrid = newHybridRetriever bm25 mockVecSearch
        results <- searchHybrid hybrid "Haskell" 2
        length results @?= 2
        head results @?= doc1
    ]

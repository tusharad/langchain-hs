{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.VectorStore.FilterSpec (tests) where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.VectorStore.Filter

tests :: TestTree
tests =
  testGroup
    "Langchain.VectorStore.Filter"
    [ testCase "Eq filter matches exact metadata value" $ do
        let doc1 = Document {pageContent = "Doc 1", metadata = Map.fromList [("brain_id", String "finance")]}
            doc2 = Document {pageContent = "Doc 2", metadata = Map.fromList [("brain_id", String "engineering")]}
            predEq = eqFilter "brain_id" (String "finance")
            filtered = filterDocuments predEq [doc1, doc2]
        length filtered @?= 1
        head filtered @?= doc1
    , testCase "In filter matches any allowed value" $ do
        let doc1 = Document {pageContent = "Doc 1", metadata = Map.fromList [("tag", String "haskell")]}
            doc2 = Document {pageContent = "Doc 2", metadata = Map.fromList [("tag", String "rust")]}
            doc3 = Document {pageContent = "Doc 3", metadata = Map.fromList [("tag", String "c")]}
            predIn = inFilter "tag" [String "haskell", String "rust"]
            filtered = filterDocuments predIn [doc1, doc2, doc3]
        length filtered @?= 2
    , testCase "And & Or composite filters evaluate correctly" $ do
        let doc1 =
              Document
                { pageContent = "Doc 1"
                , metadata = Map.fromList [("env", String "prod"), ("tier", Number 1)]
                }
            doc2 =
              Document
                { pageContent = "Doc 2"
                , metadata = Map.fromList [("env", String "staging"), ("tier", Number 1)]
                }
            doc3 =
              Document
                { pageContent = "Doc 3"
                , metadata = Map.fromList [("env", String "prod"), ("tier", Number 2)]
                }
            predComp = andFilter [eqFilter "env" (String "prod"), Gt "tier" 1.5]
            filtered = filterDocuments predComp [doc1, doc2, doc3]
        length filtered @?= 1
        head filtered @?= doc3
    ]

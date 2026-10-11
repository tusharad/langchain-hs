{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Retriever.Core (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as HM
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever.Core (Retriever (..))

data DummyRetriever = DummyRetriever
  deriving (Show, Eq)

instance Retriever DummyRetriever where
  getRelevantDocuments _ query =
    pure [Document (TL.fromStrict $ query <> " result") HM.empty]

tests :: TestTree
tests =
  testGroup
    "Retriever Tests"
    [ testCase "DummyRetriever retrieves documents" $ do
        res <- runExceptT $ getRelevantDocuments DummyRetriever "test"
        case res of
          Left err -> assertFailure ("Error: " ++ show err)
          Right docs -> map pageContent docs @?= ["test result"]
    ]

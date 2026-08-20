{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Retriever.Core (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as HM
import qualified Data.Text.Lazy as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (ChatModel (..), assistantMessage)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever.Core (Retriever (..))
import Langchain.Retriever.MultiQueryRetriever

data DummyLLM = DummyLLM
  deriving (Show, Eq)

instance ChatModel DummyLLM where
  type ModelConfig DummyLLM = ()
  invoke _ _ _ = pure $ assistantMessage "1. test query 1\n2. test query 2"
  stream _ _ _ = pure ()

data DummyRetriever = DummyRetriever
  deriving (Show, Eq)

instance Retriever DummyRetriever where
  getRelevantDocuments _ query =
    pure [Document (T.fromStrict $ query <> " result") HM.empty]

test_generateQueries :: Assertion
test_generateQueries = do
  let dummyLLM = DummyLLM
      query = "original query"
      numQueriesToGenerate = 2
      includeOriginal = True
      queryPrompt = defaultQueryGenerationPrompt
  result <-
    runExceptT $ generateQueries dummyLLM queryPrompt query numQueriesToGenerate includeOriginal
  case result of
    Left err -> assertFailure ("generateQueries failed with error: " ++ show err)
    Right qs -> do
      let expectedQueries =
            [ "original query"
            , "test query 1"
            , "test query 2"
            ]
      length qs @?= 3
      qs @?= expectedQueries

test_MultiQueryRetriever :: Assertion
test_MultiQueryRetriever = do
  let dummyLLM = DummyLLM
      dummyRetriever = DummyRetriever
      mqRetriever = newMultiQueryRetriever dummyRetriever dummyLLM
      originalQuery = "original query"
  result <- runExceptT $ getRelevantDocuments mqRetriever originalQuery
  case result of
    Left err -> assertFailure ("MultiQueryRetriever failed with error: " ++ show err)
    Right docs -> do
      length docs @?= 3
      let contents = map pageContent docs
          expectedContents =
            [ "original query result"
            , "test query 1 result"
            , "test query 2 result"
            ]
      contents @?= expectedContents

tests :: TestTree
tests =
  testGroup
    "Retriever Tests"
    [ testCase "generateQueries returns expected queries" test_generateQueries
    , testCase "MultiQueryRetriever retrieves and combines documents" test_MultiQueryRetriever
    ]

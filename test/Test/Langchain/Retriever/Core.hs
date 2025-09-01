{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Retriever.Core (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.LLM.Core (LLM (..))
import qualified Langchain.LLM.Core as LLM
import Langchain.Retriever.Core (Retriever (..))
import Langchain.Retriever.MultiQueryRetriever

import qualified Data.Map.Strict as HM

data DummyLLM = DummyLLM

-- TODO: Add some real world examples here
instance LLM DummyLLM where
  type LLMParams DummyLLM = String

  -- When 'generate' is called, we return a fixed response in the format expected by the
  -- NumberSeparatedList parser. For example:
  --
  -- "1. test query 1\n2. test query 2"
  generate _ _ _ = return $ Right "1. test query 1\n2. test query 2"
  chat _ _ _ = return $ Right $ LLM.Message LLM.User "dummy chat response" LLM.defaultMessageData
  stream _ _ _ _ = return $ Right ()

data DummyRetriever = DummyRetriever

instance Retriever DummyRetriever where
  _get_relevant_documents _ query =
    return $ Right [Document (query <> " result") HM.empty]

test_generateQueries :: Assertion
test_generateQueries = do
  let dummyLLM = DummyLLM
      query = "original query"
      numQueriesToGenerate = 2
      includeOriginal = True
      queryPrompt = defaultQueryGenerationPrompt
  result <- generateQueries dummyLLM queryPrompt query numQueriesToGenerate includeOriginal
  case result of
    Left err -> assertFailure ("generateQueries failed with error: " ++ err)
    Right qs -> do
      let expectedQueries =
            [ "original query"
            , "test query 1"
            , "test query 2"
            ]
      length qs @?= 3
      qs @?= expectedQueries

-- Test the MultiQueryRetriever _get_relevant_documents implementation.
test_MultiQueryRetriever :: Assertion
test_MultiQueryRetriever = do
  let dummyLLM = DummyLLM
      dummyRetriever = DummyRetriever
      -- Create a MultiQueryRetriever using the dummy implementations.
      mqRetriever = newMultiQueryRetriever dummyRetriever dummyLLM
      originalQuery = "original query"
  result <- _get_relevant_documents mqRetriever originalQuery
  case result of
    Left err -> assertFailure ("MultiQueryRetriever failed with error: " ++ err)
    Right docs -> do
      -- Since generateQueries returns three queries (original plus two generated),
      -- and DummyRetriever returns one document per query, we expect 3 documents.
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

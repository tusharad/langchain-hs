{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Embeddings.Core (tests) where

import Data.Text (isInfixOf, pack)
import Langchain.Embeddings.Core
import Langchain.Embeddings.Ollama
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Embedding Tests"
    [ testGroup
        "embedQuery Tests"
        [ testCase "Propagates API errors" $ do
            let embeddings = OllamaEmbeddings "error-model" Nothing Nothing Nothing
            -- Assuming embeddingOps returns Left "API Failure"
            result <- embedQuery embeddings "error query"
            case result of
              Left err ->
                assertBool "Error message contains 'error'" ("error" `isInfixOf` (pack err))
              Right _ -> assertFailure "Expected API error propagation"
        ]
    ]

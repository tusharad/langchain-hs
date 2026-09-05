{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Embeddings.Core (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Embeddings.Core
import Langchain.Embeddings.Ollama

tests :: TestTree
tests =
  testGroup
    "Embedding Tests"
    [ testGroup
        "embedQuery Tests"
        [ testCase "Propagates API errors" $ do
            let embeddings = OllamaEmbeddings "error-model" Nothing Nothing Nothing
            result <- runExceptT $ embedQuery embeddings "error query"
            case result of
              Left err ->
                assertBool
                  "Error message contains error or failure"
                  ( T.isInfixOf "error" (T.pack (show err))
                      || T.isInfixOf "Error" (T.pack (show err))
                      || T.isInfixOf "Connect" (T.pack (show err))
                  )
              Right _ -> assertFailure "Expected API error propagation"
        ]
    ]

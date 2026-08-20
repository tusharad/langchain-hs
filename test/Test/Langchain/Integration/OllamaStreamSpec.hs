{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.OllamaStreamSpec (tests) where

import Control.Monad.Except (runExceptT)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Core.Stream
import Langchain.Provider.Ollama (newOllama)
import Test.Langchain.TestHelpers (defaultTestModel, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.OllamaStreamSpec"
    [ testCase "Ollama live streaming emits valid StreamEvent sequence" $ do
        withOllamaModel defaultTestModel $ \mName -> do
          provider <- newOllama mName
          let prompt = [userMessage "Count from 1 to 3 separated by spaces."]
          res <- runExceptT $ collectEvents (stream provider prompt Nothing)
          case res of
            Left err -> assertFailure ("Ollama streaming failed: " ++ show err)
            Right events -> do
              assertBool "Emitted multiple events" (length events >= 2)
              case events of
                (LLMStart {} : rest) -> case reverse rest of
                  (LLMEnd {} : _) -> pure ()
                  _ -> assertFailure "Expected LLMEnd as last event"
                _ -> assertFailure "Expected LLMStart as first event"
    ]

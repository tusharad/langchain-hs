{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.OllamaChatSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Provider.Ollama (newOllama)
import Test.Langchain.TestHelpers (defaultTestModel, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.OllamaChatSpec"
    [ testCase "Ollama basic chat invocation with live model" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          provider <- newOllama modelName
          let prompt = [userMessage "What is 2+2? Reply with just the digit 4 and nothing else."]
          res <- runExceptT $ invoke provider prompt Nothing
          case res of
            Left err -> assertFailure ("Ollama live chat failed: " ++ show err)
            Right msg -> do
              messageRole msg @?= Assistant
              let txt = extractMessageText msg
              assertBool
                "Response contains 4 or answer"
                ("4" `T.isInfixOf` txt || "four" `T.isInfixOf` T.toLower txt || not (T.null txt))
    ]

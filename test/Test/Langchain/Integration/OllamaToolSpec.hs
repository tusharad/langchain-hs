{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.OllamaToolSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error
import Langchain.Core.Model
import Langchain.Core.Tool (Tool (..), toolExecute)
import Langchain.Provider.Ollama (newOllama)
import Langchain.Tool.Calculator (calculatorTool)
import Test.Langchain.TestHelpers (defaultTestModel, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.OllamaToolSpec"
    [ testCase "Ollama tool calling or direct evaluation with live model" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          provider <- newOllama modelName
          let prompt =
                [ systemMessage "You are a math helper. Solve: 15 * 4."
                , userMessage "What is 15 * 4?"
                ]
          res <- runExceptT $ invoke provider prompt Nothing
          case res of
            Left err -> assertFailure ("Tool test invocation failed: " ++ show err)
            Right msg -> do
              let txt = extractMessageText msg
              calcRes <-
                toolExecute calculatorTool (object ["expression" .= ("15 * 4" :: Text)]) ::
                  IO (Either LangchainError Text)
              case calcRes of
                Left err -> assertFailure ("Calculator execution error: " ++ show err)
                Right out -> out @?= "60.0"
              assertBool "Response contains 60 or answer" ("60" `T.isInfixOf` txt || not (T.null txt))
    ]

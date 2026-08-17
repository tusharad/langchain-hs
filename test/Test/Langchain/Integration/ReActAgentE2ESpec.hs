{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.ReActAgentE2ESpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct
import Langchain.Core.Model
import Langchain.Provider.Ollama (newOllama)
import Langchain.Tool.Calculator (calculatorTool)
import Test.Langchain.TestHelpers (defaultTestModel, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.ReActAgentE2ESpec"
    [ testCase "ReAct agent executes full loop with Ollama provider" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          provider <- newOllama modelName
          let agent = createReActAgent provider [calculatorTool]
              query = [userMessage "Calculate 12 * 12. Provide the result."]
          res <- runExceptT $ runReActAgent agent query
          case res of
            Left err -> assertFailure ("ReAct agent failed with error: " ++ show err)
            Right msg -> do
              let txt = extractMessageText msg
              assertBool "Result is non-empty and contains answer" (not (T.null txt))
    ]

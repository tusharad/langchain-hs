{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Agent.ReAct (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct
import Langchain.Core.Model
import Test.Langchain.Provider.Mock (newMockModel)
import Langchain.Tool.Calculator (calculatorTool)

tests :: TestTree
tests =
  testGroup
    "Langchain.Agent.ReAct"
    [ testCase "reactStep returns AgentFinish when LLM responds with plain text" $ do
        let mockModel = newMockModel "The answer is 4."
            agent = createReActAgent mockModel [calculatorTool]
        res <- runExceptT $ reactStep (agentModel agent) (agentTools agent) [userMessage "What is 2+2?"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right step -> case step of
            AgentFinish msg -> T.strip (extractMessageText msg) @?= "The answer is 4."
            _ -> assertFailure "Expected AgentFinish"
    , testCase "runReActAgent completes full loop on finish" $ do
        let mockModel = newMockModel "Finished processing"
            agent = createReActAgent mockModel [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Hello"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> T.strip (extractMessageText finalMsg) @?= "Finished processing"
    ]

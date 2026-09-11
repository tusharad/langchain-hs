{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Agent.AdvancedAgentsSpec (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.PlanAndExecute
import Langchain.Core.Error (LangchainError)
import Test.Langchain.Provider.Mock (newMockModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Agent.AdvancedAgentsSpec"
    [ testCase "PlanAndExecuteAgent plans with JSON structured output and executes sequentially" $ do
        let planner =
              newMockModel
                "{\"planSteps\": [{\"stepNumber\": 1, \"stepDescription\": \"Research Haskell\"}, {\"stepNumber\": 2, \"stepDescription\": \"Write code\"}, {\"stepNumber\": 3, \"stepDescription\": \"Run tests\"}]}"
            executor = newMockModel "Executed step successfully."
            agent = newPlanAndExecuteAgent planner executor Nothing
        res <- runExceptT $ runPlanAndExecute agent "Build a Haskell library"
        case res of
          Left err -> assertFailure ("PlanAndExecute failed: " ++ show err)
          Right ans -> ans @?= "Executed step successfully."
    , testCase "PlanAndExecuteAgent executes agent step executor" $ do
        let planner = newMockModel "{\"planSteps\": [{\"stepNumber\": 1, \"stepDescription\": \"Calculate sum\"}]}"
            agentExecutor :: T.Text -> ExceptT LangchainError IO T.Text
            agentExecutor _ = pure "Result: 42"
            agent = newPlanAndExecuteAgent planner agentExecutor Nothing
        res <- runExceptT $ runPlanAndExecute agent "Compute answer"
        case res of
          Left err -> assertFailure ("PlanAndExecute failed: " ++ show err)
          Right ans -> ans @?= "Result: 42"
    ]

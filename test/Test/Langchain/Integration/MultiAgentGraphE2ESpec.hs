{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.MultiAgentGraphE2ESpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.Supervisor
import Langchain.Core.Model
import Test.Langchain.TestHelpers (defaultTestModel, newTestOllama, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.MultiAgentGraphE2ESpec"
    [ testCase "Multi-Agent Supervisor Team coordinates specialists with live Ollama" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          model <- newTestOllama modelName
          let researcher =
                SpecialistAgent
                  { specialistName = "Researcher"
                  , specialistDescription = "Provides factual data about computer science concepts"
                  , specialistCapabilities = ["research", "facts"]
                  , specialistAction = \task -> do
                      resp <- invoke model [userMessage ("Provide 1 short sentence fact for: " <> task)] Nothing
                      pure (extractMessageText resp)
                  }
              team = newSupervisorTeam model [researcher]

          res <- runExceptT $ runSupervisorTeam team "Define functional programming in one short sentence."
          case res of
            Left err -> assertFailure ("Supervisor team failed: " ++ show err)
            Right ans -> do
              assertBool "Final supervisor answer is non-empty" (not $ T.null ans)
    ]

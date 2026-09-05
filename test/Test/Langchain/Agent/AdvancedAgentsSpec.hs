{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Agent.AdvancedAgentsSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.Functions
import Langchain.Agent.PlanAndExecute
import Langchain.Agent.Supervisor
import Langchain.Core.Model
  ( ChatModel (..)
  , ContentBlock (..)
  , Message (..)
  , Role (..)
  , ToolCall (..)
  , assistantMessage
  )
import qualified Langchain.Core.Model as Model
import Langchain.Provider.Mock (newMockModel)
import Langchain.Tool.Core (createTool)

tests :: TestTree
tests =
  testGroup
    "Langchain.Agent.AdvancedAgentsSpec"
    [ testCase "PlanAndExecuteAgent plans and executes sequentially" $ do
        let planner = newMockModel "1. Research Haskell\n2. Write code\n3. Run tests"
            executor = newMockModel "Executed step successfully."
            agent = newPlanAndExecuteAgent planner executor Nothing
        res <- runExceptT $ runPlanAndExecute agent "Build a Haskell library"
        case res of
          Left err -> assertFailure ("PlanAndExecute failed: " ++ show err)
          Right ans -> ans @?= "Executed step successfully."
    , testCase "FunctionsAgent executes tool call returned by model" $ do
        let calculatorTool =
              createTool
                "calc"
                "Computes arithmetic"
                (Aeson.object [])
                (\_ -> pure $ Right "Result: 42")
            mockToolModel = MockToolCallingModel
            agent = newFunctionsAgent mockToolModel [calculatorTool] Nothing
        res <- runExceptT $ runFunctionsAgent agent "What is 40 + 2?"
        case res of
          Left err -> assertFailure ("FunctionsAgent failed: " ++ show err)
          Right ans -> ans @?= "The answer is 42."
    , testCase "SupervisorTeam coordinates specialist agents" $ do
        let supervisor = newMockModel "DELEGATE: Coder | Implement sort function"
            coder =
              SpecialistAgent
                "Coder"
                "Writes code"
                ["haskell", "algorithms"]
                (\t -> pure ("Code written for: " <> t))
            team = newSupervisorTeam supervisor [coder]
        res <- runExceptT $ runSupervisorTeam team "Write sort function"
        case res of
          Left err -> assertFailure ("SupervisorTeam failed: " ++ show err)
          Right ans -> assertBool "Produced response" (not $ T.null ans)
    ]

data MockToolCallingModel = MockToolCallingModel

instance ChatModel MockToolCallingModel where
  type ModelConfig MockToolCallingModel = ()
  invoke _ msgs _ =
    let hasToolOutput = any (\m -> messageRole m == Model.Tool) msgs
     in if hasToolOutput
          then pure $ assistantMessage "The answer is 42."
          else
            pure $
              Message
                { messageRole = Assistant
                , messageContents = pure (TextBlock "")
                , messageName = Nothing
                , messageToolCalls = Just [ToolCall "call_1" "function" "calc" (Aeson.object [])]
                , messageToolId = Nothing
                }
  stream _ _ _ = pure ()

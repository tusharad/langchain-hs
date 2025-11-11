{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Agent.Executor (tests) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Langchain.Agent.Core
import Langchain.Agent.Executor
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Agent.Executor"
    [ testCreateInitialState
    , testShouldContinue
    , testRecordStep
    ]

testCreateInitialState :: TestTree
testCreateInitialState =
  testCase "createInitialState creates proper state" $ do
    let state = createInitialState "test input"
    agentInput state @?= "test input"
    agentIterations state @?= 0
    null (agentScratchpad state) @? "Scratchpad should be empty"
    length (NE.toList $ agentChatHistory state) @?= 1

testShouldContinue :: TestTree
testShouldContinue =
  testGroup
    "shouldContinue"
    [ testCase "returns true when within limits" $ do
        let config = defaultAgentConfig {maxIterations = 10}
            state = (createInitialState "test") {agentIterations = 5}
        shouldContinue config state 0 @? "Should continue"
    , testCase "returns false when max iterations reached" $ do
        let config = defaultAgentConfig {maxIterations = 10}
            state = (createInitialState "test") {agentIterations = 10}
        not (shouldContinue config state 0) @? "Should not continue"
    , testCase "returns false when time limit exceeded" $ do
        let config = defaultAgentConfig {maxExecutionTime = Just 5}
            state = createInitialState "test"
        not (shouldContinue config state 10) @? "Should not continue"
    , testCase "returns true when no time limit" $ do
        let config = defaultAgentConfig {maxExecutionTime = Nothing}
            state = (createInitialState "test") {agentIterations = 5}
        shouldContinue config state 1000 @? "Should continue without time limit"
    ]

testRecordStep :: TestTree
testRecordStep =
  testCase "recordStep adds step to scratchpad" $ do
    now <- getCurrentTime
    let state = createInitialState "test"
        action =
          AgentAction
            { actionTool = "test"
            , actionToolInput = "input"
            , actionLog = "log"
            , actionMetadata = Map.empty
            }
        step = AgentStep action "observation" now
        newState = recordStep state step
    length (agentScratchpad newState) @?= 1
    agentIterations newState @?= 1
    let recordedStep = head (agentScratchpad newState)
    stepObservation recordedStep @?= "observation"

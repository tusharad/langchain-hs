{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Agent.Core (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Langchain.Agent.Core
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Agent.Core"
    [ testFormatScratchpad
    , testFormatToolDescriptors
    , testDefaultAgentConfig
    , testAgentAction
    , testAgentFinish
    ]

testFormatScratchpad :: TestTree
testFormatScratchpad =
  testGroup
    "formatScratchpad"
    [ testCase "empty scratchpad" $ do
        let result = formatScratchpad []
        result @?= ""
    , testCase "single step" $ do
        now <- getCurrentTime
        let action =
              AgentAction
                { actionTool = "calculator"
                , actionToolInput = "2+2"
                , actionLog = "I need to calculate"
                , actionMetadata = Map.empty
                }
            step = AgentStep action "4" now
            result = formatScratchpad [step]
        T.isInfixOf "Your Thought: I need to calculate" result @? "Should contain thought"
        T.isInfixOf "Previous Tool: calculator" result @? "Should contain action"
        T.isInfixOf "Previous Tool Input: 2+2" result @? "Should contain input"
        T.isInfixOf "Previous Tool Result: 4" result @? "Should contain result"
    , testCase "multiple steps" $ do
        now <- getCurrentTime
        let action1 =
              AgentAction
                { actionTool = "search"
                , actionToolInput = "weather"
                , actionLog = "Search for weather"
                , actionMetadata = Map.empty
                }
            action2 =
              AgentAction
                { actionTool = "calculator"
                , actionToolInput = "10*2"
                , actionLog = "Calculate result"
                , actionMetadata = Map.empty
                }
            step1 = AgentStep action1 "sunny" now
            step2 = AgentStep action2 "20" now
            result = formatScratchpad [step1, step2]
        T.isInfixOf "Previous Tool: search" result @? "Should contain first action"
        T.isInfixOf "Previous Tool: calculator" result @? "Should contain second action"
    ]

testFormatToolDescriptors :: TestTree
testFormatToolDescriptors =
  testGroup
    "formatToolDescriptors"
    [ testCase "empty tools list" $ do
        let result = formatToolDescriptors []
        result @?= ""
    , testCase "single tool" $ do
        let tool =
              ToolDescriptor
                { toolDescName = "calculator"
                , toolDescDescription = "Performs calculations"
                , toolDescInputSchema = Nothing
                }
            result = formatToolDescriptors [tool]
        result @?= "- calculator: Performs calculations"
    , testCase "multiple tools" $ do
        let tool1 =
              ToolDescriptor
                { toolDescName = "calculator"
                , toolDescDescription = "Does math"
                , toolDescInputSchema = Nothing
                }
            tool2 =
              ToolDescriptor
                { toolDescName = "search"
                , toolDescDescription = "Searches web"
                , toolDescInputSchema = Nothing
                }
            result = formatToolDescriptors [tool1, tool2]
        T.isInfixOf "calculator: Does math" result @? "Should contain calculator"
        T.isInfixOf "search: Searches web" result @? "Should contain search"
    ]

testDefaultAgentConfig :: TestTree
testDefaultAgentConfig =
  testCase "defaultAgentConfig has expected values" $ do
    let config = defaultAgentConfig
    maxIterations config @?= 15
    maxExecutionTime config @?= Nothing
    returnIntermediateSteps config @?= False
    handleParsingErrors config @?= True
    verboseLogging config @?= False

testAgentAction :: TestTree
testAgentAction =
  testCase "AgentAction construction" $ do
    let action =
          AgentAction
            { actionTool = "test_tool"
            , actionToolInput = "test input"
            , actionLog = "test log"
            , actionMetadata = Map.fromList [("key", "value")]
            }
    actionTool action @?= "test_tool"
    actionToolInput action @?= "test input"
    actionLog action @?= "test log"
    Map.lookup "key" (actionMetadata action) @?= Just "value"

testAgentFinish :: TestTree
testAgentFinish =
  testCase "AgentFinish construction" $ do
    let finish =
          AgentFinish
            { agentOutput = "final answer"
            , finishMetadata = Map.fromList [("iterations", "5")]
            , finishLog = "completion log"
            }
    agentOutput finish @?= "final answer"
    finishLog finish @?= "completion log"
    Map.lookup "iterations" (finishMetadata finish) @?= Just "5"

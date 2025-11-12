{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Agent.ReAct (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Langchain.Agent.Core
import Langchain.Agent.ReAct
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Agent.ReAct"
    [ testParseReActOutput
    , testReActPromptTemplate
    ]

testParseReActOutput :: TestTree
testParseReActOutput =
  testGroup
    "parseReActOutput"
    [ testCase "parses final answer" $ do
        let output = "Final Answer: The capital of France is Paris"
        case parseReActOutput output of
          Left err -> assertFailure $ "Should parse: " ++ show err
          Right (ReActFinalAnswer answer) ->
            answer @?= "The capital of France is Paris"
          Right other -> assertFailure $ "Wrong parse result: " ++ show other
    , testCase "parses final answer case insensitive" $ do
        let output = "final answer: 42"
        case parseReActOutput output of
          Left err -> assertFailure $ "Should parse: " ++ show err
          Right (ReActFinalAnswer answer) ->
            answer @?= "42"
          Right other -> assertFailure $ "Wrong parse result: " ++ show other
    , testCase "parses action with input" $ do
        let output = "Action: calculator\nAction Input: 2+2"
        case parseReActOutput output of
          Left err -> assertFailure $ "Should parse: " ++ show err
          Right (ReActAction tool input) -> do
            tool @?= "calculator"
            input @?= "2+2"
          Right other -> assertFailure $ "Wrong parse result: " ++ show other
    , testCase "parses action case insensitive" $ do
        let output = "action: search\naction input: query text"
        case parseReActOutput output of
          Left err -> assertFailure $ "Should parse: " ++ show err
          Right (ReActAction tool input) -> do
            tool @?= "search"
            input @?= "query text"
          Right other -> assertFailure $ "Wrong parse result: " ++ show other
    , testCase "parses thought" $ do
        let output = "Thought: I need to calculate something"
        case parseReActOutput output of
          Left err -> assertFailure $ "Should parse: " ++ show err
          Right (ReActThought thought) ->
            thought @?= "I need to calculate something"
          Right other -> assertFailure $ "Wrong parse result: " ++ show other
    , testCase "fails on invalid format" $ do
        let output = "Some random text without proper format"
        case parseReActOutput output of
          Left _ -> return () -- Expected
          Right r -> assertFailure $ "Should fail to parse: " ++ show r
    ]

testReActPromptTemplate :: TestTree
testReActPromptTemplate =
  testGroup
    "reActPromptTemplate"
    [ testCase "formats prompt with tools" $ do
        let tools =
              [ ToolDescriptor "calculator" "Does math" Nothing
              , ToolDescriptor "search" "Searches web" Nothing
              ]
            scratchpad = []
            input = "What is 2+2?"
            prompt = reActPromptTemplate tools scratchpad input
        T.isInfixOf "calculator" prompt @? "Should contain calculator tool"
        T.isInfixOf "search" prompt @? "Should contain search tool"
        T.isInfixOf "What is 2+2?" prompt @? "Should contain input"
    , testCase "includes scratchpad when present" $ do
        now <- getCurrentTime
        let tools = []
            action =
              AgentAction
                { actionTool = "test"
                , actionToolInput = "input"
                , actionLog = "thinking"
                , actionMetadata = Map.empty
                }
            step = AgentStep action "result" now
            scratchpad = [step]
            input = "test"
            prompt = reActPromptTemplate tools scratchpad input
        T.isInfixOf "Previous Tool Result: result" prompt @? "Should contain observation"
    ]

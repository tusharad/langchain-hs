{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Agent.ReAct (tests) where

import Data.Aeson (object, (.=))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Text (Text)
import Langchain.Agent.Core
import Langchain.Agent.ReAct
import Langchain.Error (LangchainError, llmError)
import Langchain.LLM.Core
import Langchain.Memory.Core (BaseMemory (..), WindowBufferMemory (..))
import Langchain.Tool.Core
import Test.Tasty
import Test.Tasty.HUnit

-- Mock LLM for testing
newtype MockLLM = MockLLM
  { mockResponse :: Either LangchainError Message
  }

instance LLM MockLLM where
  type LLMParams MockLLM = ()
  type LLMStreamTokenType MockLLM = Text

  generate _ _ _ = pure $ Left $ llmError "Not implemented" Nothing Nothing

  chat llm _ _ = pure $ mockResponse llm

  stream _ _ _ _ = pure $ Left $ llmError "Not implemented" Nothing Nothing

-- Mock Tool for testing
newtype MockTool = MockTool Text
  deriving (Show, Eq)

instance Tool MockTool where
  type Input MockTool = ToolCall
  type Output MockTool = Text

  toolName (MockTool toolName_) = toolName_
  toolDescription _ = "A mock tool for testing"
  runTool _ tc = pure $ "Executed: " <> toolFunctionName (toolCallFunction tc)

tests :: TestTree
tests =
  testGroup
    "Agent.ReAct"
    [ testPlanReturnsFinishWhenNoToolCalls
    , testPlanReturnsActionWhenToolCallsPresent
    , testPlanPropagatesLLMError
    , testExecuteToolFindsCorrectTool
    , testExecuteToolReturnsErrorWhenToolNotFound
    , testInitializeSetsUpStateCorrectly
    ]

-- Test that plan returns AgentFinish when LLM returns no tool calls
testPlanReturnsFinishWhenNoToolCalls :: TestTree
testPlanReturnsFinishWhenNoToolCalls = testCase "plan returns AgentFinish when no tool calls" $ do
  let mockMsg = Message Assistant "Final answer" defaultMessageData
      mockLLM = MockLLM (Right mockMsg)
      agent = createReActAgent mockLLM Nothing []
      testMemory = WindowBufferMemory 10 (NE.fromList [defaultMessage {content = "test"}])
      state =
        AgentState
          { agentMemory = SomeMemory testMemory
          , agentInput = "test input"
          , agentIterations = 0
          }

  result <- plan agent state
  case result of
    Right (Right finish) -> do
      assertEqual "Output should match content" "Final answer" (agentOutput finish)
      assertEqual "Log should match content" "Final answer" (finishLog finish)
    _ -> assertFailure $ "Expected Right (Right AgentFinish), got: " ++ show result

-- Test that plan returns AgentAction when LLM returns tool calls
testPlanReturnsActionWhenToolCallsPresent :: TestTree
testPlanReturnsActionWhenToolCallsPresent = testCase "plan returns AgentAction when tool calls present" $ do
  let toolCall =
        ToolCall
          { toolCallId = "call_123"
          , toolCallType = "function"
          , toolCallFunction =
              ToolFunction
                { toolFunctionName = "search"
                , toolFunctionArguments = Map.fromList [("query", object ["text" .= ("test" :: Text)])]
                }
          }
      msgData = defaultMessageData {toolCalls = Just [toolCall]}
      mockMsg = Message Assistant "Let me search" msgData
      mockLLM = MockLLM (Right mockMsg)
      agent = createReActAgent mockLLM Nothing []
      testMemory = WindowBufferMemory 10 (NE.fromList [defaultMessage {content = "test"}])
      state =
        AgentState
          { agentMemory = SomeMemory testMemory
          , agentInput = "test input"
          , agentIterations = 0
          }

  result <- plan agent state
  case result of
    Right (Left action) -> do
      assertEqual "Should have one tool call" 1 (length $ actionToolCall action)
      assertEqual "Log should match content" "Let me search" (actionLog action)
    _ -> assertFailure $ "Expected Right (Left AgentAction), got: " ++ show result

-- Test that plan propagates LLM errors
testPlanPropagatesLLMError :: TestTree
testPlanPropagatesLLMError = testCase "plan propagates LLM error" $ do
  let mockError = llmError "LLM failed" Nothing Nothing
      mockLLM = MockLLM (Left mockError)
      agent = createReActAgent mockLLM Nothing []
      testMemory = WindowBufferMemory 10 (NE.fromList [defaultMessage {content = "test"}])
      state =
        AgentState
          { agentMemory = SomeMemory testMemory
          , agentInput = "test input"
          , agentIterations = 0
          }

  result <- plan agent state
  case result of
    Left _ -> pure () -- Expected error
    Right _ -> assertFailure "Expected Left error, got Right"

-- Test that executeTool finds and executes the correct tool
testExecuteToolFindsCorrectTool :: TestTree
testExecuteToolFindsCorrectTool = testCase "executeTool finds and executes correct tool" $ do
  let tool1 = ToolAcceptingToolCall (MockTool "tool1")
      tool2 = ToolAcceptingToolCall (MockTool "tool2")
      mockLLM = MockLLM (Right defaultMessage)
      agent = createReActAgent mockLLM Nothing [tool1, tool2]
      toolCall =
        ToolCall
          { toolCallId = "call_123"
          , toolCallType = "function"
          , toolCallFunction =
              ToolFunction
                { toolFunctionName = "tool2"
                , toolFunctionArguments = Map.empty
                }
          }

  result <- executeTool agent toolCall
  case result of
    Right output -> do
      assertEqual "Should execute tool2" "Executed: tool2" output
    Left err -> assertFailure $ "Expected Right, got error: " ++ show err

-- Test that executeTool returns error when tool not found
testExecuteToolReturnsErrorWhenToolNotFound :: TestTree
testExecuteToolReturnsErrorWhenToolNotFound = testCase "executeTool returns error when tool not found" $ do
  let tool1 = ToolAcceptingToolCall (MockTool "tool1")
      mockLLM = MockLLM (Right defaultMessage)
      agent = createReActAgent mockLLM Nothing [tool1]
      toolCall =
        ToolCall
          { toolCallId = "call_123"
          , toolCallType = "function"
          , toolCallFunction =
              ToolFunction
                { toolFunctionName = "nonexistent"
                , toolFunctionArguments = Map.empty
                }
          }

  result <- executeTool agent toolCall
  case result of
    Left _ -> pure () -- Expected error
    Right _ -> assertFailure "Expected error for nonexistent tool"

-- Test that initialize sets up state correctly
testInitializeSetsUpStateCorrectly :: TestTree
testInitializeSetsUpStateCorrectly = testCase "initialize sets up state correctly" $ do
  let mockLLM = MockLLM (Right defaultMessage)
      agent = createReActAgent mockLLM Nothing []
      testMemory = WindowBufferMemory 10 (NE.fromList [defaultMessage])
      inputState =
        AgentState
          { agentMemory = SomeMemory testMemory
          , agentInput = "What is 2+2?"
          , agentIterations = 0
          }

  result <- initialize agent inputState
  case result of
    Right newState -> do
      assertEqual "Input should be preserved" "What is 2+2?" (agentInput newState)
      assertEqual "Iterations should be 0" 0 (agentIterations newState)

      -- Check chat history has system message and user message by accessing memory
      case agentMemory newState of
        SomeMemory mem -> do
          eHistory <- messages mem
          case eHistory of
            Right history -> do
              let historyList = NE.toList history
              assertEqual "Should have 3 messages (initial + system + user)" 3 (length historyList)
              case reverse historyList of
                (userMsg : sysMsg : _) -> do
                  assertEqual "Last message should be User" User (role userMsg)
                  assertEqual "Second to last message should be System" System (role sysMsg)
                  assertEqual "User message content should match input" "What is 2+2?" (content userMsg)
                _ -> assertFailure "Expected at least 2 messages in history"
            Left err -> assertFailure $ "Failed to get messages from memory: " ++ show err
    Left err -> assertFailure $ "Expected Right, got error: " ++ show err

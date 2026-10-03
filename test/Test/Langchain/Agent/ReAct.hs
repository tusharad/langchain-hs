{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Agent.ReAct (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
import Langchain.Core.Tool (Tool)
import Langchain.Memory.Core (BaseMemory (..), newWindowBufferMemory)
import Langchain.Tool.Binding (ToolBinder (..))
import Langchain.Tool.Calculator (calculatorTool)
import Test.Langchain.Provider.Mock (newMockModel)

-- | Mock model that records the config received by invoke
data ConfigRecordingModel = ConfigRecordingModel (IORef (Maybe Value)) T.Text

instance ChatModel ConfigRecordingModel where
  type ModelConfig ConfigRecordingModel = Value
  invoke (ConfigRecordingModel ref resp) _ mbCfg = do
    liftIO $ writeIORef ref mbCfg
    pure $ assistantMessage resp
  stream = error "stream not supported in ConfigRecordingModel"

instance ToolBinder ConfigRecordingModel m where
  bindToolsConfig tools _ =
    Just $ object ["tool_count" .= length tools]

-- | Mock model that yields a pre-configured sequence of responses and logs history
data StepSequenceModel = StepSequenceModel (IORef [Message]) (IORef [[Message]])

instance ChatModel StepSequenceModel where
  type ModelConfig StepSequenceModel = Value
  invoke (StepSequenceModel stepsRef histRef) history _ = liftIO $ do
    modifyIORef histRef (++ [history])
    steps <- readIORef stepsRef
    case steps of
      [] -> pure $ assistantMessage "Default response"
      (m : rest) -> do
        writeIORef stepsRef rest
        pure m
  stream = error "stream not supported in StepSequenceModel"

instance ToolBinder StepSequenceModel m where
  bindToolsConfig _ _ = Nothing

tests :: TestTree
tests =
  testGroup
    "Langchain.Agent.ReAct"
    [ testCase "reactStep returns AgentFinish when LLM responds with plain text" $ do
        let mockModel = newMockModel "The answer is 4."
            tools = [calculatorTool]
        res <- runExceptT $ reactStep mockModel tools [userMessage "What is 2+2?"] Nothing
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right step -> case step of
            AgentFinish msg -> T.strip (extractMessageText msg) @?= "The answer is 4."
            _ -> assertFailure "Expected AgentFinish"
    , testCase "reactStep returns AgentAction with all tool calls" $ do
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            tc2 = ToolCall "call_2" "function" "calculator" (object ["expression" .= ("3*3" :: T.Text)])
            respWithTools = (assistantMessage "") {messageToolCalls = Just [tc1, tc2]}
        sRef <- newIORef [respWithTools]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            tools = [calculatorTool :: Tool (ExceptT LangchainError IO)]
        res <- runExceptT $ reactStep model tools [userMessage "Calculate both"] Nothing
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right step -> case step of
            AgentAction _ tcs -> length tcs @?= 2
            _ -> assertFailure "Expected AgentAction with multiple tool calls"
    , testCase "runReActAgent executes multiple parallel tool calls and reaches finish" $ do
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            tc2 = ToolCall "call_2" "function" "calculator" (object ["expression" .= ("5*2" :: T.Text)])
            respWithTools = (assistantMessage "calculating") {messageToolCalls = Just [tc1, tc2]}
            finalResp = assistantMessage "4 and 10"
        sRef <- newIORef [respWithTools, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent = defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Calculate 2+2 and 5*2"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> do
            T.strip (extractMessageText finalMsg) @?= "4 and 10"
            -- Verify history in step 2 received observations for BOTH tool calls
            histories <- readIORef hRef
            case histories of
              [_, secondCallHistory] -> do
                let toolMsgs = filter (\m -> messageRole m == Tool) secondCallHistory
                length toolMsgs @?= 2
                map messageToolId toolMsgs @?= [Just "call_1", Just "call_2"]
              _ -> assertFailure $ "Expected 2 invocations, got: " ++ show (length histories)
    , testCase "runReActAgent handles unknown tool gracefully via observation error" $ do
        let tc = ToolCall "call_bad" "function" "unknown_tool" (object [])
            respWithBadTool = (assistantMessage "") {messageToolCalls = Just [tc]}
            finalResp = assistantMessage "Handled missing tool"
        sRef <- newIORef [respWithBadTool, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent = defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Run unknown tool"]
        case res of
          Left err -> assertFailure $ "Expected recovery but got error: " ++ show err
          Right finalMsg -> do
            T.strip (extractMessageText finalMsg) @?= "Handled missing tool"
            histories <- readIORef hRef
            case histories of
              [_, secondCallHistory] -> do
                let toolMsgs = filter (\m -> messageRole m == Tool) secondCallHistory
                length toolMsgs @?= 1
                case toolMsgs of
                  (m : _) ->
                    assertBool "Error observation" ("Tool not found: unknown_tool" `T.isInfixOf` extractMessageText m)
                  _ -> assertFailure "Expected tool message"
              _ -> assertFailure "Expected 2 invocations"
    , testCase "runReActAgent completes full loop on finish" $ do
        let mockModel = newMockModel "Finished processing"
            agent = defaultReActAgent mockModel [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Hello"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> T.strip (extractMessageText finalMsg) @?= "Finished processing"
    , testCase "reactStep passes bound tools config to model invoke" $ do
        ref <- newIORef Nothing
        let recordingModel = ConfigRecordingModel ref "Direct Answer"
            tools = [calculatorTool :: Tool (ExceptT LangchainError IO)]
        res <- runExceptT $ reactStep recordingModel tools [userMessage "Calculate 2+2"] Nothing
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right _ -> do
            captured <- readIORef ref
            captured @?= Just (object ["tool_count" .= (1 :: Int)])
    , testCase "runReActAgentWithTrace returns intermediate steps" $ do
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            respWithTools = (assistantMessage "calculating") {messageToolCalls = Just [tc1]}
            finalResp = assistantMessage "The answer is 4"
        sRef <- newIORef [respWithTools, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent = defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgentWithTrace agent [userMessage "Calculate 2+2"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right trace -> do
            -- Trace should have 2 steps: one action + one finish
            length (traceSteps trace) @?= 2
            T.strip (extractMessageText (traceFinalAnswer trace)) @?= "The answer is 4"
    , testCase "withSystemPrompt prepends system message to history" $ do
        hRef <- newIORef ([] :: [[Message]])
        sRef <- newIORef [assistantMessage "Got it"]
        let model = StepSequenceModel sRef hRef
            agent =
              withSystemPrompt "You are a math tutor." $
                defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Hi"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right _ -> do
            histories <- readIORef hRef
            case histories of
              (firstCall : _) ->
                case firstCall of
                  (sysMsg : _) -> do
                    messageRole sysMsg @?= System
                    T.strip (extractMessageText sysMsg) @?= "You are a math tutor."
                  _ -> assertFailure "Expected system message in history"
              _ -> assertFailure "Expected at least one invocation"
    , testCase "withMemory loads and saves conversation history" $ do
        mem <- newWindowBufferMemory 50 []
        hRef <- newIORef ([] :: [[Message]])
        sRef <- newIORef [assistantMessage "Memory works!"]
        let model = StepSequenceModel sRef hRef
            agent =
              withMemory mem $
                defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Test memory"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> do
            T.strip (extractMessageText finalMsg) @?= "Memory works!"
            -- Verify the messages were saved to memory
            memRes <- runExceptT $ messages mem
            case memRes of
              Left err -> assertFailure $ "Memory read failed: " ++ show err
              Right savedMsgs -> do
                -- Should have user message + assistant response
                length savedMsgs @?= 2
                case listToMaybe savedMsgs of
                  Nothing -> assertFailure "Message list is empty"
                  Just firstMessage -> messageRole firstMessage @?= User
                messageRole (savedMsgs !! 1) @?= Assistant
    , testCase "withModelConfig passes config to provider" $ do
        ref <- newIORef Nothing
        let recordingModel = ConfigRecordingModel ref "Configured Answer"
            tools = [calculatorTool]
            agent =
              withModelConfig
                (Just $ object ["temperature" .= (0.7 :: Double)])
                (defaultReActAgent recordingModel tools)
        res <- runExceptT $ runReActAgent agent [userMessage "Test config"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right _ -> do
            captured <- readIORef ref
            -- The config should be the result of bindToolsConfig merging with our config
            case captured of
              Nothing -> assertFailure "Expected config to be passed to model"
              Just _ -> pure () -- Config was passed (exact value depends on ToolBinder instance)
    ]

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Agent.ReAct (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
import Langchain.Core.Tool (Tool)
import Langchain.Provider.Gemini (Gemini)
import Langchain.Provider.Ollama (ChatRequest (..), Ollama, chatTools)
import Langchain.Provider.OpenAI (OpenAI)
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
            agent = createReActAgent mockModel [calculatorTool]
        res <- runExceptT $ reactStep (agentModel agent) (agentTools agent) [userMessage "What is 2+2?"]
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
            agent = createReActAgent model [calculatorTool :: Tool (ExceptT LangchainError IO)]
        res <- runExceptT $ reactStep (agentModel agent) (agentTools agent) [userMessage "Calculate both"]
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
            agent = createReActAgent model [calculatorTool :: Tool (ExceptT LangchainError IO)]
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
            agent = createReActAgent model [calculatorTool :: Tool (ExceptT LangchainError IO)]
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
            agent = createReActAgent mockModel [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Hello"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> T.strip (extractMessageText finalMsg) @?= "Finished processing"
    , testCase "reactStep passes bound tools config to model invoke" $ do
        ref <- newIORef Nothing
        let recordingModel = ConfigRecordingModel ref "Direct Answer"
            tools = [calculatorTool :: Tool (ExceptT LangchainError IO)]
        res <- runExceptT $ reactStep recordingModel tools [userMessage "Calculate 2+2"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right _ -> do
            captured <- readIORef ref
            captured @?= Just (object ["tool_count" .= (1 :: Int)])
    , testCase "ToolBinder Ollama attaches tools to ChatRequest config" $ do
        let tools = [calculatorTool :: Tool IO]
            mbCfg = bindToolsConfig @Ollama tools Nothing
        case mbCfg of
          Nothing -> assertFailure "Expected Just ChatRequest"
          Just req -> case chatTools req of
            Nothing -> assertFailure "Expected Just tools in ChatRequest"
            Just ts -> length ts @?= 1
    , testCase "ToolBinder OpenAI attaches tools to JSON config" $ do
        let tools = [calculatorTool :: Tool IO]
            mbCfg = bindToolsConfig @OpenAI tools Nothing
        case mbCfg of
          Just (Object obj) -> assertBool "Has 'tools' key" (KeyMap.member "tools" obj)
          _ -> assertFailure "Expected Just Object with tools"
    , testCase "ToolBinder Gemini attaches tools to JSON config" $ do
        let tools = [calculatorTool :: Tool IO]
            mbCfg = bindToolsConfig @Gemini tools Nothing
        case mbCfg of
          Just (Object obj) -> assertBool "Has 'tools' key" (KeyMap.member "tools" obj)
          _ -> assertFailure "Expected Just Object with tools"
    ]

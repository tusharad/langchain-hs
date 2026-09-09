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

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.RegressionSpec (tests) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Resource (runResourceT)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct
import Langchain.Core.Model
import Langchain.Core.Stream
import Langchain.Memory.Core (BaseMemory (..), newWindowBufferMemory)
import qualified Langchain.Memory.Core as TB
import Langchain.Tool.Calculator (calculatorTool)
import Test.Langchain.Provider.Mock (newMockModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.RegressionSpec"
    [ testCase "regression_ollama_stream_lifecycle: StreamEvent stream ends with LLMEnd" $ do
        let mockModel = newMockModel "Streaming chunk data"
            input = [userMessage "Ping"]
        res <- runResourceT $ runExceptT $ collectEvents (stream mockModel input Nothing)
        case res of
          Left err -> assertFailure ("Stream failed: " ++ show err)
          Right events -> do
            length events @?= 3
            case last events of
              LLMEnd _ finalMsg _ -> extractMessageText finalMsg @?= "Streaming chunk data"
              _ -> assertFailure "Expected LLMEnd as last event in stream"
    , testCase "regression_react_agent_plain_response: Completes immediately when no tool calls" $ do
        let mockModel = newMockModel "Direct Answer without tool calls"
            agent = createReActAgent mockModel [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "What is the capital of France?"]
        case res of
          Left err -> assertFailure ("ReAct agent failed: " ++ show err)
          Right finalMsg -> extractMessageText finalMsg @?= "Direct Answer without tool calls"
    , testCase "regression_memory_window_trimming: System message preserved during trimming" $ do
        let sys = systemMessage "System Prompt"
            u1 = userMessage "User 1"
            u2 = userMessage "User 2"
        mem <- newWindowBufferMemory 2 [sys, u1]
        res <- runExceptT $ do
          addMessage mem u2
          messages mem
        case res of
          Left err -> assertFailure ("Memory failed: " ++ show err)
          Right msgs -> msgs @?= [sys, u2]
    , testCase "regression_token_buffer_system_preservation: System message kept within token budget" $ do
        let sys = systemMessage "Sys"
            u1 = userMessage "Long user message 12345678"
            u2 = userMessage "Long user message 12345678"
        mem <- TB.newTokenBufferMemory 8 [sys, u1]
        res <- runExceptT $ do
          addMessage mem u2
          messages mem
        case res of
          Left err -> assertFailure ("TokenBuffer failed: " ++ show err)
          Right msgs -> do
            assertBool "Contains system message" (any (\m -> messageRole m == System) msgs)
    ]

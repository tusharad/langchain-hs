{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.RegressionSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (decode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct
import Langchain.Core.Error
import Langchain.Core.Model
import Langchain.Core.Stream
import Langchain.Core.Tool (Tool (..), toolExecute)
import Langchain.Memory.Core
import qualified Langchain.Memory.TokenBufferMemory as TB
import Langchain.Provider.OpenAI (parseOpenAIResponse)
import Langchain.Tool.Calculator (calculatorTool)
import Langchain.Tool.WebScraper (webScraperTool)

tests :: TestTree
tests =
  testGroup
    "Langchain.RegressionSpec"
    [ testCase "regression_ollama_stream_lifecycle: StreamEvent stream ends with LLMEnd" $ do
        let mockModel = newMockModel "Streaming chunk data"
            input = [userMessage "Ping"]
        res <- runExceptT $ collectEvents (stream mockModel input Nothing)
        case res of
          Left err -> assertFailure ("Stream failed: " ++ show err)
          Right events -> do
            length events @?= 3
            case last events of
              LLMEnd _ finalMsg _ -> extractMessageText finalMsg @?= "Streaming chunk data"
              _ -> assertFailure "Expected LLMEnd as last event in stream"
    , testCase "regression_system_fingerprint_nullable: OpenAI JSON parses without fingerprint" $ do
        let jsonWithoutFingerprint =
              "{\"id\":\"cmpl-1\",\"object\":\"chat.completion\",\"created\":1600000000,\"model\":\"gpt-4o\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"OK\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":1,\"completion_tokens\":1,\"total_tokens\":2}}"
        case decode (LBSC.pack jsonWithoutFingerprint) of
          Nothing -> assertFailure "Failed to decode JSON value"
          Just val -> case parseOpenAIResponse val of
            Left err -> assertFailure ("OpenAI parsing failed on nullable fingerprint: " ++ err)
            Right (msg, _) -> extractMessageText msg @?= "OK"
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
    , testCase "regression_webscraper_invalid_url: Fails with structured ToolError" $ do
        res <-
          toolExecute webScraperTool (object ["url" .= ("invalid-url-protocol" :: Text)]) ::
            IO (Either LangchainError Text)
        case res of
          Left err -> assertBool "Error is ToolError" (case err of ToolError {} -> True; _ -> False)
          Right _ -> assertFailure "Expected tool error for invalid URL"
    ]

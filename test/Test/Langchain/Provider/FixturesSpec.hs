{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.FixturesSpec (tests) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Core.Stream (TokenUsage (..))
import Langchain.Provider.Anthropic (parseAnthropicResponse)
import Langchain.Provider.DeepSeek (extractReasoningChain)
import Langchain.Provider.Gemini (parseGeminiEmbedResponse, parseGeminiResponse)
import Langchain.Provider.OpenAI (parseOpenAIResponse)

loadFixture :: FilePath -> IO (Either String Value)
loadFixture fp = do
  content <- LBS.readFile fp
  case decode content of
    Nothing -> pure $ Left ("Failed to decode JSON from fixture: " ++ fp)
    Just val -> pure $ Right val

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.FixturesSpec"
    [ testCase "Parse OpenAI Chat Completion fixture" $ do
        eVal <- loadFixture "test/fixtures/openai_chat_response.json"
        case eVal of
          Left err -> assertFailure err
          Right val -> case parseOpenAIResponse val of
            Left parseErr -> assertFailure ("OpenAI parser error: " ++ parseErr)
            Right (msg, mbUsage) -> do
              messageRole msg @?= Assistant
              extractMessageText msg @?= "Hello! I am OpenAI GPT-4o."
              case mbUsage of
                Nothing -> assertFailure "Expected TokenUsage in response"
                Just usage -> promptTokens usage @?= 9
    , testCase "Parse OpenAI Tool Call fixture" $ do
        eVal <- loadFixture "test/fixtures/openai_tool_call.json"
        case eVal of
          Left err -> assertFailure err
          Right val -> case parseOpenAIResponse val of
            Left parseErr -> assertFailure ("OpenAI parser error: " ++ parseErr)
            Right (msg, _) -> do
              messageRole msg @?= Assistant
              case messageToolCalls msg of
                Just [tc] -> do
                  toolCallName tc @?= "calculator"
                  toolCallId tc @?= "call_abc123"
                _ -> assertFailure "Expected tool call in OpenAI message"
    , testCase "Parse Anthropic Claude fixture" $ do
        eVal <- loadFixture "test/fixtures/anthropic_response.json"
        case eVal of
          Left err -> assertFailure err
          Right val -> case parseAnthropicResponse val of
            Left parseErr -> assertFailure ("Anthropic parser error: " ++ parseErr)
            Right (msg, mbUsage) -> do
              messageRole msg @?= Assistant
              extractMessageText msg @?= "Hello! I am Claude 3.5 Sonnet."
              case mbUsage of
                Nothing -> assertFailure "Expected TokenUsage"
                Just usage -> promptTokens usage @?= 10
    , testCase "Parse Gemini Chat fixture" $ do
        eVal <- loadFixture "test/fixtures/gemini_response.json"
        case eVal of
          Left err -> assertFailure err
          Right val -> case parseGeminiResponse val of
            Left parseErr -> assertFailure ("Gemini parser error: " ++ parseErr)
            Right msg -> do
              messageRole msg @?= Assistant
              extractMessageText msg @?= "Hello! I am Google Gemini 2.5."
    , testCase "Parse Gemini Embeddings fixture" $ do
        eVal <- loadFixture "test/fixtures/gemini_embed_response.json"
        case eVal of
          Left err -> assertFailure err
          Right val -> case parseGeminiEmbedResponse val of
            Left parseErr -> assertFailure ("Gemini embed parser error: " ++ parseErr)
            Right vec -> do
              length vec @?= 3
              vec @?= [0.0123, -0.0456, 0.0789]
    , testCase "Parse DeepSeek Reasoning fixture" $ do
        eVal <- loadFixture "test/fixtures/deepseek_reasoning_response.json"
        case eVal of
          Left err -> assertFailure err
          Right val -> case parseOpenAIResponse val of
            Left parseErr -> assertFailure ("DeepSeek OpenAI parser error: " ++ parseErr)
            Right (msg, _) -> do
              let txt = extractMessageText msg
                  (mbThink, cleanTxt) = extractReasoningChain txt
              case mbThink of
                Nothing -> assertFailure "Expected extracted reasoning <think> block"
                Just thinkTxt -> assertBool "Reasoning text contains proof" ("step-by-step" `T.isInfixOf` thinkTxt)
              assertBool "Clean text contains answer 42" ("42" `T.isInfixOf` cleanTxt)
    ]

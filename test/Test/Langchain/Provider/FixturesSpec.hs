{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.FixturesSpec (tests) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Core.Stream (TokenUsage (..))
import Langchain.Provider.Gemini (parseGeminiResponse)
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
    , testCase "Parse Gemini Chat fixture" $ do
        eVal <- loadFixture "test/fixtures/gemini_response.json"
        case eVal of
          Left err -> assertFailure err
          Right val -> case parseGeminiResponse val of
            Left parseErr -> assertFailure ("Gemini parser error: " ++ parseErr)
            Right msg -> do
              messageRole msg @?= Assistant
              extractMessageText msg @?= "Hello! I am Google Gemini 2.5."
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Core.Stream (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Except (runExceptT)

import Langchain.Core.Model
import Langchain.Core.Stream

tests :: TestTree
tests =
  testGroup
    "Langchain.Core.Stream"
    [ testCase "stream emits LLMStart, LLMChunk, LLMEnd events" $ do
        let model = MockModel "Streamed content" "mock-gpt"
            input = [userMessage "Stream test"]
        res <- runExceptT $ collectEvents (stream model input Nothing)
        case res of
          Left err -> assertFailure $ "Unexpected stream error: " ++ show err
          Right events -> do
            length events @?= 3
            case events of
              [s@LLMStart {}, c@LLMChunk {}, e@LLMEnd {}] -> do
                modelName s @?= "mock-gpt"
                chunkText c @?= "Streamed content"
                extractMessageText (finalMessage e) @?= "Streamed content"
              _ -> assertFailure $ "Unexpected event sequence: " ++ show events
    ]

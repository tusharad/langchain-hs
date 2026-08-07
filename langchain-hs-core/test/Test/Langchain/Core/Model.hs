{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Core.Model (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Except (runExceptT)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error
import Langchain.Core.Model

tests :: TestTree
tests =
  testGroup
    "Langchain.Core.Model"
    [ testGroup
        "Multi-Modal ContentBlock & Message"
        [ testCase "textMessage creates User message with TextBlock" $ do
            let msg = userMessage "Hello AI"
            messageRole msg @?= User
            extractMessageText msg @?= "Hello AI\n"
        , testCase "systemMessage creates System message" $ do
            let msg = systemMessage "You are a assistant"
            messageRole msg @?= System
            extractMessageText msg @?= "You are a assistant\n"
        , testCase "imageMessage creates ImageBlock message" $ do
            let msg = imageMessage User "image/png" "base64data=="
            messageRole msg @?= User
            case messageContents msg of
              (ImageBlock mime b64 :| []) -> do
                mime @?= "image/png"
                b64 @?= "base64data=="
              _ -> assertFailure "Expected ImageBlock"
        ]
    , testGroup
        "Effect-Polymorphic ChatModel (MockModel)"
        [ testCase "invoke returns Assistant response" $ do
            let model = MockModel "Hello human" "mock-gpt"
                input = [userMessage "Hi"]
            res <- runExceptT $ invoke model input Nothing
            case res of
              Left err -> assertFailure $ "Unexpected error: " ++ show err
              Right msg -> do
                messageRole msg @?= Assistant
                extractMessageText msg @?= "Hello human\n"
        , testCase "batch processes multiple inputs sequentially" $ do
            let model = MockModel "Pong" "mock-gpt"
                inputs = [[userMessage "Ping 1"], [userMessage "Ping 2"]]
            res <- runExceptT $ batch model inputs Nothing
            case res of
              Left err -> assertFailure $ "Unexpected error: " ++ show err
              Right msgs -> do
                length msgs @?= 2
                map extractMessageText msgs @?= ["Pong\n", "Pong\n"]
        ]
    ]

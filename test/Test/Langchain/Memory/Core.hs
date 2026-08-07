{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.Core (tests) where

import Control.Concurrent.Async (forConcurrently_)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Error (toString)
import Langchain.LLM.Core (Message (..), Role (..), defaultMessageData)
import Langchain.Memory.Core
import Langchain.Runnable.Core
import Test.Tasty
import Test.Tasty.HUnit

systemMsg :: Text -> Message
systemMsg text = Message System text defaultMessageData

userMsg :: Text -> Message
userMsg text = Message User text defaultMessageData

aiMsg :: Text -> Message
aiMsg text = Message Assistant text defaultMessageData

utilityTests :: TestTree
utilityTests =
  testGroup
    "Utility Functions Tests"
    [ testCase "initialChatMessage should create chat with system message" $ do
        let prompt = "You are a helpful assistant"
            result = initialChatMessage prompt
        NE.length result @?= 1
        NE.head result @?= systemMsg prompt
    , testCase "trimChatMessage should keep specified number of messages" $ do
        let msgs =
              NE.fromList
                [ systemMsg "System"
                , userMsg "User1"
                , aiMsg "AI1"
                , userMsg "User2"
                ]
            trimmed = trimChatMessage 2 msgs
        NE.length trimmed @?= 2
        NE.toList trimmed @?= [aiMsg "AI1", userMsg "User2"]
    , testCase "trimChatMessage should keep all messages if n >= length" $ do
        let msgs = NE.fromList [systemMsg "System", userMsg "User1"]
            trimmed = trimChatMessage 3 msgs
        NE.length trimmed @?= 2
        NE.toList trimmed @?= [systemMsg "System", userMsg "User1"]
    , testCase "trimChatMessage should handle minimum size of 1" $ do
        let msgs = NE.fromList [systemMsg "System", userMsg "User1", aiMsg "AI1"]
            trimmed = trimChatMessage 1 msgs
        NE.length trimmed @?= 1
        NE.toList trimmed @?= [aiMsg "AI1"]
    , testCase "addAndTrim should add message and trim history" $ do
        let msgs = NE.fromList [systemMsg "System", userMsg "User1", aiMsg "AI1"]
            newMsg = userMsg "User2"
            result = addAndTrim 2 newMsg msgs
        NE.length result @?= 2
        NE.toList result @?= [aiMsg "AI1", userMsg "User2"]
    ]

windowBufferMemoryTests :: TestTree
windowBufferMemoryTests =
  testGroup
    "WindowBufferMemory Tests"
    [ testCase "messages should return current messages" $ do
        let initialMsgs = NE.fromList [systemMsg "System"]
            memory = WindowBufferMemory 3 initialMsgs
        result <- messages memory
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
          Right msgs -> msgs @?= initialMsgs
    , testCase "addMessage should add message when under capacity" $ do
        let initialMsgs = NE.fromList [systemMsg "System"]
            memory = WindowBufferMemory 3 initialMsgs
            newMsg = userMsg "User1"
        result <- addMessage memory newMsg
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
          Right newMemory -> do
            msgsResult <- messages newMemory
            case msgsResult of
              Left err ->
                assertFailure $
                  "Expected Right but got Left: " ++ toString err
              Right msgs ->
                NE.toList msgs
                  @?= [ systemMsg "System"
                      , userMsg "User1"
                      ]
    , testCase "addMessage should maintain max window size" $ do
        let initialMsgs =
              NE.fromList
                [ systemMsg "System"
                , userMsg "User1"
                , aiMsg "AI1"
                ]
            memory = WindowBufferMemory 3 initialMsgs
            newMsg = userMsg "User2"
        result <- addMessage memory newMsg
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
          Right newMemory -> do
            msgsResult <- messages newMemory
            case msgsResult of
              Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
              Right msgs -> do
                NE.length msgs @?= 3
                NE.toList msgs
                  @?= [ systemMsg "System"
                      , aiMsg "AI1"
                      , userMsg "User2"
                      ]
    , testCase "addUserMessage should add message with User role" $ do
        let initialMsgs = NE.fromList [systemMsg "System"]
            memory = WindowBufferMemory 3 initialMsgs
        result <- addUserMessage memory "Hello"
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
          Right newMemory -> do
            msgsResult <- messages newMemory
            case msgsResult of
              Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
              Right msgs -> do
                NE.length msgs @?= 2
                NE.toList msgs @?= [systemMsg "System", userMsg "Hello"]
    , testCase "addAiMessage should add message with Assistant role" $ do
        let initialMsgs = NE.fromList [systemMsg "System"]
            memory = WindowBufferMemory 3 initialMsgs
        result <- addAiMessage memory "I can help"
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
          Right newMemory -> do
            msgsResult <- messages newMemory
            case msgsResult of
              Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
              Right msgs -> do
                NE.length msgs @?= 2
                NE.toList msgs
                  @?= [ systemMsg "System"
                      , aiMsg "I can help"
                      ]
    , testCase "clear should reset to just system message" $ do
        let initialMsgs =
              NE.fromList
                [ systemMsg "System"
                , userMsg "User1"
                , aiMsg "AI1"
                ]
            memory = WindowBufferMemory 3 initialMsgs
        result <- clear memory
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
          Right newMemory -> do
            msgsResult <- messages newMemory
            case msgsResult of
              Left err ->
                assertFailure $
                  "Expected Right but got Left: "
                    ++ toString err
              Right msgs -> do
                NE.length msgs @?= 1
                NE.head msgs @?= systemMsg "You are an AI model"
    ]

runnableTests :: TestTree
runnableTests =
  testGroup
    "Runnable Instance Tests"
    [ testCase "invoke should add user message" $ do
        let initialMsgs = NE.fromList [systemMsg "System"]
            memory = WindowBufferMemory 3 initialMsgs
        result <- invoke memory "Test input"
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
          Right newMemory -> do
            msgsResult <- messages newMemory
            case msgsResult of
              Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
              Right msgs -> do
                NE.length msgs @?= 2
                NE.toList msgs @?= [systemMsg "System", userMsg "Test input"]
    ]

concurrencyTests :: TestTree
concurrencyTests =
  testGroup
    "Concurrency Tests"
    [ testCase "100 concurrent writes produce consistent window size" $ do
        let initialMsgs = NE.fromList [systemMsg "System"]
            maxSize = 200
            memory = WindowBufferMemory maxSize initialMsgs
        forConcurrently_ [1 .. 100 :: Int] $ \i -> do
          _ <- addMessage memory (userMsg $ "Msg " <> T.pack (show i))
          pure ()
        result <- messages memory
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
          Right msgs -> do
            -- Initial system message + 100 user messages = 101 messages
            NE.length msgs @?= 101
    ]

tests :: TestTree
tests =
  testGroup
    "Langchain.Memory.Core Tests"
    [ utilityTests
    , windowBufferMemoryTests
    , runnableTests
    , concurrencyTests
    ]

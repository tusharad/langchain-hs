{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.Core (tests) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
  ( assistantMessage
  , systemMessage
  , userMessage
  )
import Langchain.Memory.Core

tests :: TestTree
tests =
  testGroup
    "Langchain.Memory.Core Tests"
    [ utilityTests
    , windowBufferMemoryTests
    , concurrencyTests
    ]

utilityTests :: TestTree
utilityTests =
  testGroup
    "Utility Functions Tests"
    [ testCase "initialMessages should create list with system message" $ do
        let prompt = "You are a helpful assistant"
            result = initialMessages prompt
        length result @?= 1
        case result of
          (m : _) -> m @?= systemMessage prompt
          [] -> assertFailure "Expected non-empty list"
    , testCase "trimMessages should keep specified number of messages" $ do
        let msgs =
              [ systemMessage "System"
              , userMessage "User1"
              , assistantMessage "AI1"
              , userMessage "User2"
              ]
            trimmed = trimMessages 2 msgs
        length trimmed @?= 2
        trimmed @?= [assistantMessage "AI1", userMessage "User2"]
    , testCase "trimMessages should keep all messages if n >= length" $ do
        let msgs = [systemMessage "System", userMessage "User1"]
            trimmed = trimMessages 3 msgs
        length trimmed @?= 2
        trimmed @?= [systemMessage "System", userMessage "User1"]
    ]

windowBufferMemoryTests :: TestTree
windowBufferMemoryTests =
  testGroup
    "WindowBufferMemory Tests"
    [ testCase "messages should return current messages" $ do
        let initialMsgs = [systemMessage "System"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= initialMsgs
    , testCase "addMessage should add message when under capacity" $ do
        let initialMsgs = [systemMessage "System"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          addMessage memory (userMessage "User1")
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= [systemMessage "System", userMessage "User1"]
    , testCase "addMessage should maintain max window size" $ do
        let initialMsgs =
              [ systemMessage "System"
              , userMessage "User1"
              , assistantMessage "AI1"
              ]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          addMessage memory (userMessage "User2")
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> do
            length msgs @?= 3
            msgs @?= [systemMessage "System", assistantMessage "AI1", userMessage "User2"]
    , testCase "addUserMessage should add message with User role" $ do
        let initialMsgs = [systemMessage "System"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          addUserMessage memory "Hello"
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= [systemMessage "System", userMessage "Hello"]
    , testCase "addAiMessage should add message with Assistant role" $ do
        let initialMsgs = [systemMessage "System"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          addAiMessage memory "I can help"
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= [systemMessage "System", assistantMessage "I can help"]
    , testCase "clear should reset to default system message" $ do
        let initialMsgs =
              [ systemMessage "System"
              , userMessage "User1"
              , assistantMessage "AI1"
              ]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          clear memory
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> do
            length msgs @?= 1
            case msgs of
              (m : _) -> m @?= systemMessage "You are a helpful AI assistant"
              [] -> assertFailure "Expected non-empty messages"
    ]

concurrencyTests :: TestTree
concurrencyTests =
  testGroup
    "Concurrency Tests"
    [ testCase "100 concurrent writes produce consistent window size" $ do
        let initialMsgs = [systemMessage "System"]
            maxSize = 200
        memory <- newWindowBufferMemory maxSize initialMsgs
        forConcurrently_ [1 .. 100 :: Int] $ \i -> do
          _ <- runExceptT $ addMessage memory (userMessage $ "Msg " <> T.pack (show i))
          pure ()
        result <- runExceptT $ messages memory
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> length msgs @?= 101
    ]

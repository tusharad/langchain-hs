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
    [ testCase "initialMessages creates list with a single system message" $ do
        let result = initialMessages "You are a helpful assistant"
        length result @?= 1
        case result of
          (m : _) -> m @?= systemMessage "You are a helpful assistant"
          [] -> assertFailure "Expected non-empty list"
    , testCase "trimMessages keeps last n messages (non-system)" $ do
        let msgs =
              [ systemMessage "System"
              , userMessage "User1"
              , assistantMessage "AI1"
              , userMessage "User2"
              ]
            trimmed = trimMessages 2 msgs
        trimmed @?= [assistantMessage "AI1", userMessage "User2"]
    ]

windowBufferMemoryTests :: TestTree
windowBufferMemoryTests =
  testGroup
    "WindowBufferMemory Tests"
    [ testCase "messages returns current messages" $ do
        let initialMsgs = [systemMessage "System"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= initialMsgs
    , testCase "addMessage adds message when under capacity" $ do
        let initialMsgs = [systemMessage "System"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          addMessage memory (userMessage "User1")
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= [systemMessage "System", userMessage "User1"]
    , testCase "addMessage trims oldest non-system message when at capacity" $ do
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
          Right msgs ->
            msgs @?= [systemMessage "System", assistantMessage "AI1", userMessage "User2"]
    , testCase "clear resets to default system message" $ do
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

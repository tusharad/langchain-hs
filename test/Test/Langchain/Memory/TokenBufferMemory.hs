{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.TokenBufferMemory (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Langchain.Core.Error (errorMessage)
import Langchain.Core.Model
  ( assistantMessage
  , systemMessage
  , userMessage
  )
import Langchain.Memory.Core (BaseMemory (..))
import qualified Langchain.Memory.Core as TB

tests :: TestTree
tests =
  testGroup
    "TokenBufferMemory Tests"
    [ constructorTests
    , addMessageTests
    , addUserAndAiMessageTests
    , clearTest
    ]

constructorTests :: TestTree
constructorTests =
  testGroup
    "Constructor Tests"
    [ testCase "TokenBufferMemory initializes with system message" $ do
        mem <- TB.newTokenBufferMemory 100 [systemMessage "You are an AI model"]
        TB.maxTokens mem @?= 100
        res <- runExceptT $ messages mem
        res @?= Right [systemMessage "You are an AI model"]
    ]

addMessageTests :: TestTree
addMessageTests =
  testGroup
    "addMessage logic"
    [ testCase "Add message within token limit" $ do
        let sysMsg = systemMessage "sys"
            user1 = userMessage "12345678"
            user2 = userMessage "12345678"
        mem <- TB.newTokenBufferMemory 10 [sysMsg, user1]
        res <- runExceptT $ do
          addMessage mem user2
          messages mem
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= [sysMsg, user1, user2]
    , testCase "Evicts oldest non-system message when exceeding limit" $ do
        let sysMsg = systemMessage "sys!"
            user1 = userMessage "12345678"
            user2 = userMessage "12345678"
        mem <- TB.newTokenBufferMemory 4 [sysMsg, user1]
        res <- runExceptT $ do
          addMessage mem user2
          messages mem
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= [sysMsg, user2]
    , testCase "Error when message itself exceeds limit" $ do
        let sysMsg = systemMessage "12345678"
            userMsg = userMessage "12345678901234567890" -- 5 tokens
        mem <- TB.newTokenBufferMemory 3 [sysMsg]
        res <- runExceptT $ addMessage mem userMsg
        case res of
          Left err ->
            assertBool "Should contain limit error message" ("exceeds" `T.isInfixOf` errorMessage err)
          Right _ -> assertFailure "Expected Left due to overflow"
    , testCase "BaseMemory messages retrieves history" $ do
        mem <- TB.newTokenBufferMemory 10 [systemMessage "init"]
        res <- runExceptT $ messages mem
        res @?= Right [systemMessage "init"]
    ]

addUserAndAiMessageTests :: TestTree
addUserAndAiMessageTests =
  testGroup
    "addUserMessage and addAiMessage"
    [ testCase "addUserMessage adds User role message" $ do
        mem <- TB.newTokenBufferMemory 100 [systemMessage ""]
        res <- runExceptT $ do
          addUserMessage mem "Hello!"
          messages mem
        case res of
          Right msgs -> msgs @?= [systemMessage "", userMessage "Hello!"]
          Left err -> assertFailure $ "Unexpected Left: " ++ show err
    , testCase "addAiMessage adds Assistant role message" $ do
        mem <- TB.newTokenBufferMemory 100 [systemMessage ""]
        res <- runExceptT $ do
          addAiMessage mem "I'm an assistant."
          messages mem
        case res of
          Right msgs -> msgs @?= [systemMessage "", assistantMessage "I'm an assistant."]
          Left err -> assertFailure $ "Unexpected Left: " ++ show err
    ]

clearTest :: TestTree
clearTest =
  testCase "clear resets messages to default system message" $ do
    mem <- TB.newTokenBufferMemory 100 [userMessage "old"]
    res <- runExceptT $ do
      clear mem
      messages mem
    case res of
      Right msgs -> msgs @?= [systemMessage "You are a helpful AI assistant"]
      Left _ -> assertFailure "Clear failed unexpectedly"

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.TokenBufferMemory (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Langchain.Core.Error (errorMessage)
import Langchain.Core.Model
  ( systemMessage
  , userMessage
  )
import Langchain.Memory.Core (BaseMemory (..))
import qualified Langchain.Memory.Core as TB

tests :: TestTree
tests =
  testGroup
    "TokenBufferMemory Tests"
    [ testCase "Initializes with provided messages" $ do
        mem <- TB.newTokenBufferMemory 100 [systemMessage "You are an AI model"]
        TB.maxTokens mem @?= 100
        res <- runExceptT $ messages mem
        res @?= Right [systemMessage "You are an AI model"]
    , testCase "Adds message within token limit" $ do
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
    , testCase "Evicts oldest non-system message when exceeding token limit" $ do
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
    , testCase "Returns error when message itself exceeds token limit" $ do
        let sysMsg = systemMessage "12345678"
            userMsg = userMessage "12345678901234567890"
        mem <- TB.newTokenBufferMemory 3 [sysMsg]
        res <- runExceptT $ addMessage mem userMsg
        case res of
          Left err ->
            assertBool "Error mentions exceeds" ("exceeds" `T.isInfixOf` errorMessage err)
          Right _ -> assertFailure "Expected Left due to overflow"
    , testCase "clear resets to default system message" $ do
        mem <- TB.newTokenBufferMemory 100 [userMessage "old"]
        res <- runExceptT $ do
          clear mem
          messages mem
        case res of
          Right msgs -> msgs @?= [systemMessage "You are a helpful AI assistant"]
          Left _ -> assertFailure "Clear failed unexpectedly"
    ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Runnable.ConversationChains (tests) where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Langchain.LLM.Core
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.PromptTemplate (PromptTemplate (..))
import Langchain.Runnable.ConversationChain
import Langchain.Runnable.Core
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
import Data.Text (Text)

data TestMemory = TestMemory (IORef [Message])

instance BaseMemory TestMemory where
  addUserMessage (TestMemory ref) input = do
    let userMsg = Message User input defaultMessageData
    modifyIORef ref (++ [userMsg])
    return $ Right (TestMemory ref)

  addAiMessage (TestMemory ref) response = do
    let aiMsg = Message Assistant response defaultMessageData
    modifyIORef ref (++ [aiMsg])
    return $ Right (TestMemory ref)

  addMessage (TestMemory ref) msg = do
    modifyIORef ref (++ [msg])
    return $ Right (TestMemory ref)

  clear (TestMemory ref) = do
    (modifyIORef ref (const []))
    return $ Right $ TestMemory ref

  messages (TestMemory ref) = fmap Right (NE.fromList <$> readIORef ref)

data FailingMemory = FailingMemory

instance BaseMemory FailingMemory where
  addUserMessage _ _ = return $ Left "Memory error"
  addAiMessage _ _ = return $ Left "Memory error"
  messages _ = return $ Left "memory error"
  addMessage _ _ = return $ Left "memory error"
  clear _ = return $ Left "memory error"

data MockLLM = MockLLM
  { llmResponse :: Either String Message
  , receivedMessages :: IORef [Message]
  }

instance LLM MockLLM where
  type LLMParams MockLLM = String
  type LLMStreamTokenType MockLLM = Text

  chat llm0 (msgs :: NonEmpty Message) _ = do
    writeIORef (receivedMessages llm0) (NE.toList msgs)
    return (llmResponse llm0)
  generate = undefined
  stream = undefined

tests :: TestTree
tests =
  testGroup
    "ConversationChain Tests"
    [ testCase "Basic conversation flow" $ do
        memRef <- newIORef []
        let testMem = TestMemory memRef
        msgRef <- newIORef []
        let mockLLM = MockLLM (Right $ Message User "Hello!" defaultMessageData) msgRef
            chain = ConversationChain testMem mockLLM (PromptTemplate "")
        result <- invoke chain "Hi"
        result @?= Right "Hello!"
        -- Verify LLM received correct messages
        received <- readIORef msgRef
        assertEqual "LLM received user message" [Message User "Hi" defaultMessageData] received
        -- Verify memory contains both messages
        mem <- readIORef memRef
        assertEqual
          "Memory has user and AI messages"
          [ Message User "Hi" defaultMessageData
          , Message Assistant "Hello!" defaultMessageData
          ]
          mem
    , testCase "Error adding user message" $ do
        nRef <- newIORef []
        let failingMem = FailingMemory
            mockLLM = MockLLM (Right $ Message User "" defaultMessageData) nRef
            chain = ConversationChain failingMem mockLLM (PromptTemplate "")
        result <- invoke chain "Hi"
        result @?= Left "Memory error"
    , testCase "LLM returns error" $ do
        memRef <- newIORef []
        let testMem = TestMemory memRef
        msgRef <- newIORef []
        let mockLLM = MockLLM (Left "LLM error") msgRef
            chain = ConversationChain testMem mockLLM (PromptTemplate "")
        result <- invoke chain "Hi"
        result @?= Left "LLM error"
        -- Verify only user message in memory
        mem <- readIORef memRef
        assertEqual "Only user message in memory" [Message User "Hi" defaultMessageData] mem
    , testCase "Memory update after response" $ do
        memRef <- newIORef []
        nRef <- newIORef []
        let testMem = TestMemory memRef
            mockLLM = MockLLM (Right $ Message User "Response" defaultMessageData) nRef
            chain = ConversationChain testMem mockLLM (PromptTemplate "")
        _ <- invoke chain "Test"
        mem <- readIORef memRef
        assertEqual "Memory contains both messages" 2 (length mem)
    ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Memory.Core
Description : Memory management for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implementation of LangChain's memory management patterns, providing:

- Chat history tracking with size limits
- Message addition/trimming strategies
- Integration with Runnable workflows

Example usage:

@
-- Create memory with 5-message window
memory = WindowBufferMemory 5 (initialChatMessage "You are an assistant")

-- Add user message
newMemory <- addUserMessage memory "Hello, world!"

-- Retrieve current messages
messages <- messages newMemory
-- Right [Message System "...", Message User "Hello, world!"]
@
-}
module Langchain.Memory.Core
  ( BaseMemory (..)
  , WindowBufferMemory (..)
  , trimChatMessage
  , addAndTrim
  , initialChatMessage
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Langchain.LLM.Core
  ( ChatMessage
  , Message (..)
  , Role (..)
  , defaultMessageData
  )
import Langchain.Runnable.Core

{- | Base typeclass for memory implementations
Defines standard operations for chat history management.

Example instance:

@
instance BaseMemory MyMemory where
  messages = ...
  addUserMessage = ...
@
-}
class BaseMemory mem where
  -- | Retrieve current chat history
  messages :: mem -> IO (Either String ChatMessage)

  -- | Add user message to history
  addUserMessage :: mem -> Text -> IO (Either String mem)

  -- | Add AI response to history
  addAiMessage :: mem -> Text -> IO (Either String mem)

  -- | Add generic message to history
  addMessage :: mem -> Message -> IO (Either String mem)

  -- | Reset memory to initial state
  clear :: mem -> IO (Either String mem)

  messagesM :: MonadIO m => mem -> m (Either String ChatMessage)
  messagesM = liftIO . messages

  addUserMessageM :: MonadIO m => mem -> Text -> m (Either String mem)
  addUserMessageM mem msg = liftIO $ addUserMessage mem msg

  addAiMessageM :: MonadIO m => mem -> Text -> m (Either String mem)
  addAiMessageM mem msg = liftIO $ addAiMessage mem msg

  addMessageM :: MonadIO m => mem -> Message -> m (Either String mem)
  addMessageM mem msg = liftIO $ addMessage mem msg

  clearM :: MonadIO m => mem -> m (Either String mem)
  clearM mem = liftIO $ clear mem

{- | Sliding window memory implementation.
Stores chat history with maximum size limit.

Note: This implementation will not trim system messages.

Example:

>>> let mem = WindowBufferMemory 2 (NE.singleton (Message System "Sys" defaultMessageData))
>>> addMessage mem (Message User "Hello" defaultMessageData)
Right (WindowBufferMemory {maxWindowSize = 2, ...})
-}
data WindowBufferMemory = WindowBufferMemory
  { maxWindowSize :: Int
  {- ^ Maximum number of messages to retain
  ^ It is user's responsiblity to make sure the number is > 0.
  -}
  , windowBufferMessages :: ChatMessage
  -- ^ Current message buffer
  }
  deriving (Show, Eq)

instance BaseMemory WindowBufferMemory where
  -- \| Get current messages
  --
  --  Example:
  --
  --  >>> messages (WindowBufferMemory 5 initialMessages)
  --  Right initialMessages
  --
  messages WindowBufferMemory {..} = pure $ Right windowBufferMessages

  -- \| Add message with window trimming
  --
  --  Example:
  --
  --  >>> let mem = WindowBufferMemory 2 (NE.fromList [msg1])
  --  >>> addMessage mem msg2
  --  Right (WindowBufferMemory {windowBufferMessages = [msg1, msg2]})
  --
  --  >>> addMessage mem msg3
  --  Right (WindowBufferMemory {windowBufferMessages = [msg2, msg3]})
  --
  addMessage winBuffMem@WindowBufferMemory {..} newMsg = do
    let currentMsgs = NE.toList windowBufferMessages
        newMsgs = currentMsgs ++ [newMsg]

    if length newMsgs > maxWindowSize
      then do
        let trimmedMsgs = removeOldestNonSystem newMsgs
        pure $
          Right $
            winBuffMem {windowBufferMessages = NE.fromList trimmedMsgs}
      else
        pure $ Right $ winBuffMem {windowBufferMessages = NE.fromList newMsgs}
    where
      isSystem (Message role _ _) = role == System

      removeOldestNonSystem = go
        where
          go [] = []
          go (m : ms)
            | isSystem m = m : go ms
            | otherwise = ms

  -- \| Add user message
  --
  --  Example:
  --
  --  >>> addUserMessage mem "Hello"
  --  Right (WindowBufferMemory { ... })
  --
  addUserMessage winBuffMem uMsg =
    addMessage winBuffMem (Message User uMsg defaultMessageData)

  -- \| Add AI message
  --
  --  Example:
  --
  --  >>> addAiMessage mem "Response"
  --  Right (WindowBufferMemory { ... })
  --
  addAiMessage winBuffMem uMsg =
    addMessage winBuffMem (Message Assistant uMsg defaultMessageData)

  -- \| Reset to initial system message
  --
  --  Example:
  --
  --  >>> clear mem
  --  Right (WindowBufferMemory { windowBufferMessages = [systemMsg] })
  --
  clear winBuffMem =
    pure $
      Right $
        winBuffMem
          { windowBufferMessages =
              NE.singleton $
                Message System "You are an AI model" defaultMessageData
          }

{- | Trim chat history to last n messages
Example:

>>> let msgs = NE.fromList [msg1, msg2, msg3]
>>> trimChatMessage 2 msgs
[msg2, msg3]
-}
trimChatMessage :: Int -> ChatMessage -> ChatMessage
trimChatMessage n msgs =
  NE.fromList $
    drop (max 0 (NE.length msgs - n)) (NE.toList msgs)

{- | Add and maintain window size
Example:

>>> let msgs = NE.fromList [msg1]
>>> addAndTrim 2 msg2 msgs
[msg1, msg2]
-}
addAndTrim :: Int -> Message -> ChatMessage -> ChatMessage
addAndTrim n msg msgs = trimChatMessage n (msgs <> NE.singleton msg)

{- | Create initial chat history
Example:

>>> initialChatMessage "You are Qwen"
[Message System "You are Qwen"]
-}
initialChatMessage :: Text -> ChatMessage
initialChatMessage systemPrompt =
  NE.singleton $
    Message System systemPrompt defaultMessageData

instance Runnable WindowBufferMemory where
  type RunnableInput WindowBufferMemory = Text
  type RunnableOutput WindowBufferMemory = WindowBufferMemory

  -- \| Runnable interface for user input
  --
  --  Example:
  --
  --  >>> invoke memory "Hello"
  --  Right (WindowBufferMemory { ... })
  --
  invoke = addUserMessage

{- $examples
Test case patterns:
1. Message trimming
   >>> let mem = WindowBufferMemory 2 [msg1, msg2]
   >>> addMessage mem msg3
   Right [msg2, msg3]

2. Initial state
   >>> messages (WindowBufferMemory 5 initialMessages)
   Right initialMessages

3. Runnable integration
   >>> run (WindowBufferMemory 5 initialMessages) "Hello"
   Right (WindowBufferMemory { ... })
-}

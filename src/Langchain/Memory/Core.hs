{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

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
  , WindowBufferMemory (WindowBufferMemory)
  , maxWindowSize
  , windowBufferMessages
  , newWindowBufferMemory
  , trimChatMessage
  , addAndTrim
  , initialChatMessage
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Langchain.Error (LangchainResult)
import Langchain.LLM.Core
  ( ChatHistory
  , Message (..)
  , Role (..)
  , defaultMessageData
  )
import Langchain.Runnable.Core
import System.IO.Unsafe (unsafePerformIO)

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
  messages :: mem -> IO (LangchainResult ChatHistory)

  -- | Add user message to history
  addUserMessage :: mem -> Text -> IO (LangchainResult mem)

  -- | Add AI response to history
  addAiMessage :: mem -> Text -> IO (LangchainResult mem)

  -- | Add generic message to history
  addMessage :: mem -> Message -> IO (LangchainResult mem)

  -- | Reset memory to initial state
  clear :: mem -> IO (LangchainResult mem)

  messagesM :: MonadIO m => mem -> m (LangchainResult ChatHistory)
  messagesM = liftIO . messages

  addUserMessageM :: MonadIO m => mem -> Text -> m (LangchainResult mem)
  addUserMessageM mem msg = liftIO $ addUserMessage mem msg

  addAiMessageM :: MonadIO m => mem -> Text -> m (LangchainResult mem)
  addAiMessageM mem msg = liftIO $ addAiMessage mem msg

  addMessageM :: MonadIO m => mem -> Message -> m (LangchainResult mem)
  addMessageM mem msg = liftIO $ addMessage mem msg

  clearM :: MonadIO m => mem -> m (LangchainResult mem)
  clearM mem = liftIO $ clear mem

{- | Internal representation of sliding window memory backed by 'TVar' for thread safety.
-}
data WindowBufferMemory = WindowBufferMemoryInternal !Int !(TVar ChatHistory)

-- | Retrieve the current message buffer from a 'WindowBufferMemory'.
windowBufferMessages :: WindowBufferMemory -> ChatHistory
windowBufferMessages (WindowBufferMemoryInternal _ tv) = unsafePerformIO (readTVarIO tv)

-- | Extract max window size.
maxWindowSize :: WindowBufferMemory -> Int
maxWindowSize (WindowBufferMemoryInternal sz _) = sz

-- | Pattern synonym providing backward compatibility for 'WindowBufferMemory' positional
-- construction and pattern matching, backed internally by an STM 'TVar'.
pattern WindowBufferMemory :: Int -> ChatHistory -> WindowBufferMemory
pattern WindowBufferMemory sz msgs <- (getWindowBufferMemoryPair -> (sz, msgs))
  where
    WindowBufferMemory sz msgs = unsafePerformIO $ do
      tv <- newTVarIO msgs
      pure $ WindowBufferMemoryInternal sz tv

getWindowBufferMemoryPair :: WindowBufferMemory -> (Int, ChatHistory)
getWindowBufferMemoryPair (WindowBufferMemoryInternal sz tv) =
  (sz, unsafePerformIO (readTVarIO tv))

{-# COMPLETE WindowBufferMemory #-}

instance Show WindowBufferMemory where
  showsPrec d (WindowBufferMemory sz msgs) =
    showParen (d > 10) $
      showString "WindowBufferMemory {maxWindowSize = "
        . shows sz
        . showString ", windowBufferMessages = "
        . shows msgs
        . showString "}"

instance Eq WindowBufferMemory where
  WindowBufferMemory sz1 msgs1 == WindowBufferMemory sz2 msgs2 =
    sz1 == sz2 && msgs1 == msgs2

-- | Construct a thread-safe 'WindowBufferMemory' explicitly in 'MonadIO'.
newWindowBufferMemory :: MonadIO m => Int -> ChatHistory -> m WindowBufferMemory
newWindowBufferMemory sz msgs = liftIO $ do
  tv <- newTVarIO msgs
  pure $ WindowBufferMemoryInternal sz tv

instance BaseMemory WindowBufferMemory where
  -- | Get current messages from TVar
  messages (WindowBufferMemoryInternal _ tv) = do
    msgs <- readTVarIO tv
    pure $ Right msgs

  -- | Add message with STM atomic window trimming
  addMessage winBuffMem@(WindowBufferMemoryInternal maxSz tv) newMsg = do
    atomically $ modifyTVar' tv $ \currentHistory ->
      let currentMsgs = NE.toList currentHistory
          newMsgs = currentMsgs ++ [newMsg]
          trimmedMsgs =
            if length newMsgs > maxSz
              then removeOldestNonSystem newMsgs
              else newMsgs
       in NE.fromList trimmedMsgs
    pure $ Right winBuffMem
    where
      isSystem (Message role _ _) = role == System

      removeOldestNonSystem = go
        where
          go [] = []
          go (m : ms)
            | isSystem m = m : go ms
            | otherwise = ms

  -- | Add user message
  addUserMessage winBuffMem uMsg =
    addMessage winBuffMem (Message User uMsg defaultMessageData)

  -- | Add AI message
  addAiMessage winBuffMem uMsg =
    addMessage winBuffMem (Message Assistant uMsg defaultMessageData)

  -- | Reset to initial system message atomically
  clear winBuffMem@(WindowBufferMemoryInternal _ tv) = do
    let sysMsg =
          NE.singleton $
            Message System "You are an AI model" defaultMessageData
    atomically $ writeTVar tv sysMsg
    pure $ Right winBuffMem

{- | Trim chat history to last n messages
Example:

>>> let msgs = NE.fromList [msg1, msg2, msg3]
>>> trimChatMessage 2 msgs
[msg2, msg3]
-}
trimChatMessage :: Int -> ChatHistory -> ChatHistory
trimChatMessage n msgs =
  NE.fromList $
    drop (max 0 (NE.length msgs - n)) (NE.toList msgs)

{- | Add and maintain window size
Example:

>>> let msgs = NE.fromList [msg1]
>>> addAndTrim 2 msg2 msgs
[msg1, msg2]
-}
addAndTrim :: Int -> Message -> ChatHistory -> ChatHistory
addAndTrim n msg msgs = trimChatMessage n (msgs <> NE.singleton msg)

{- | Create initial chat history
Example:

>>> initialChatMessage "You are Qwen"
[Message System "You are Qwen"]
-}
initialChatMessage :: Text -> ChatHistory
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

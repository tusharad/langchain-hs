{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Memory.Core
Description : Effect-polymorphic memory management for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Thread-safe, effect-polymorphic conversation memory interfaces using STM.
-}
module Langchain.Memory.Core
  ( BaseMemory (..)
  , WindowBufferMemory (..)
  , newWindowBufferMemory
  , trimMessages
  , initialMessages
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( Message (..)
  , Role (..)
  , assistantMessage
  , systemMessage
  , userMessage
  )

-- | Effect-polymorphic BaseMemory typeclass
class BaseMemory mem where
  -- | Retrieve current conversation messages
  messages ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    m [Message]

  -- | Add a user message to history
  addUserMessage ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    Text ->
    m ()
  addUserMessage mem txt = addMessage mem (userMessage txt)

  -- | Add an AI response message to history
  addAiMessage ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    Text ->
    m ()
  addAiMessage mem txt = addMessage mem (assistantMessage txt)

  -- | Add a structured message to history
  addMessage ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    Message ->
    m ()

  -- | Reset memory to initial state
  clear ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    m ()

-- | Sliding window memory backed by thread-safe STM TVar
data WindowBufferMemory = WindowBufferMemory
  { maxWindowSize :: !Int
  , memVar :: !(TVar [Message])
  }

instance Show WindowBufferMemory where
  show (WindowBufferMemory sz _) = "WindowBufferMemory { maxWindowSize = " ++ show sz ++ " }"

instance Eq WindowBufferMemory where
  (WindowBufferMemory sz1 tv1) == (WindowBufferMemory sz2 tv2) =
    sz1 == sz2 && tv1 == tv2

-- | Construct a thread-safe WindowBufferMemory in MonadIO
newWindowBufferMemory :: MonadIO m => Int -> [Message] -> m WindowBufferMemory
newWindowBufferMemory sz initMsgs = liftIO $ do
  tv <- newTVarIO initMsgs
  pure $ WindowBufferMemory sz tv

instance BaseMemory WindowBufferMemory where
  messages (WindowBufferMemory _ tv) = liftIO $ readTVarIO tv

  addMessage (WindowBufferMemory maxSz tv) newMsg = liftIO $ do
    atomically $ modifyTVar' tv $ \currMsgs ->
      let combined = currMsgs ++ [newMsg]
       in if length combined > maxSz
            then removeOldestNonSystem combined
            else combined
    where
      removeOldestNonSystem [] = []
      removeOldestNonSystem (m : ms)
        | messageRole m == System = m : removeOldestNonSystem ms
        | otherwise = ms

  clear (WindowBufferMemory _ tv) = liftIO $ do
    atomically $ writeTVar tv [systemMessage "You are a helpful AI assistant"]

-- | Pure helper to trim messages to last N
trimMessages :: Int -> [Message] -> [Message]
trimMessages n msgs = drop (max 0 (length msgs - n)) msgs

-- | Pure helper to construct initial system message history
initialMessages :: Text -> [Message]
initialMessages sysPrompt = [systemMessage sysPrompt]

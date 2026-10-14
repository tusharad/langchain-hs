{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Memory.Core
Description : Effect-polymorphic, thread-safe conversation memory interfaces
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Thread-safe, effect-polymorphic conversation memory interfaces using STM.
Includes sliding window buffers, token-budgeted buffers, unbounded conversation buffers,
context saving, variable loading for prompt interpolation, and JSON persistence.
-}
module Langchain.Memory.Core
  ( -- * Base Memory Interface
    BaseMemory (..)
  , SomeMemory (..)

    -- * Context Saving & Variable Loading
  , saveContext
  , loadMemoryVariables
  , formatHistory
  , formatHistoryWithLabels

    -- * Memory Implementations
  , ConversationBufferMemory (..)
  , newConversationBufferMemory
  , WindowBufferMemory (..)
  , newWindowBufferMemory
  , TokenBufferMemory (..)
  , newTokenBufferMemory
  , newTokenBufferMemoryWithStrategy
  , OverflowStrategy (..)
  , TokenCounter

    -- * Persistence & Serialization
  , exportMessages
  , saveMemoryToFile
  , loadMemoryFromFile

    -- * Utility Functions
  , countTokens
  , approximateTokenCount
  , trimMessages
  , initialMessages
  , resetToInitial
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Error (LangchainError, memoryError)
import Langchain.Model
  ( Message (..)
  , Role (..)
  , assistantMessage
  , extractMessageText
  , systemMessage
  , userMessage
  )

-- ---------------------------------------------------------------------------
-- Base Memory Interface
-- ---------------------------------------------------------------------------

-- | Effect-polymorphic BaseMemory typeclass providing full conversation management.
class BaseMemory mem where
  -- | Retrieve current conversation messages
  messages ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    m [Message]

  -- | Add a structured message to history
  addMessage ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    Message ->
    m ()

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

  -- | Reset memory to initial state
  clear ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    m ()

  -- | Import and replace conversation messages completely
  importMessages ::
    (MonadIO m, MonadError LangchainError m) =>
    mem ->
    [Message] ->
    m ()
  importMessages mem msgs = do
    clear mem
    mapM_ (addMessage mem) msgs

-- | Atomically record both user input and AI response in a single step
saveContext ::
  (BaseMemory mem, MonadIO m, MonadError LangchainError m) =>
  mem ->
  Text ->
  Text ->
  m ()
saveContext mem userTxt aiTxt = do
  addUserMessage mem userTxt
  addAiMessage mem aiTxt

-- | Load memory variables as a Map for direct prompt template interpolation
loadMemoryVariables ::
  (BaseMemory mem, MonadIO m, MonadError LangchainError m) =>
  mem ->
  m (Map Text Text)
loadMemoryVariables mem = do
  msgs <- messages mem
  pure $ Map.singleton "history" (formatHistory msgs)

-- | Format conversation messages to standard string representation
formatHistory :: [Message] -> Text
formatHistory = formatHistoryWithLabels "Human" "AI"

-- | Format conversation messages with custom role labels
formatHistoryWithLabels :: Text -> Text -> [Message] -> Text
formatHistoryWithLabels userLabel aiLabel msgs =
  T.intercalate "\n" (map formatMsg msgs)
  where
    formatMsg m =
      let prefix = case messageRole m of
            User -> userLabel <> ": "
            Assistant -> aiLabel <> ": "
            System -> "System: "
            Tool -> "Tool: "
            Function -> "Function: "
            Developer -> "Developer: "
       in prefix <> extractMessageText m

-- ---------------------------------------------------------------------------
-- Existential Wrapper
-- ---------------------------------------------------------------------------

-- | Existential wrapper for any 'BaseMemory' instance
data SomeMemory where
  SomeMemory :: (BaseMemory mem) => mem -> SomeMemory

instance BaseMemory SomeMemory where
  messages (SomeMemory m) = messages m
  addMessage (SomeMemory m) = addMessage m
  clear (SomeMemory m) = clear m
  addUserMessage (SomeMemory m) = addUserMessage m
  addAiMessage (SomeMemory m) = addAiMessage m
  importMessages (SomeMemory m) = importMessages m

-- | No-op BaseMemory instance for unit
instance BaseMemory () where
  messages () = pure []
  addMessage () _ = pure ()
  clear () = pure ()

-- ---------------------------------------------------------------------------
-- ConversationBufferMemory (Unbounded)
-- ---------------------------------------------------------------------------

-- | Unbounded conversation memory keeping the full message history in a thread-safe STM TVar
data ConversationBufferMemory = ConversationBufferMemory
  { memVar :: !(TVar [Message])
  , initialMessagesList :: ![Message]
  }

instance Show ConversationBufferMemory where
  show _ = "ConversationBufferMemory"

instance Eq ConversationBufferMemory where
  (ConversationBufferMemory tv1 _) == (ConversationBufferMemory tv2 _) = tv1 == tv2

-- | Construct a new unbounded ConversationBufferMemory
newConversationBufferMemory :: (MonadIO m) => [Message] -> m ConversationBufferMemory
newConversationBufferMemory initMsgs = liftIO $ do
  tv <- newTVarIO initMsgs
  pure $ ConversationBufferMemory tv initMsgs

instance BaseMemory ConversationBufferMemory where
  messages (ConversationBufferMemory tv _) = liftIO $ readTVarIO tv

  addMessage (ConversationBufferMemory tv _) newMsg = liftIO $ atomically $ do
    modifyTVar' tv (\curr -> curr ++ [newMsg])

  clear (ConversationBufferMemory tv inits) = liftIO $ atomically $ do
    let resetMsgs = if null inits then [systemMessage "You are a helpful AI assistant"] else inits
    writeTVar tv resetMsgs

  importMessages (ConversationBufferMemory tv _) msgs = liftIO $ atomically $ do
    writeTVar tv msgs

-- ---------------------------------------------------------------------------
-- WindowBufferMemory (Sliding Window by Count)
-- ---------------------------------------------------------------------------

-- | Sliding window memory backed by thread-safe STM TVar
data WindowBufferMemory = WindowBufferMemory
  { maxWindowSize :: !Int
  , memVar :: !(TVar [Message])
  , initialMessagesList :: ![Message]
  }

instance Show WindowBufferMemory where
  show (WindowBufferMemory sz _ _) = "WindowBufferMemory { maxWindowSize = " ++ show sz ++ " }"

instance Eq WindowBufferMemory where
  (WindowBufferMemory sz1 tv1 _) == (WindowBufferMemory sz2 tv2 _) =
    sz1 == sz2 && tv1 == tv2

-- | Construct a thread-safe WindowBufferMemory in MonadIO
newWindowBufferMemory ::
  (MonadIO m) =>
  -- | Maximum message window size
  Int ->
  -- | Initial messages to seed the window
  [Message] ->
  -- | Initialized thread-safe WindowBufferMemory
  m WindowBufferMemory
newWindowBufferMemory sz initMsgs = liftIO $ do
  tv <- newTVarIO initMsgs
  pure $ WindowBufferMemory sz tv initMsgs

instance BaseMemory WindowBufferMemory where
  messages (WindowBufferMemory _ tv _) = liftIO $ readTVarIO tv

  addMessage (WindowBufferMemory maxSz tv _) newMsg = liftIO $ do
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

  clear (WindowBufferMemory _ tv _) = liftIO $ do
    atomically $ writeTVar tv [systemMessage "You are a helpful AI assistant"]

  importMessages (WindowBufferMemory _ tv _) msgs = liftIO $ atomically $ do
    writeTVar tv msgs

-- ---------------------------------------------------------------------------
-- TokenBufferMemory (Sliding Window by Token Budget)
-- ---------------------------------------------------------------------------

-- | Strategy for handling incoming messages that exceed token capacity
data OverflowStrategy
  = EvictOldest
  | RejectOnOverflow
  deriving (Show, Eq, Generic)

-- | Function type for counting tokens in a list of messages
type TokenCounter = [Message] -> Int

-- | Approximate token count: 4 characters ≈ 1 token
approximateTokenCount :: [Message] -> Int
approximateTokenCount =
  sum . map (\m -> ceiling (fromIntegral (T.length (extractMessageText m)) / (4.0 :: Double)))

-- | Approximate token count alias for backwards compatibility
countTokens :: [Message] -> Int
countTokens = approximateTokenCount

-- | Token-based sliding window memory type
data TokenBufferMemory = TokenBufferMemory
  { maxTokens :: !Int
  , memVar :: !(TVar [Message])
  , overflowStrategy :: !OverflowStrategy
  , tokenCounter :: !TokenCounter
  , initialMessagesList :: ![Message]
  }

instance Show TokenBufferMemory where
  show (TokenBufferMemory maxT _ strat _ _) =
    "TokenBufferMemory { maxTokens = " ++ show maxT ++ ", overflowStrategy = " ++ show strat ++ " }"

instance Eq TokenBufferMemory where
  (TokenBufferMemory t1 tv1 s1 _ _) == (TokenBufferMemory t2 tv2 s2 _ _) =
    t1 == t2 && tv1 == tv2 && s1 == s2

-- | Construct a new TokenBufferMemory with default eviction strategy and approximate counting
newTokenBufferMemory ::
  (MonadIO m) =>
  -- | Maximum allowed tokens in history
  Int ->
  -- | Initial messages to seed the buffer
  [Message] ->
  -- | Initialized thread-safe TokenBufferMemory
  m TokenBufferMemory
newTokenBufferMemory maxT initMsgs =
  newTokenBufferMemoryWithStrategy maxT initMsgs EvictOldest approximateTokenCount

-- | Construct a TokenBufferMemory with explicit overflow strategy and custom token counter
newTokenBufferMemoryWithStrategy ::
  (MonadIO m) =>
  Int ->
  [Message] ->
  OverflowStrategy ->
  TokenCounter ->
  m TokenBufferMemory
newTokenBufferMemoryWithStrategy maxT initMsgs strat counter = liftIO $ do
  tv <- newTVarIO initMsgs
  pure $ TokenBufferMemory maxT tv strat counter initMsgs

instance BaseMemory TokenBufferMemory where
  messages (TokenBufferMemory _ tv _ _ _) = liftIO $ readTVarIO tv

  addMessage (TokenBufferMemory maxT tv _ counter _) newMsg = do
    let newMsgTokens = counter [newMsg]
    if newMsgTokens > maxT
      then
        throwError $
          memoryError "New message exceeds maximum token limit" (Just "TokenBufferMemory") Nothing
      else liftIO $ atomically $ modifyTVar' tv $ \currMsgs ->
        trimToLimit currMsgs newMsgTokens [newMsg]
    where
      trimToLimit currMsgs newMsgToks acc =
        let candidate = currMsgs ++ acc
         in if counter candidate <= maxT
              then candidate
              else case removeOldestNonSystem currMsgs of
                Just trimmed -> trimToLimit trimmed newMsgToks acc
                Nothing -> [newMsg]

      removeOldestNonSystem [] = Nothing
      removeOldestNonSystem (m : ms)
        | messageRole m == System = fmap (m :) (removeOldestNonSystem ms)
        | otherwise = Just ms

  clear (TokenBufferMemory _ tv _ _ _) = liftIO $ do
    atomically $ writeTVar tv [systemMessage "You are a helpful AI assistant"]

  importMessages (TokenBufferMemory _ tv _ _ _) msgs = liftIO $ atomically $ do
    writeTVar tv msgs

-- ---------------------------------------------------------------------------
-- Persistence & Serialization
-- ---------------------------------------------------------------------------

-- | Export all conversation messages from memory
exportMessages ::
  (BaseMemory mem, MonadIO m, MonadError LangchainError m) =>
  mem ->
  m [Message]
exportMessages = messages

-- | Persist memory state to a JSON file
saveMemoryToFile ::
  (BaseMemory mem, MonadIO m, MonadError LangchainError m) =>
  FilePath ->
  mem ->
  m ()
saveMemoryToFile path mem = do
  msgs <- messages mem
  liftIO $ BSL.writeFile path (encode msgs)

-- | Load memory state from a JSON file
loadMemoryFromFile ::
  (BaseMemory mem, MonadIO m, MonadError LangchainError m) =>
  FilePath ->
  mem ->
  m ()
loadMemoryFromFile path mem = do
  content <- liftIO $ BSL.readFile path
  case decode content of
    Nothing ->
      throwError $
        memoryError
          ("Failed to parse memory JSON from file: " <> T.pack path)
          (Just "loadMemoryFromFile")
          Nothing
    Just msgs -> importMessages mem msgs

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Reset memory back to its exact initial messages
resetToInitial ::
  (MonadIO m) =>
  TVar [Message] ->
  [Message] ->
  m ()
resetToInitial tv inits = liftIO $ atomically $ writeTVar tv inits

-- | Pure helper to trim messages to last N
trimMessages ::
  -- | Number of most recent messages to keep
  Int ->
  -- | Input message list
  [Message] ->
  -- | Trimmed message list
  [Message]
trimMessages n msgs = drop (max 0 (length msgs - n)) msgs

-- | Pure helper to construct initial system message history
initialMessages ::
  -- | System prompt text
  Text ->
  -- | Initial message history with system message
  [Message]
initialMessages sysPrompt = [systemMessage sysPrompt]

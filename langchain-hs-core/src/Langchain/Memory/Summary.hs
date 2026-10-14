{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Memory.Summary
Description : Summary-based conversation memory with progressive LLM summarization
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Progressively summarizes older conversation history using a ChatModel when history exceeds a threshold.
Guarantees transaction safety: conversation history is only pruned after the LLM summarization
call successfully completes, preventing irreversible data loss on network or model failures.
-}
module Langchain.Memory.Summary
  ( SummaryMemory (..)
  , newSummaryMemory
  , newSummaryMemoryWithPrompt
  , getSummary
  , defaultSummaryPrompt
  , formatMessages
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Memory.Core (BaseMemory (..), formatHistory)
import Langchain.Model
  ( ChatModel (..)
  , Message (..)
  , Role (..)
  , extractMessageText
  , systemMessage
  , userMessage
  )

-- | Progressive summarization memory backed by thread-safe STM TVars
data SummaryMemory model = SummaryMemory
  { summaryModel :: model
  , maxMessageThreshold :: !Int
  , summaryBufferVar :: !(TVar Text)
  , recentMessagesVar :: !(TVar [Message])
  , promptBuilder :: !(Text -> [Message] -> Text)
  , initialMessagesList :: ![Message]
  }

-- | Default summarization prompt generator
defaultSummaryPrompt :: Text -> [Message] -> Text
defaultSummaryPrompt currentSummary toSummarize =
  "Current summary:\n"
    <> currentSummary
    <> "\n\nNew lines to summarize:\n"
    <> formatMessages toSummarize
    <> "\n\nPlease provide an updated, concise summary of the conversation above."

-- | Construct a new SummaryMemory instance with default prompt
newSummaryMemory :: MonadIO m => model -> Int -> [Message] -> m (SummaryMemory model)
newSummaryMemory model threshold initMsgs =
  newSummaryMemoryWithPrompt model threshold initMsgs defaultSummaryPrompt

-- | Construct a SummaryMemory instance with custom summarization prompt generator
newSummaryMemoryWithPrompt ::
  MonadIO m =>
  model ->
  Int ->
  [Message] ->
  (Text -> [Message] -> Text) ->
  m (SummaryMemory model)
newSummaryMemoryWithPrompt model threshold initMsgs pBuilder = liftIO $ do
  sVar <- newTVarIO ""
  mVar <- newTVarIO initMsgs
  pure $ SummaryMemory model threshold sVar mVar pBuilder initMsgs

-- | Retrieve the current accumulated summary text
getSummary :: MonadIO m => SummaryMemory model -> m Text
getSummary SummaryMemory {..} = liftIO $ readTVarIO summaryBufferVar

instance (ChatModel model) => BaseMemory (SummaryMemory model) where
  messages SummaryMemory {..} = liftIO $ do
    sumTxt <- readTVarIO summaryBufferVar
    recent <- readTVarIO recentMessagesVar
    if T.null sumTxt
      then pure recent
      else do
        -- Preserve any system instructions at the head, followed by the summary, followed by recent turns
        let (sysMsgs, nonSysMsgs) = partition (\m -> messageRole m == System) recent
            summaryMsg = systemMessage ("Summary of previous conversation:\n" <> sumTxt)
        pure (sysMsgs ++ [summaryMsg] ++ nonSysMsgs)

  addMessage SummaryMemory {..} newMsg = do
    -- 1. Append message to recent buffer
    (shouldSummarize, toSummarize, keepCount) <- liftIO $ atomically $ do
      modifyTVar' recentMessagesVar (\msgs -> msgs ++ [newMsg])
      currentMsgs <- readTVar recentMessagesVar
      if length currentMsgs > maxMessageThreshold
        then do
          let keep = max 2 (maxMessageThreshold `div` 2)
              (old, _) = splitAt (length currentMsgs - keep) currentMsgs
          pure (True, old, keep)
        else pure (False, [], length currentMsgs)

    -- 2. If summarization threshold reached, execute LLM summarization.
    -- Crucial: Pruning ONLY happens after the LLM invoke succeeds!
    when (shouldSummarize && not (null toSummarize)) $ do
      currentSummary <- liftIO $ readTVarIO summaryBufferVar
      let summaryPrompt = promptBuilder currentSummary toSummarize
      aiResp <- invoke summaryModel [userMessage summaryPrompt] Nothing
      let newSummary = extractMessageText aiResp

      -- Atomically commit new summary and prune summarized messages
      liftIO $ atomically $ do
        writeTVar summaryBufferVar newSummary
        modifyTVar' recentMessagesVar (\msgs -> drop (max 0 (length msgs - keepCount)) msgs)

  clear SummaryMemory {..} = liftIO $ atomically $ do
    writeTVar summaryBufferVar ""
    writeTVar recentMessagesVar []

formatMessages :: [Message] -> Text
formatMessages = formatHistory

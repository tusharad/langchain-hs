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
-}
module Langchain.Memory.Summary
  ( SummaryMemory (..)
  , newSummaryMemory
  , getSummary
  ) where

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Model
  ( ChatModel (..)
  , Message (..)
  , Role (..)
  , extractMessageText
  , systemMessage
  , userMessage
  )
import Langchain.Memory.Core (BaseMemory (..))

-- | Progressive summarization memory backed by STM TVars
data SummaryMemory model = SummaryMemory
  { summaryModel :: model
  , maxMessageThreshold :: !Int
  , summaryBufferVar :: !(TVar Text)
  , recentMessagesVar :: !(TVar [Message])
  }

-- | Construct a new SummaryMemory instance
newSummaryMemory :: MonadIO m => model -> Int -> [Message] -> m (SummaryMemory model)
newSummaryMemory model threshold initMsgs = liftIO $ do
  sVar <- newTVarIO ""
  mVar <- newTVarIO initMsgs
  pure $ SummaryMemory model threshold sVar mVar

-- | Retrieve the current accumulated summary text
getSummary :: MonadIO m => SummaryMemory model -> m Text
getSummary SummaryMemory {..} = liftIO $ readTVarIO summaryBufferVar

instance (ChatModel model) => BaseMemory (SummaryMemory model) where
  messages SummaryMemory {..} = liftIO $ do
    sumTxt <- readTVarIO summaryBufferVar
    recent <- readTVarIO recentMessagesVar
    if T.null sumTxt
      then pure recent
      else pure (systemMessage ("Summary of previous conversation:\n" <> sumTxt) : recent)

  addMessage SummaryMemory {..} newMsg = do
    (shouldSummarize, toSummarize, _) <- liftIO $ atomically $ do
      modifyTVar' recentMessagesVar (\msgs -> msgs ++ [newMsg])
      currentMsgs <- readTVar recentMessagesVar
      if length currentMsgs > maxMessageThreshold
        then do
          let (old, keep) = splitAt (length currentMsgs - max 2 (maxMessageThreshold `div` 2)) currentMsgs
          writeTVar recentMessagesVar keep
          pure (True, old, keep)
        else pure (False, [], currentMsgs)

    when (shouldSummarize && not (null toSummarize)) $ do
      currentSummary <- liftIO $ readTVarIO summaryBufferVar
      let summaryPrompt =
            "Current summary:\n"
              <> currentSummary
              <> "\n\nNew lines to summarize:\n"
              <> formatMessages toSummarize
              <> "\n\nPlease provide an updated, concise summary of the conversation above."
      aiResp <- invoke summaryModel [userMessage summaryPrompt] Nothing
      let newSummary = extractMessageText aiResp
      liftIO $ atomically $ writeTVar summaryBufferVar newSummary

  clear SummaryMemory {..} = liftIO $ atomically $ do
    writeTVar summaryBufferVar ""
    writeTVar recentMessagesVar []

formatMessages :: [Message] -> Text
formatMessages msgs =
  T.unlines
    [ formatRole (messageRole m) <> ": " <> extractMessageText m
    | m <- msgs
    ]
  where
    formatRole System = "System"
    formatRole User = "Human"
    formatRole Assistant = "AI"
    formatRole Tool = "Tool"
    formatRole Developer = "Developer"
    formatRole Function = "Function"

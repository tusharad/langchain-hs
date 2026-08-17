{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Server
Description : Real-time Event Streaming & Telemetry Broadcaster
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides multi-subscriber event broadcasting and Server-Sent Events (SSE) / WebSocket
streaming channels for real-time telemetry across multi-agent research pipelines.
-}
module Cortex.Server
  ( CortexEventBroadcaster (..)
  , newCortexEventBroadcaster
  , emitCortexEvent
  , subscribeCortexEvents
  , broadcastDecomposedTask
  , broadcastScrapeProgress
  , broadcastFactCheck
  , broadcastCitation
  ) where

import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, writeTChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import Langchain.Observability.StreamProtocol (AgentStreamEvent (..))

-- | Event broadcaster handle backed by STM broadcast channels
newtype CortexEventBroadcaster = CortexEventBroadcaster
  { broadcastChan :: TChan AgentStreamEvent
  }

-- | Initialize a new thread-safe event broadcaster
newCortexEventBroadcaster :: IO CortexEventBroadcaster
newCortexEventBroadcaster = do
  chan <- newBroadcastTChanIO
  pure $ CortexEventBroadcaster chan

-- | Emit a structured event to all active subscribers
emitCortexEvent :: MonadIO m => CortexEventBroadcaster -> AgentStreamEvent -> m ()
emitCortexEvent CortexEventBroadcaster {..} evt =
  liftIO $ atomically $ writeTChan broadcastChan evt

-- | Create a dedicated subscription channel for a client
subscribeCortexEvents :: MonadIO m => CortexEventBroadcaster -> m (TChan AgentStreamEvent)
subscribeCortexEvents CortexEventBroadcaster {..} =
  liftIO $ atomically $ dupTChan broadcastChan

-- | Convenience helper to broadcast a task decomposition event
broadcastDecomposedTask :: MonadIO m => CortexEventBroadcaster -> Text -> [Text] -> Text -> m ()
broadcastDecomposedTask broadcaster tId subtasks reason =
  emitCortexEvent broadcaster (TaskDecomposedEvent tId subtasks reason)

-- | Convenience helper to broadcast scraping progress
broadcastScrapeProgress :: MonadIO m => CortexEventBroadcaster -> Text -> Text -> Int -> m ()
broadcastScrapeProgress broadcaster url status wCount =
  emitCortexEvent broadcaster (ScrapeProgressEvent url status wCount)

-- | Convenience helper to broadcast a fact-check review
broadcastFactCheck :: MonadIO m => CortexEventBroadcaster -> Text -> Bool -> Double -> Text -> m ()
broadcastFactCheck broadcaster claim verified conf critique =
  emitCortexEvent broadcaster (FactCheckEvent claim verified conf critique)

-- | Convenience helper to broadcast a citation
broadcastCitation :: MonadIO m => CortexEventBroadcaster -> Int -> Text -> Text -> Double -> m ()
broadcastCitation broadcaster idx url title trust =
  emitCortexEvent broadcaster (CitationEvent idx url title trust)

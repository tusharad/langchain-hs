{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.TimeTravel
Description : Graph time-travel debugging, checkpoint history replay, and state rollback
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Enables stepping forward/backward through historic state transitions and resuming execution
from arbitrary historical checkpoints.
-}
module Langchain.Graph.TimeTravel
  ( StateSnapshot (..)
  , TimeTravelHistory (..)
  , newTimeTravelHistory
  , recordSnapshot
  , getSnapshots
  , getSnapshotAtStep
  , resumeFromSnapshot
  ) where

import Control.Concurrent.STM
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock

import Langchain.Core.Error (LangchainError)
import Langchain.Graph.StateGraph

-- | Snapshot of graph state at a specific execution step
data StateSnapshot s = StateSnapshot
  { snapshotStep :: !Int
  , snapshotThreadId :: !Text
  , snapshotNodeId :: !NodeId
  , snapshotState :: !s
  , snapshotTimestamp :: !UTCTime
  }
  deriving (Show, Eq)

-- | In-memory time travel history store backed by STM TVar
newtype TimeTravelHistory s = TimeTravelHistory
  { historyVar :: TVar (Map Text [StateSnapshot s])
  }

-- | Construct a new TimeTravelHistory store
newTimeTravelHistory :: MonadIO m => m (TimeTravelHistory s)
newTimeTravelHistory = liftIO $ do
  var <- newTVarIO Map.empty
  pure $ TimeTravelHistory var

-- | Record a new state snapshot into the history
recordSnapshot ::
  MonadIO m =>
  TimeTravelHistory s ->
  Text ->
  NodeId ->
  s ->
  m (StateSnapshot s)
recordSnapshot TimeTravelHistory {..} threadId nId st = liftIO $ do
  now <- getCurrentTime
  atomically $ do
    histMap <- readTVar historyVar
    let existing = Map.findWithDefault [] threadId histMap
        stepIdx = length existing + 1
        snap = StateSnapshot stepIdx threadId nId st now
    writeTVar historyVar (Map.insert threadId (existing ++ [snap]) histMap)
    pure snap

-- | Retrieve all recorded snapshots for a given thread
getSnapshots :: MonadIO m => TimeTravelHistory s -> Text -> m [StateSnapshot s]
getSnapshots TimeTravelHistory {..} threadId = liftIO $ do
  histMap <- readTVarIO historyVar
  pure $ Map.findWithDefault [] threadId histMap

-- | Retrieve a specific snapshot by step index
getSnapshotAtStep ::
  MonadIO m =>
  TimeTravelHistory s ->
  Text ->
  Int ->
  m (Maybe (StateSnapshot s))
getSnapshotAtStep hist threadId targetStep = do
  snaps <- getSnapshots hist threadId
  pure $ case filter (\s -> snapshotStep s == targetStep) snaps of
    (x : _) -> Just x
    [] -> Nothing

-- | Resume compiled graph execution starting from a historical snapshot
resumeFromSnapshot ::
  (MonadIO m, MonadError LangchainError m) =>
  CompiledGraph s m ->
  StateSnapshot s ->
  m s
resumeFromSnapshot compiledGraph StateSnapshot {..} =
  runGraph compiledGraph snapshotNodeId snapshotState

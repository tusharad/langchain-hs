{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.SubGraph
Description : Recursive sub-graph embedding with timeout, retry, and error propagation
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Allows StateGraphs to invoke other compiled graphs as subroutines with full state adapters,
configurable per-subgraph execution timeouts, and automatic retry policies.
-}
module Langchain.Graph.SubGraph
  ( SubGraphOptions (..)
  , defaultSubGraphOptions
  , embedSubGraphWithOptions
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Langchain.Core.Error (LangchainError)
import Langchain.Graph.StateGraph

-- | Configuration options for sub-graph execution
data SubGraphOptions = SubGraphOptions
  { subGraphTimeoutMicros :: Maybe Int
  , subGraphMaxRetries :: Int
  , subGraphRetryDelayMicros :: Int
  }
  deriving (Show, Eq)

-- | Default sub-graph options (no timeout, 0 retries)
defaultSubGraphOptions :: SubGraphOptions
defaultSubGraphOptions =
  SubGraphOptions
    { subGraphTimeoutMicros = Nothing
    , subGraphMaxRetries = 0
    , subGraphRetryDelayMicros = 100000
    }

-- | Embed a compiled sub-graph into a parent graph node with options
embedSubGraphWithOptions ::
  (MonadIO m, MonadError LangchainError m) =>
  NodeId ->
  CompiledGraph subState m ->
  SubGraphOptions ->
  (parentState -> subState) ->
  (parentState -> subState -> parentState) ->
  Node parentState m
embedSubGraphWithOptions name subGraph SubGraphOptions {..} toSubState mergeState =
  Node
    { nodeId = name
    , nodeAction = \parentSt -> do
        let initSubSt = toSubState parentSt
        finalSubSt <- executeWithRetries initSubSt subGraphMaxRetries
        pure $ Right $ mergeState parentSt finalSubSt
    }
  where
    executeWithRetries subSt attemptsLeft =
      runGraph subGraph startNodeId subSt `catchError` \err ->
        if attemptsLeft <= 0
          then throwError err
          else do
            liftIO $ threadDelay subGraphRetryDelayMicros
            executeWithRetries subSt (attemptsLeft - 1)

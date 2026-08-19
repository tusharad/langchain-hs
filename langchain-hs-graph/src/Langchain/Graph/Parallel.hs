{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.Parallel
Description : Parallel concurrent node execution with state merging
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Executes multiple independent graph nodes concurrently in parallel threads using async,
and merges their resulting sub-states into the parent state via a deterministic reducer.
-}
module Langchain.Graph.Parallel
  ( parallelNode
  , addParallelNodes
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Langchain.Core.Error (LangchainError)
import Langchain.Graph.StateGraph

-- | Construct a parallel composite node that executes worker actions concurrently
parallelNode ::
  MonadIO m =>
  NodeId ->
  [s -> IO (Either LangchainError s)] ->
  (s -> [s] -> s) ->
  Node s m
parallelNode name workerActions mergeFn =
  Node
    { nodeId = name
    , nodeAction = \initState -> liftIO $ do
        results <- mapConcurrently (\action -> action initState) workerActions
        case sequence results of
          Left err -> pure $ Left err
          Right states -> pure $ Right $ mergeFn initState states
    }

-- | Helper to register a parallel execution step into a StateGraph
addParallelNodes ::
  MonadIO m =>
  NodeId ->
  [s -> IO (Either LangchainError s)] ->
  (s -> [s] -> s) ->
  StateGraph s m ->
  StateGraph s m
addParallelNodes name workerActions mergeFn graph =
  let pNode = parallelNode name workerActions mergeFn
   in addNode name (nodeAction pNode) graph

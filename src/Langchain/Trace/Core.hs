{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: we must use hs-telemetry instead of writing it from scratch

{- |
Module      : Langchain.Trace.Core
Description : Agent execution tracing, step timing, and token telemetry
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Captures full chronological telemetry of agent runs: LLM calls, tool executions,
latency per step, and token usage metrics.
-}
module Langchain.Trace.Core
  ( ActionType (..)
  , TraceStep (..)
  , AgentTrace (..)
  , Tracer (..)
  , newTracer
  , recordStep
  , getTrace
  , findSlowestStep
  , filterByActionType
  , clearTracer
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics (Generic)

-- | Type of action recorded in the trace
data ActionType
  = LLMCallAction !Text -- Model name
  | ToolCallAction !Text -- Tool name
  | AgentDecisionAction
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Single discrete step in an agent execution trace
data TraceStep = TraceStep
  { stepIndex :: !Int
  , stepActionType :: !ActionType
  , stepInput :: !Text
  , stepOutput :: !Text
  , stepDurationMicros :: !Int
  , stepTimestamp :: !UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Comprehensive agent execution trace summary
data AgentTrace = AgentTrace
  { traceSessionId :: !Text
  , traceSteps :: ![TraceStep]
  , traceTotalDurationMicros :: !Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | In-memory thread-safe tracer backed by STM TVar
data Tracer = Tracer
  { tracerSessionId :: !Text
  , tracerStepsVar :: !(TVar [TraceStep])
  }

-- | Construct a new Tracer
newTracer :: MonadIO m => Text -> m Tracer
newTracer sessId = liftIO $ do
  var <- newTVarIO []
  pure $ Tracer sessId var

-- | Record a step in the tracer
recordStep ::
  MonadIO m =>
  Tracer ->
  ActionType ->
  Text ->
  Text ->
  Int ->
  m TraceStep
recordStep Tracer {..} actType input output duration = liftIO $ do
  now <- getCurrentTime
  atomically $ do
    steps <- readTVar tracerStepsVar
    let nextIdx = length steps + 1
        newStep = TraceStep nextIdx actType input output duration now
    writeTVar tracerStepsVar (steps ++ [newStep])
    pure newStep

-- | Retrieve the full AgentTrace summary
getTrace :: MonadIO m => Tracer -> m AgentTrace
getTrace Tracer {..} = liftIO $ do
  steps <- readTVarIO tracerStepsVar
  let totalDuration = sum (map stepDurationMicros steps)
  pure $ AgentTrace tracerSessionId steps totalDuration

-- | Find the slowest execution step in the trace
findSlowestStep :: AgentTrace -> Maybe TraceStep
findSlowestStep AgentTrace {..} =
  if null traceSteps
    then Nothing
    else Just $ maximumBy (comparing stepDurationMicros) traceSteps

-- | Filter trace steps by action type
filterByActionType :: (ActionType -> Bool) -> AgentTrace -> [TraceStep]
filterByActionType predicate AgentTrace {..} =
  filter (\s -> predicate (stepActionType s)) traceSteps

-- | Reset the tracer history
clearTracer :: MonadIO m => Tracer -> m ()
clearTracer Tracer {..} = liftIO $ atomically $ writeTVar tracerStepsVar []

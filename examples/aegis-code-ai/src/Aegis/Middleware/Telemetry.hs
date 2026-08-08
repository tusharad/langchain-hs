{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Middleware.Telemetry
Description : Structured telemetry and observability for the AegisCode pipeline
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Structured logging and telemetry collection for the AegisCode AI pipeline.
Emits timestamped events to a 'TChan' for real-time streaming to WebSocket
clients. Tracks step timings, error rates, and agent activity.
-}
module Aegis.Middleware.Telemetry
  ( -- * Telemetry System
    TelemetrySystem (..)
  , newTelemetrySystem

    -- * Emitting Events
  , emitEvent
  , emitInfo
  , emitWarning
  , emitError
  , emitAgentStart
  , emitAgentEnd
  , emitNodeStart
  , emitNodeEnd
  , emitTokenUsage

    -- * Consuming Events
  , subscribeTelemetry
  , readAllEvents
  , getEventCount

    -- * Telemetry Event Types
  , TelemetryEvent (..)
  , TelemetryEventKind (..)
  ) where

import Control.Concurrent.STM
import Data.Aeson (ToJSON (..), FromJSON (..), Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Aegis.Core.Types.Pipeline (PipelinePhase (..), phaseToText, EventSeverity (..))

-- ---------------------------------------------------------------------------
-- Telemetry Event Types
-- ---------------------------------------------------------------------------

-- | Classification of telemetry events
data TelemetryEventKind
  = TelemetryLog EventSeverity
  -- ^ Standard log event
  | TelemetryAgentStart Text
  -- ^ Agent execution started (agent name)
  | TelemetryAgentEnd Text Double
  -- ^ Agent execution ended (agent name, duration seconds)
  | TelemetryNodeStart Text
  -- ^ Graph node execution started (node name)
  | TelemetryNodeEnd Text Double
  -- ^ Graph node execution ended (node name, duration seconds)
  | TelemetryTokenUsage Text Int Int
  -- ^ Token usage event (model, prompt tokens, completion tokens)
  | TelemetryPhaseChange PipelinePhase
  -- ^ Pipeline phase transition
  | TelemetryError Text
  -- ^ Error event
  | TelemetryMetric Text Double
  -- ^ Custom metric (name, value)
  deriving (Eq, Show, Generic)

instance ToJSON TelemetryEventKind
instance FromJSON TelemetryEventKind

-- | A timestamped telemetry event
data TelemetryEvent = TelemetryEvent
  { teTimestamp :: UTCTime
  -- ^ When the event occurred
  , teKind :: TelemetryEventKind
  -- ^ Kind of event
  , tePhase :: PipelinePhase
  -- ^ Current pipeline phase
  , teAgent :: Text
  -- ^ Component that generated the event
  , teMessage :: Text
  -- ^ Human-readable message
  , teMetadata :: Map Text Value
  -- ^ Additional structured metadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON TelemetryEvent
instance FromJSON TelemetryEvent

-- ---------------------------------------------------------------------------
-- Telemetry System
-- ---------------------------------------------------------------------------

-- | Telemetry system using STM for thread-safe event emission and consumption
data TelemetrySystem = TelemetrySystem
  { tsBroadcast :: TChan TelemetryEvent
  -- ^ Broadcast channel for real-time streaming to subscribers
  , tsEventLog :: TVar [TelemetryEvent]
  -- ^ Accumulated event log
  , tsEventCount :: TVar Int
  -- ^ Total event count
  , tsCurrentPhase :: TVar PipelinePhase
  -- ^ Current pipeline phase for auto-tagging
  }

-- | Create a new telemetry system
newTelemetrySystem :: IO TelemetrySystem
newTelemetrySystem = do
  broadcast <- newBroadcastTChanIO
  eventLog <- newTVarIO []
  eventCount <- newTVarIO 0
  phase <- newTVarIO PhaseInitializing
  pure TelemetrySystem
    { tsBroadcast = broadcast
    , tsEventLog = eventLog
    , tsEventCount = eventCount
    , tsCurrentPhase = phase
    }

-- ---------------------------------------------------------------------------
-- Emitting Events
-- ---------------------------------------------------------------------------

-- | Emit a raw telemetry event
emitEvent :: TelemetrySystem -> TelemetryEventKind -> Text -> Text -> Map Text Value -> IO ()
emitEvent ts kind agent message metadata = do
  now <- getCurrentTime
  phase <- readTVarIO (tsCurrentPhase ts)
  let event = TelemetryEvent
        { teTimestamp = now
        , teKind = kind
        , tePhase = phase
        , teAgent = agent
        , teMessage = message
        , teMetadata = metadata
        }
  atomically $ do
    writeTChan (tsBroadcast ts) event
    modifyTVar' (tsEventLog ts) (event :)
    modifyTVar' (tsEventCount ts) (+ 1)
    -- Update phase if this is a phase change event
    case kind of
      TelemetryPhaseChange newPhase -> writeTVar (tsCurrentPhase ts) newPhase
      _ -> pure ()

-- | Emit an informational log event
emitInfo :: TelemetrySystem -> Text -> Text -> IO ()
emitInfo ts agent msg =
  emitEvent ts (TelemetryLog EventInfo) agent msg Map.empty

-- | Emit a warning log event
emitWarning :: TelemetrySystem -> Text -> Text -> IO ()
emitWarning ts agent msg =
  emitEvent ts (TelemetryLog EventWarning) agent msg Map.empty

-- | Emit an error log event
emitError :: TelemetrySystem -> Text -> Text -> IO ()
emitError ts agent msg =
  emitEvent ts (TelemetryError msg) agent msg Map.empty

-- | Emit an agent start event
emitAgentStart :: TelemetrySystem -> Text -> IO ()
emitAgentStart ts agent =
  emitEvent ts (TelemetryAgentStart agent) agent ("Agent started: " <> agent) Map.empty

-- | Emit an agent end event with duration
emitAgentEnd :: TelemetrySystem -> Text -> Double -> IO ()
emitAgentEnd ts agent duration =
  emitEvent ts (TelemetryAgentEnd agent duration) agent
    ("Agent finished: " <> agent <> " (" <> T.pack (show duration) <> "s)")
    (Map.singleton "duration_seconds" (toJSON duration))

-- | Emit a graph node start event
emitNodeStart :: TelemetrySystem -> Text -> IO ()
emitNodeStart ts nodeName =
  emitEvent ts (TelemetryNodeStart nodeName) "Graph" ("Node started: " <> nodeName) Map.empty

-- | Emit a graph node end event with duration
emitNodeEnd :: TelemetrySystem -> Text -> Double -> IO ()
emitNodeEnd ts nodeName duration =
  emitEvent ts (TelemetryNodeEnd nodeName duration) "Graph"
    ("Node finished: " <> nodeName <> " (" <> T.pack (show duration) <> "s)")
    (Map.singleton "duration_seconds" (toJSON duration))

-- | Emit a token usage event
emitTokenUsage :: TelemetrySystem -> Text -> Text -> Int -> Int -> IO ()
emitTokenUsage ts agent model promptTokens completionTokens =
  emitEvent ts (TelemetryTokenUsage model promptTokens completionTokens) agent
    ("Token usage: " <> T.pack (show (promptTokens + completionTokens)) <> " tokens (" <> model <> ")")
    (Map.fromList
      [ ("model", toJSON model)
      , ("prompt_tokens", toJSON promptTokens)
      , ("completion_tokens", toJSON completionTokens)
      , ("total_tokens", toJSON (promptTokens + completionTokens))
      ])

-- ---------------------------------------------------------------------------
-- Consuming Events
-- ---------------------------------------------------------------------------

-- | Create a subscriber channel for real-time event streaming
subscribeTelemetry :: TelemetrySystem -> IO (TChan TelemetryEvent)
subscribeTelemetry ts = atomically $ dupTChan (tsBroadcast ts)

-- | Read all accumulated events (newest first)
readAllEvents :: TelemetrySystem -> IO [TelemetryEvent]
readAllEvents ts = readTVarIO (tsEventLog ts)

-- | Get total event count
getEventCount :: TelemetrySystem -> IO Int
getEventCount ts = readTVarIO (tsEventCount ts)

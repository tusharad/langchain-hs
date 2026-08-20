{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
Module      : Langchain.Observability.StreamProtocol
Description : Unified Multi-Agent Real-time Event Streaming Protocol (SSE & JSON-Lines)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Standardized event schemas for real-time WebSocket and Server-Sent Events (SSE) telemetry
across multi-agent research pipelines, cognitive routers, and dynamic flow execution.
-}
module Langchain.Observability.StreamProtocol
  ( AgentStreamEvent (..)
  , formatSSE
  , formatNdJson
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), encode)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Standardized multi-agent streaming event schema
data AgentStreamEvent
  = TaskDecomposedEvent
      { eventTaskId :: !Text
      , eventSubtasks :: ![Text]
      , eventReasoning :: !Text
      }
  | ScrapeProgressEvent
      { eventUrl :: !Text
      , eventStatus :: !Text
      , eventWordsCount :: !Int
      }
  | FactCheckEvent
      { eventClaim :: !Text
      , eventVerified :: !Bool
      , eventConfidence :: !Double
      , eventCritique :: !Text
      }
  | FlowStepEvent
      { eventNodeId :: !Text
      , eventNodeType :: !Text
      , eventStatus :: !Text
      , eventDurationMs :: !Double
      }
  | CitationEvent
      { eventIndex :: !Int
      , eventUrl :: !Text
      , eventTitle :: !Text
      , eventTrustScore :: !Double
      }
  | ReportChunkEvent
      { eventChunk :: !Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON AgentStreamEvent
instance FromJSON AgentStreamEvent

-- | Format an event as Server-Sent Event (SSE) wire protocol
formatSSE :: AgentStreamEvent -> Text
formatSSE evt =
  let jsonBytes = encode evt
      jsonStr = TE.decodeUtf8 (LBSC.toStrict jsonBytes)
      eventType = getEventName evt
   in "event: " <> eventType <> "\ndata: " <> jsonStr <> "\n\n"
  where
    getEventName (TaskDecomposedEvent {}) = "task_decomposed"
    getEventName (ScrapeProgressEvent {}) = "scrape_progress"
    getEventName (FactCheckEvent {}) = "fact_check"
    getEventName (FlowStepEvent {}) = "flow_step"
    getEventName (CitationEvent {}) = "citation"
    getEventName (ReportChunkEvent {}) = "report_chunk"

-- | Format an event as a newline-delimited JSON line
formatNdJson :: AgentStreamEvent -> Text
formatNdJson evt = TE.decodeUtf8 (LBSC.toStrict (encode evt)) <> "\n"

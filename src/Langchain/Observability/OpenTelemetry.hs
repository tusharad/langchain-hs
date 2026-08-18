{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Observability.OpenTelemetry
Description : OpenTelemetry-compatible tracing, spans, and attributes exporter
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides OpenTelemetry-compatible distributed tracing with hierarchical spans,
rich attribute maps, status codes, and JSON/OTLP exporters.
-}
module Langchain.Observability.OpenTelemetry
  ( SpanKind (..)
  , SpanStatus (..)
  , Span (..)
  , OTelTracer (..)
  , newOTelTracer
  , getSpans
  , startSpan
  , endSpan
  , withSpan
  , exportSpansJson
  ) where

import Control.Concurrent.STM
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Random (randomRIO)

import Langchain.Core.Error (LangchainError)

-- | OpenTelemetry Span Kind
data SpanKind
  = InternalSpan
  | ClientSpan
  | ServerSpan
  | ProducerSpan
  | ConsumerSpan
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | OpenTelemetry Span Status
data SpanStatus
  = StatusUnset
  | StatusOk
  | StatusError !Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Single OpenTelemetry Span
data Span = Span
  { spanName :: !Text
  , spanTraceId :: !Text
  , spanId :: !Text
  , spanParentId :: !(Maybe Text)
  , spanKind :: !SpanKind
  , spanStartTime :: !UTCTime
  , spanEndTime :: !(Maybe UTCTime)
  , spanDurationMicros :: !(Maybe Int)
  , spanAttributes :: !(Map Text Text)
  , spanStatus :: !SpanStatus
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Thread-safe in-memory OpenTelemetry tracer backed by STM TVar
data OTelTracer = OTelTracer
  { tracerTraceId :: !Text
  , tracerSpansVar :: !(TVar [Span])
  }

-- | Construct a new OTelTracer with a given or auto-generated trace ID
newOTelTracer :: MonadIO m => Maybe Text -> m OTelTracer
newOTelTracer mbTraceId = liftIO $ do
  tId <- case mbTraceId of
    Just tid -> pure tid
    Nothing -> do
      randVal <- randomRIO (1000000000000000 :: Integer, 9999999999999999 :: Integer)
      pure $ "trace-" <> T.pack (show randVal)
  var <- newTVarIO []
  pure $ OTelTracer tId var

-- | Retrieve all recorded spans
getSpans :: MonadIO m => OTelTracer -> m [Span]
getSpans OTelTracer {..} = liftIO $ readTVarIO tracerSpansVar

-- | Start a new OpenTelemetry span
startSpan ::
  MonadIO m =>
  OTelTracer ->
  Text ->
  Maybe Text ->
  SpanKind ->
  Map Text Text ->
  m Span
startSpan OTelTracer {..} name parentId kind attrs = liftIO $ do
  now <- getCurrentTime
  randSpan <- randomRIO (10000000 :: Integer, 99999999 :: Integer)
  let sId = "span-" <> T.pack (show randSpan)
      sp =
        Span
          { spanName = name
          , spanTraceId = tracerTraceId
          , spanId = sId
          , spanParentId = parentId
          , spanKind = kind
          , spanStartTime = now
          , spanEndTime = Nothing
          , spanDurationMicros = Nothing
          , spanAttributes = attrs
          , spanStatus = StatusUnset
          }
  atomically $ modifyTVar' tracerSpansVar (\spans -> spans ++ [sp])
  pure sp

-- | Complete an active span with final status
endSpan :: MonadIO m => OTelTracer -> Text -> SpanStatus -> m ()
endSpan OTelTracer {..} targetSpanId status = liftIO $ do
  now <- getCurrentTime
  atomically $ modifyTVar' tracerSpansVar (map (finalizeSpan now))
  where
    finalizeSpan now sp
      | spanId sp == targetSpanId =
          let durMicros = round (diffUTCTime now (spanStartTime sp) * 1000000)
           in sp
                { spanEndTime = Just now
                , spanDurationMicros = Just durMicros
                , spanStatus = status
                }
      | otherwise = sp

-- | Wrap a monadic computation within an OpenTelemetry span
withSpan ::
  (MonadIO m, MonadError LangchainError m) =>
  OTelTracer ->
  Text ->
  Maybe Text ->
  SpanKind ->
  Map Text Text ->
  m a ->
  m a
withSpan tracer name parentId kind attrs action = do
  sp <- startSpan tracer name parentId kind attrs
  res <-
    action `catchError` \err -> do
      endSpan tracer (spanId sp) (StatusError (T.pack (show err)))
      throwError err
  endSpan tracer (spanId sp) StatusOk
  pure res

-- | Export all recorded spans as JSON ByteString
exportSpansJson :: MonadIO m => OTelTracer -> m Text
exportSpansJson tracer = do
  spans <- getSpans tracer
  pure $ T.pack $ LBSC.unpack $ encode spans

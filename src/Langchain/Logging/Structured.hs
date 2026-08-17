{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Logging.Structured
Description : Structured logging system with log levels, metadata context, and pluggable backends
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides typed structured logging with configurable log severity levels, JSON serialization,
and pluggable backends (In-Memory STM, Stderr, File).
-}
module Langchain.Logging.Structured
  ( LogLevel (..)
  , LogEvent (..)
  , Logger (..)
  , InMemoryLogger (..)
  , newInMemoryLogger
  , getInMemoryLogs
  , stderrLogger
  , logEvent
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)

-- | Severity level for log events
data LogLevel
  = DebugLevel
  | InfoLevel
  | WarnLevel
  | ErrorLevel
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Structured log event with metadata
data LogEvent = LogEvent
  { logLevel :: !LogLevel
  , logTimestamp :: !UTCTime
  , logComponent :: !Text
  , logMessage :: !Text
  , logMetadata :: !(Map Text Text)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Pluggable logger backend
data Logger = Logger
  { minLevel :: !LogLevel
  , writeLog :: LogEvent -> IO ()
  }

-- | In-memory logger storing events in STM TVar
data InMemoryLogger = InMemoryLogger
  { inMemoryVar :: !(TVar [LogEvent])
  , inMemoryMinLevel :: !LogLevel
  }

-- | Construct a new InMemoryLogger
newInMemoryLogger :: MonadIO m => LogLevel -> m InMemoryLogger
newInMemoryLogger minLvl = liftIO $ do
  var <- newTVarIO []
  pure $ InMemoryLogger var minLvl

-- | Retrieve all logged events from an InMemoryLogger
getInMemoryLogs :: MonadIO m => InMemoryLogger -> m [LogEvent]
getInMemoryLogs InMemoryLogger {..} = liftIO $ readTVarIO inMemoryVar

-- | Convert InMemoryLogger to standard Logger record
inMemoryToLogger :: InMemoryLogger -> Logger
inMemoryToLogger InMemoryLogger {..} =
  Logger
    { minLevel = inMemoryMinLevel
    , writeLog = \event -> atomically $ modifyTVar' inMemoryVar (\logs -> logs ++ [event])
    }

-- | Default stderr logger
stderrLogger :: LogLevel -> Logger
stderrLogger minLvl =
  Logger
    { minLevel = minLvl
    , writeLog = \event -> do
        let line = LBSC.unpack (encode event)
        hPutStrLn stderr line
    }

-- | Log a structured event through a logger
logEvent :: MonadIO m => Logger -> LogLevel -> Text -> Text -> Map Text Text -> m ()
logEvent Logger {..} lvl comp msg meta =
  if lvl >= minLevel
    then liftIO $ do
      now <- getCurrentTime
      let event = LogEvent lvl now comp msg meta
      writeLog event
    else pure ()

-- | Log a debug message
logDebug :: MonadIO m => Logger -> Text -> Text -> m ()
logDebug logger comp msg = logEvent logger DebugLevel comp msg Map.empty

-- | Log an info message
logInfo :: MonadIO m => Logger -> Text -> Text -> m ()
logInfo logger comp msg = logEvent logger InfoLevel comp msg Map.empty

-- | Log a warning message
logWarn :: MonadIO m => Logger -> Text -> Text -> m ()
logWarn logger comp msg = logEvent logger WarnLevel comp msg Map.empty

-- | Log an error message
logError :: MonadIO m => Logger -> Text -> Text -> m ()
logError logger comp msg = logEvent logger ErrorLevel comp msg Map.empty

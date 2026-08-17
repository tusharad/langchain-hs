{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Langchain.Diagnostics.HealthCheck
Description : Runtime health checks, dependency pinging, and diagnostic reporting
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides runtime diagnostic probes to assess connectivity and readiness of LLM endpoints,
vector stores, local SQLite databases, and caches.
-}
module Langchain.Diagnostics.HealthCheck
  ( HealthStatus (..)
  , ComponentHealth (..)
  , HealthReport (..)
  , checkOllamaHealth
  , checkSqliteHealth
  , runFullHealthCheck
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Network.HTTP.Simple

import Langchain.Core.Monad (LangchainConfig (..))

-- | Discrete health assessment status
data HealthStatus
  = Healthy
  | Degraded
  | Unhealthy
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Health check outcome for an individual component
data ComponentHealth = ComponentHealth
  { componentName :: !Text
  , componentStatus :: !HealthStatus
  , componentLatencyMs :: !Int
  , componentMessage :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Comprehensive diagnostic health report
data HealthReport = HealthReport
  { overallStatus :: !HealthStatus
  , reportTimestamp :: !UTCTime
  , componentChecks :: ![ComponentHealth]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Ping local Ollama instance for readiness
checkOllamaHealth :: MonadIO m => Text -> m ComponentHealth
checkOllamaHealth baseUrl = liftIO $ do
  start <- getCurrentTime
  let req = parseRequest_ (T.unpack baseUrl ++ "/api/tags")
  eResp <- try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString))
  end <- getCurrentTime
  let latMs = round (diffUTCTime end start * 1000)
  case eResp of
    Right resp | getResponseStatusCode resp == 200 ->
      pure $ ComponentHealth "Ollama" Healthy latMs "Ollama API is responsive"
    Right resp ->
      pure $ ComponentHealth "Ollama" Degraded latMs ("Ollama returned status " <> T.pack (show (getResponseStatusCode resp)))
    Left err ->
      pure $ ComponentHealth "Ollama" Unhealthy latMs ("Ollama unreachable: " <> T.pack (show err))

-- | Check SQLite database connection and query readiness
checkSqliteHealth :: MonadIO m => FilePath -> m ComponentHealth
checkSqliteHealth dbPath = liftIO $ do
  start <- getCurrentTime
  eRes <- try $ withConnection dbPath $ \conn -> do
    query_ conn "SELECT 1;" :: IO [Only Int]
  end <- getCurrentTime
  let latMs = round (diffUTCTime end start * 1000)
  case eRes of
    Right [Only 1] ->
      pure $ ComponentHealth ("SQLite (" <> T.pack dbPath <> ")") Healthy latMs "Database responsive"
    Right _ ->
      pure $ ComponentHealth ("SQLite (" <> T.pack dbPath <> ")") Degraded latMs "Unexpected query result"
    Left (err :: SomeException) ->
      pure $ ComponentHealth ("SQLite (" <> T.pack dbPath <> ")") Unhealthy latMs ("SQLite error: " <> T.pack (show err))

-- | Run a complete battery of health checks and produce a consolidated HealthReport
runFullHealthCheck :: MonadIO m => LangchainConfig -> Maybe FilePath -> m HealthReport
runFullHealthCheck _ mbDbPath = liftIO $ do
  now <- getCurrentTime
  ollamaCheck <- checkOllamaHealth "http://localhost:11434"
  sqliteCheck <- case mbDbPath of
    Just path -> checkSqliteHealth path
    Nothing -> pure $ ComponentHealth "SQLite" Healthy 0 "No database configured (skipped)"
  let checks = [ollamaCheck, sqliteCheck]
      overall =
        if any (\c -> componentStatus c == Unhealthy) checks
          then Unhealthy
          else
            if any (\c -> componentStatus c == Degraded) checks
              then Degraded
              else Healthy
  pure $ HealthReport overall now checks

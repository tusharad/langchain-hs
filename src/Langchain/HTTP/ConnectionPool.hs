{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.HTTP.ConnectionPool
Description : HTTP connection pooling and manager lifecycle for remote LLM providers
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides connection reuse, socket pooling, and connection manager configuration for HTTP-based
provider clients (OpenAI, Anthropic, Gemini, DeepSeek, Ollama).
-}
module Langchain.HTTP.ConnectionPool
  ( ConnectionPoolConfig (..)
  , defaultPoolConfig
  , PooledHttpManager (..)
  , newPooledHttpManager
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, managerIdleConnectionCount, managerResponseTimeout, newManager, responseTimeoutMicro)

-- | Configuration settings for HTTP connection pool
data ConnectionPoolConfig = ConnectionPoolConfig
  { maxIdleConnections :: !Int
  , responseTimeoutSec :: !Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default connection pool configuration (32 idle connections, 60s timeout)
defaultPoolConfig :: ConnectionPoolConfig
defaultPoolConfig =
  ConnectionPoolConfig
    { maxIdleConnections = 32
    , responseTimeoutSec = 60
    }

-- | Wrapper around pooled HTTP Client Manager
newtype PooledHttpManager = PooledHttpManager
  { getHttpManager :: Manager
  }

-- | Construct a new thread-safe PooledHttpManager
newPooledHttpManager :: MonadIO m => ConnectionPoolConfig -> m PooledHttpManager
newPooledHttpManager ConnectionPoolConfig {..} = liftIO $ do
  let settings =
        defaultManagerSettings
          { managerIdleConnectionCount = maxIdleConnections
          , managerResponseTimeout = responseTimeoutMicro (responseTimeoutSec * 1000000)
          }
  mgr <- newManager settings
  pure $ PooledHttpManager mgr

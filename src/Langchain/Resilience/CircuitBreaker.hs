{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Resilience.CircuitBreaker
Description : Circuit breaker pattern for LLM provider failover and graceful degradation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implements the Circuit Breaker pattern (Closed, Open, HalfOpen) to prevent cascading failures
when upstream LLM APIs or vector stores experience outages.
-}
module Langchain.Resilience.CircuitBreaker
  ( CircuitState (..)
  , CircuitBreakerConfig (..)
  , defaultCircuitConfig
  , CircuitBreaker (..)
  , newCircuitBreaker
  , getCircuitState
  , withCircuitBreaker
  ) where

import Control.Concurrent.STM
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError, internalError)

-- | State of the circuit breaker
data CircuitState
  = CircuitClosed
  | CircuitOpen !UTCTime -- Timestamp when opened
  | CircuitHalfOpen
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Configuration parameters for the circuit breaker
data CircuitBreakerConfig = CircuitBreakerConfig
  { failureThreshold :: !Int
  , resetTimeoutSec :: !Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Sensible default configuration (5 failures to open, 30 seconds reset timeout)
defaultCircuitConfig :: CircuitBreakerConfig
defaultCircuitConfig =
  CircuitBreakerConfig
    { failureThreshold = 5
    , resetTimeoutSec = 30.0
    }

-- | Circuit breaker handle backed by STM TVar
data CircuitBreaker = CircuitBreaker
  { circuitName :: !Text
  , circuitConfig :: !CircuitBreakerConfig
  , circuitStateVar :: !(TVar (CircuitState, Int)) -- (State, consecutive failures)
  }

-- | Construct a new CircuitBreaker
newCircuitBreaker :: MonadIO m => Text -> CircuitBreakerConfig -> m CircuitBreaker
newCircuitBreaker name cfg = liftIO $ do
  var <- newTVarIO (CircuitClosed, 0)
  pure $ CircuitBreaker name cfg var

-- | Query current state of the circuit breaker
getCircuitState :: MonadIO m => CircuitBreaker -> m CircuitState
getCircuitState CircuitBreaker {..} = liftIO $ do
  (st, _) <- readTVarIO circuitStateVar
  pure st

-- | Execute a protected action through the circuit breaker
withCircuitBreaker ::
  (MonadIO m, MonadError LangchainError m) =>
  CircuitBreaker ->
  m a ->
  m a
withCircuitBreaker CircuitBreaker {..} action = do
  now <- liftIO getCurrentTime
  canProceed <- liftIO $ atomically $ do
    (st, _) <- readTVar circuitStateVar -- TODO: second arguement fails not taken care of
    case st of
      CircuitClosed -> pure True
      CircuitHalfOpen -> pure True
      CircuitOpen openTime ->
        if diffUTCTime now openTime >= realToFrac (resetTimeoutSec circuitConfig)
          then do
            writeTVar circuitStateVar (CircuitHalfOpen, 0)
            pure True
          else pure False

  if not canProceed
    then
      throwError $
        internalError
          ("Circuit breaker '" <> circuitName <> "' is OPEN. Fast-failing request.")
          (Just circuitName)
          Nothing
    else do
      res <-
        action `catchError` \err -> do
          liftIO $ atomically $ do
            (st, fails) <- readTVar circuitStateVar
            let newFails = fails + 1
            if newFails >= failureThreshold circuitConfig
              then writeTVar circuitStateVar (CircuitOpen now, newFails)
              else writeTVar circuitStateVar (st, newFails)
          throwError err

      -- On success, reset circuit to closed and reset failure count
      liftIO $ atomically $ writeTVar circuitStateVar (CircuitClosed, 0)
      pure res

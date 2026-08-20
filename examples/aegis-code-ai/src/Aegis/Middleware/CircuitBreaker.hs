{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Middleware.CircuitBreaker
Description : Circuit breaker pattern for model health management
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implements the circuit breaker pattern for tracking LLM model health.
Tracks consecutive failures per model, opens the circuit after threshold,
and implements half-open probing for recovery detection. Thread-safe via STM.
-}
module Aegis.Middleware.CircuitBreaker
  ( -- * Circuit Breaker
    CircuitBreaker (..)
  , newCircuitBreaker
  , CircuitState (..)

    -- * Operations
  , canCallModel
  , recordSuccess
  , recordFailure
  , getCircuitState
  , resetCircuit
  , getAllCircuitStates
  ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)

import Aegis.Core.Types.Config (CircuitBreakerConfig (..))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Circuit breaker state for a single model
data CircuitState
  = CircuitClosed
  -- ^ Circuit is closed (healthy, requests flow through)
  | CircuitOpen UTCTime
  -- ^ Circuit is open (unhealthy, requests blocked). Contains timestamp of when it opened.
  | CircuitHalfOpen Int
  -- ^ Circuit is half-open (probing). Contains number of successful probes.
  deriving (Eq, Show)

-- | Internal state record for a single model circuit
data ModelCircuit = ModelCircuit
  { mcState :: CircuitState
  , mcConsecutiveFailures :: Int
  , mcConsecutiveSuccesses :: Int
  , mcTotalFailures :: Int
  , mcTotalSuccesses :: Int
  , mcLastFailure :: Maybe UTCTime
  , mcLastSuccess :: Maybe UTCTime
  }
  deriving (Eq, Show)

-- | Initial model circuit state
initialModelCircuit :: ModelCircuit
initialModelCircuit = ModelCircuit
  { mcState = CircuitClosed
  , mcConsecutiveFailures = 0
  , mcConsecutiveSuccesses = 0
  , mcTotalFailures = 0
  , mcTotalSuccesses = 0
  , mcLastFailure = Nothing
  , mcLastSuccess = Nothing
  }

-- ---------------------------------------------------------------------------
-- Circuit Breaker
-- ---------------------------------------------------------------------------

-- | Thread-safe circuit breaker manager using STM
data CircuitBreaker = CircuitBreaker
  { cbConfig :: CircuitBreakerConfig
  -- ^ Circuit breaker configuration
  , cbCircuits :: TVar (Map Text ModelCircuit)
  -- ^ Per-model circuit states
  }

-- | Create a new circuit breaker manager
newCircuitBreaker :: CircuitBreakerConfig -> IO CircuitBreaker
newCircuitBreaker config = do
  circuits <- newTVarIO Map.empty
  pure CircuitBreaker
    { cbConfig = config
    , cbCircuits = circuits
    }

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Check whether a model can be called (circuit allows requests)
canCallModel :: CircuitBreaker -> Text -> IO Bool
canCallModel cb modelName = do
  now <- getCurrentTime
  atomically $ do
    circuits <- readTVar (cbCircuits cb)
    case Map.lookup modelName circuits of
      Nothing -> pure True  -- Unknown model, allow
      Just mc -> case mcState mc of
        CircuitClosed -> pure True
        CircuitOpen openTime ->
          let elapsed = diffUTCTime now openTime
              resetTimeout = fromIntegral (cbResetTimeoutSeconds (cbConfig cb))
          in if elapsed >= resetTimeout
             then do
               -- Transition to half-open
               let mc' = mc { mcState = CircuitHalfOpen 0
                            , mcConsecutiveSuccesses = 0
                            }
               modifyTVar' (cbCircuits cb) (Map.insert modelName mc')
               pure True
             else pure False
        CircuitHalfOpen probes ->
          pure $ probes < cbHalfOpenMaxProbes (cbConfig cb)

-- | Record a successful call to a model
recordSuccess :: CircuitBreaker -> Text -> IO ()
recordSuccess cb modelName = do
  now <- getCurrentTime
  atomically $ do
    circuits <- readTVar (cbCircuits cb)
    let mc = Map.findWithDefault initialModelCircuit modelName circuits
        successes = mcConsecutiveSuccesses mc + 1
        newState = case mcState mc of
          CircuitHalfOpen _ ->
            if successes >= cbSuccessThresholdToClose (cbConfig cb)
            then CircuitClosed
            else CircuitHalfOpen successes
          _ -> CircuitClosed
        mc' = mc
          { mcState = newState
          , mcConsecutiveFailures = 0
          , mcConsecutiveSuccesses = successes
          , mcTotalSuccesses = mcTotalSuccesses mc + 1
          , mcLastSuccess = Just now
          }
    modifyTVar' (cbCircuits cb) (Map.insert modelName mc')

-- | Record a failed call to a model
recordFailure :: CircuitBreaker -> Text -> IO ()
recordFailure cb modelName = do
  now <- getCurrentTime
  atomically $ do
    circuits <- readTVar (cbCircuits cb)
    let mc = Map.findWithDefault initialModelCircuit modelName circuits
        failures = mcConsecutiveFailures mc + 1
        newState = case mcState mc of
          CircuitHalfOpen _ -> CircuitOpen now  -- Half-open probe failed, reopen
          _ ->
            if failures >= cbFailureThreshold (cbConfig cb)
            then CircuitOpen now
            else CircuitClosed
        mc' = mc
          { mcState = newState
          , mcConsecutiveFailures = failures
          , mcConsecutiveSuccesses = 0
          , mcTotalFailures = mcTotalFailures mc + 1
          , mcLastFailure = Just now
          }
    modifyTVar' (cbCircuits cb) (Map.insert modelName mc')

-- | Get the current circuit state for a model
getCircuitState :: CircuitBreaker -> Text -> IO CircuitState
getCircuitState cb modelName = do
  circuits <- readTVarIO (cbCircuits cb)
  pure $ case Map.lookup modelName circuits of
    Nothing -> CircuitClosed
    Just mc -> mcState mc

-- | Reset the circuit for a specific model
resetCircuit :: CircuitBreaker -> Text -> IO ()
resetCircuit cb modelName = atomically $
  modifyTVar' (cbCircuits cb) (Map.insert modelName initialModelCircuit)

-- | Get all circuit states for monitoring
getAllCircuitStates :: CircuitBreaker -> IO (Map Text CircuitState)
getAllCircuitStates cb = do
  circuits <- readTVarIO (cbCircuits cb)
  pure $ Map.map mcState circuits

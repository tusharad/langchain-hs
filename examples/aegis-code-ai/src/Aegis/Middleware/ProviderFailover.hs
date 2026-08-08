{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Aegis.Middleware.ProviderFailover
Description : Multi-model failover engine for Ollama
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Multi-model failover engine that wraps LLM invocations with ordered fallback
across configured Ollama models. Detects failures and retries with next model
in the chain. Integrates with the circuit breaker for health tracking.
-}
module Aegis.Middleware.ProviderFailover
  ( -- * Failover Engine
    FailoverEngine (..)
  , newFailoverEngine
  , invokeWithFailover
  , FailoverResult (..)

    -- * Model Registry
  , ModelEntry (..)
  , ModelStatus (..)
  , getActiveModels
  , markModelFailed
  , markModelSuccess
  , resetModelStatus
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)

import Aegis.Core.Types.Config

-- ---------------------------------------------------------------------------
-- Model Status
-- ---------------------------------------------------------------------------

-- | Status of a model in the failover chain
data ModelStatus
  = ModelActive
  -- ^ Model is healthy and active
  | ModelDegraded Int
  -- ^ Model has had consecutive failures (count)
  | ModelDown UTCTime
  -- ^ Model is down, with timestamp of when it went down
  deriving (Eq, Show)

-- | An entry in the model registry
data ModelEntry = ModelEntry
  { meModelName :: Text
  -- ^ Ollama model name
  , meStatus :: ModelStatus
  -- ^ Current status
  , meConsecutiveFailures :: Int
  -- ^ Consecutive failure count
  , meConsecutiveSuccesses :: Int
  -- ^ Consecutive success count (for recovery)
  , meTotalInvocations :: Int
  -- ^ Total invocation count
  , meTotalFailures :: Int
  -- ^ Total failure count
  , meLastUsed :: Maybe UTCTime
  -- ^ When this model was last used
  }
  deriving (Eq, Show)

-- | Result of a failover invocation
data FailoverResult = FailoverResult
  { frModelUsed :: Text
  -- ^ Which model actually served the request
  , frAttempts :: Int
  -- ^ Number of models attempted
  , frResult :: Either Text Text
  -- ^ The final result (success or final error)
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Failover Engine
-- ---------------------------------------------------------------------------

-- | Failover engine managing model health and routing
data FailoverEngine = FailoverEngine
  { feModels :: TVar [TVar ModelEntry]
  -- ^ Ordered list of model entries
  , feMaxRetries :: Int
  -- ^ Maximum retries per invocation
  , feConfig :: LLMProviderConfig
  -- ^ Provider configuration
  }

-- | Create a new failover engine from configuration
newFailoverEngine :: LLMProviderConfig -> IO FailoverEngine
newFailoverEngine config = do
  let modelNames = ollamaModel (primaryProvider config) : fallbackModels config
  entries <- mapM (newTVarIO . mkEntry) modelNames
  modelsTVar <- newTVarIO entries
  pure FailoverEngine
    { feModels = modelsTVar
    , feMaxRetries = maxRetries config
    , feConfig = config
    }
  where
    mkEntry name = ModelEntry
      { meModelName = name
      , meStatus = ModelActive
      , meConsecutiveFailures = 0
      , meConsecutiveSuccesses = 0
      , meTotalInvocations = 0
      , meTotalFailures = 0
      , meLastUsed = Nothing
      }

-- | Invoke an LLM operation with automatic failover across models
-- The @action@ function receives the model name and should perform the LLM call.
invokeWithFailover
  :: FailoverEngine
  -> (Text -> IO (Either Text Text))  -- ^ Action: model name -> result
  -> IO FailoverResult
invokeWithFailover engine action = do
  activeModels <- getActiveModels engine
  go activeModels 1
  where
    go [] attempts = pure FailoverResult
      { frModelUsed = ""
      , frAttempts = attempts
      , frResult = Left "All models exhausted or unavailable"
      }
    go (modelTVar : rest) attempts = do
      entry <- readTVarIO modelTVar
      let modelName = meModelName entry
      now <- getCurrentTime
      -- Update last used timestamp
      atomically $ modifyTVar' modelTVar $ \e -> e { meLastUsed = Just now }
      -- Attempt the action
      eResult <- try (action modelName) :: IO (Either SomeException (Either Text Text))
      case eResult of
        Left exc -> do
          markModelFailed engine modelTVar
          go rest (attempts + 1)
        Right (Left err) -> do
          markModelFailed engine modelTVar
          if null rest
            then pure FailoverResult
              { frModelUsed = modelName
              , frAttempts = attempts
              , frResult = Left err
              }
            else go rest (attempts + 1)
        Right (Right result) -> do
          markModelSuccess engine modelTVar
          pure FailoverResult
            { frModelUsed = modelName
            , frAttempts = attempts
            , frResult = Right result
            }

-- ---------------------------------------------------------------------------
-- Model Registry Management
-- ---------------------------------------------------------------------------

-- | Get list of active (healthy) model entries
getActiveModels :: FailoverEngine -> IO [TVar ModelEntry]
getActiveModels engine = do
  allModels <- readTVarIO (feModels engine)
  filterActiveModels allModels
  where
    filterActiveModels [] = pure []
    filterActiveModels (m : rest) = do
      entry <- readTVarIO m
      case meStatus entry of
        ModelActive -> do
          remaining <- filterActiveModels rest
          pure (m : remaining)
        ModelDegraded n | n < 5 -> do
          remaining <- filterActiveModels rest
          pure (m : remaining)
        _ -> filterActiveModels rest

-- | Mark a model as having failed
markModelFailed :: FailoverEngine -> TVar ModelEntry -> IO ()
markModelFailed _engine modelTVar = do
  now <- getCurrentTime
  atomically $ modifyTVar' modelTVar $ \e ->
    let failures = meConsecutiveFailures e + 1
        status = if failures >= 5
                 then ModelDown now
                 else ModelDegraded failures
    in e
      { meConsecutiveFailures = failures
      , meConsecutiveSuccesses = 0
      , meTotalFailures = meTotalFailures e + 1
      , meTotalInvocations = meTotalInvocations e + 1
      , meStatus = status
      }

-- | Mark a model as having succeeded
markModelSuccess :: FailoverEngine -> TVar ModelEntry -> IO ()
markModelSuccess _engine modelTVar = do
  atomically $ modifyTVar' modelTVar $ \e ->
    e
      { meConsecutiveFailures = 0
      , meConsecutiveSuccesses = meConsecutiveSuccesses e + 1
      , meTotalInvocations = meTotalInvocations e + 1
      , meStatus = ModelActive
      }

-- | Reset all model statuses to active (e.g., after config change)
resetModelStatus :: FailoverEngine -> IO ()
resetModelStatus engine = do
  allModels <- readTVarIO (feModels engine)
  forM_ allModels $ \modelTVar ->
    atomically $ modifyTVar' modelTVar $ \e ->
      e { meStatus = ModelActive, meConsecutiveFailures = 0, meConsecutiveSuccesses = 0 }

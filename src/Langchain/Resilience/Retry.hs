{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Resilience.Retry
Description : Retry policies with exponential backoff and token-bucket rate limiting
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Resilience combinators for network calls and LLM provider invocations.
-}
module Langchain.Resilience.Retry
  ( RetryPolicy (..)
  , defaultRetryPolicy
  , withRetry
  , RateLimiter (..)
  , newRateLimiter
  , withRateLimit
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock
import System.Random (randomRIO)

import Langchain.Core.Error (LangchainError)

-- | Exponential backoff retry policy
data RetryPolicy = RetryPolicy
  { maxRetries :: !Int
  , baseDelayMicros :: !Int
  , maxDelayMicros :: !Int
  , useJitter :: !Bool
  }
  deriving (Show, Eq)

-- | Default retry policy (3 retries, base 50ms, max 2s, with jitter)
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  RetryPolicy
    { maxRetries = 3
    , baseDelayMicros = 50000
    , maxDelayMicros = 2000000
    , useJitter = True
    }

-- | Execute an action with retry according to RetryPolicy on LangchainError
withRetry
  :: (MonadIO m, MonadError LangchainError m)
  => RetryPolicy
  -> m a
  -> m a
withRetry policy action = go (maxRetries policy) (baseDelayMicros policy)
  where
    go retriesLeft currentDelay =
      action `catchError` \err ->
        if retriesLeft <= 0
          then throwError err
          else do
            delayWithJitter <-
              if useJitter policy
                then liftIO $ randomRIO (currentDelay `div` 2, currentDelay)
                else pure currentDelay
            liftIO $ threadDelay delayWithJitter
            let nextDelay = min (maxDelayMicros policy) (currentDelay * 2)
            go (retriesLeft - 1) nextDelay

-- | Token bucket rate limiter backed by STM TVars
data RateLimiter = RateLimiter
  { bucketCapacity :: !Double
  , refillRatePerSec :: !Double
  , tokensVar :: !(TVar Double)
  , lastRefillVar :: !(TVar UTCTime)
  }

-- | Construct a new Token Bucket RateLimiter (e.g. capacity = 10 tokens, refill = 5 tokens/sec)
newRateLimiter :: MonadIO m => Double -> Double -> m RateLimiter
newRateLimiter cap rate = liftIO $ do
  now <- getCurrentTime
  tVar <- newTVarIO cap
  rVar <- newTVarIO now
  pure $ RateLimiter cap rate tVar rVar

-- | Execute an action subject to token-bucket rate limiting (blocks if bucket empty)
withRateLimit :: (MonadIO m) => RateLimiter -> m a -> m a
withRateLimit RateLimiter {..} action = do
  liftIO $ do
    waitForToken
  action
  where
    waitForToken = do
      now <- getCurrentTime
      waitNeeded <- atomically $ do
        lastTime <- readTVar lastRefillVar
        tokens <- readTVar tokensVar
        let elapsedSecs = realToFrac (diffUTCTime now lastTime) :: Double
            refilledTokens = min bucketCapacity (tokens + elapsedSecs * refillRatePerSec)
        if refilledTokens >= 1.0
          then do
            writeTVar tokensVar (refilledTokens - 1.0)
            writeTVar lastRefillVar now
            pure (0 :: Int)
          else do
            let deficit = 1.0 - refilledTokens
                sleepSecs = deficit / refillRatePerSec
            pure (ceiling (sleepSecs * 1000000) :: Int)
      if waitNeeded > 0
        then do
          threadDelay waitNeeded
          waitForToken
        else pure ()

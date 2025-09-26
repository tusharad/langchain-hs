{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Langchain.Runnable.Utils
Description : Utility wrappers for Runnable components in LangChain
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>

This module provides various utility wrappers for 'Runnable' components that enhance
their behavior with common patterns like:

* Configuration management
* Result caching
* Automatic retries
* Timeout handling

These utilities follow the decorator pattern, wrapping existing 'Runnable' instances
with additional functionality while preserving the original input/output types.

Note: This module is experimental and the API may change in future versions.
-}
module Langchain.Runnable.Utils
  ( -- * Configuration Management
    WithConfig (..)

    -- * Caching
  , Cached (..)
  , cached

    -- * Resilience Patterns
  , Retry (..)
  , WithTimeout (..)
  ) where

import Control.Concurrent
import Data.Map.Strict as Map
import Langchain.Runnable.Core

{- | Wrapper for 'Runnable' components with configurable behavior.

This wrapper allows attaching configuration data to a 'Runnable' instance.
The configuration data can be accessed and modified without changing the
underlying 'Runnable' implementation.

Example:

@
data LLMConfig = LLMConfig
  { temperature :: Float
  , maxTokens :: Int
  }

let
  baseModel = OpenAI defaultOpenAIConfig
  configuredModel = WithConfig
    { configuredRunnable = baseModel
    , runnableConfig = LLMConfig 0.7 100
    }

-- Later, modify the configuration without changing the model
let updatedModel = configuredModel { runnableConfig = LLMConfig 0.9 150 }

-- Use the model as a regular Runnable
result <- invoke updatedModel "Explain monads in Haskell"
@
-}
data WithConfig config r
  = (Runnable r) =>
  WithConfig
  { configuredRunnable :: r
  -- ^ The wrapped 'Runnable' instance
  , runnableConfig :: config
  -- ^ Configuration data for this 'Runnable'
  }

-- | Make WithConfig a Runnable that applies the configuration
instance (Runnable r) => Runnable (WithConfig config r) where
  type RunnableInput (WithConfig config r) = RunnableInput r
  type RunnableOutput (WithConfig config r) = RunnableOutput r

  invoke (WithConfig r1 _) = invoke r1

{- | Cache results of a 'Runnable' to avoid duplicate computations.

This wrapper stores previously computed results in a thread-safe cache.
When an input is encountered again, the cached result is returned instead
of recomputing it, which can significantly improve performance for expensive
operations or when the same inputs are frequently processed.

Note: The cached results are stored in-memory and will be lost when the program
terminates. For persistent caching, consider implementing a custom wrapper that
uses database storage.

The 'RunnableInput' type must be an instance of 'Ord' for map lookups.
-}
data Cached r
  = (Runnable r, Ord (RunnableInput r)) =>
  Cached
  { cachedRunnable :: r
  -- ^ The wrapped 'Runnable' instance
  , cacheMap :: MVar (Map.Map (RunnableInput r) (RunnableOutput r))
  -- ^ Thread-safe cache storage
  }

cached :: (Runnable r, Ord (RunnableInput r)) => r -> IO (Cached r)
cached r = do
  cache <- newMVar Map.empty
  return $ Cached r cache

-- | Make Cached a Runnable that uses a cache
instance (Runnable r, Ord (RunnableInput r)) => Runnable (Cached r) where
  type RunnableInput (Cached r) = RunnableInput r
  type RunnableOutput (Cached r) = RunnableOutput r

  invoke (Cached r cacheRef) input = do
    cache <- readMVar cacheRef
    case Map.lookup input cache of
      Just output -> return $ Right output -- Cache hit: return cached result
      Nothing -> do
        -- Cache miss: compute and store resul
        result <- invoke r input
        case result of
          Left err -> return $ Left err
          Right output -> do
            modifyMVar_ cacheRef $ \c -> return $ Map.insert input output c
            return $ Right output

{- | Add retry capability to any 'Runnable'.

This wrapper automatically retries failed operations up to a specified
number of times with a configurable delay between attempts. This is particularly
useful for network operations or external API calls that might fail transiently.

Example:

@
-- Create an LLM with automatic retry for network failures
let
  baseModel = OpenAI defaultConfig
  resilientModel = Retry
    { retryRunnable = baseModel
    , maxRetries = 3
    , retryDelay = 1000000  -- 1 second delay between retries
    }

-- If the API call fails, it will retry up to 3 times
result <- invoke resilientModel "Generate a story about a Haskell programmer"
@
-}
data Retry r
  = (Runnable r) =>
  Retry
  { retryRunnable :: r
  -- ^ The wrapped 'Runnable' instance
  , maxRetries :: Int
  -- ^ Maximum number of retry attempts
  , retryDelay :: Int
  -- ^ Delay between retry attempts in microseconds
  }

-- | Make Retry a Runnable that retries on failure
instance (Runnable r) => Runnable (Retry r) where
  type RunnableInput (Retry r) = RunnableInput r
  type RunnableOutput (Retry r) = RunnableOutput r

  invoke (Retry r maxRetries_ delay) input = retryWithCount 0
    where
      retryWithCount count = do
        result <- invoke r input
        case result of
          Left err ->
            if count < maxRetries_
              then do
                threadDelay delay
                retryWithCount (count + 1)
              else return $ Left err
          Right output -> return $ Right output

{- | Add timeout capability to any 'Runnable'.

This wrapper enforces a maximum execution time for the wrapped 'Runnable'.
If the operation takes longer than the specified timeout, it is cancelled and
an error is returned. This is useful for limiting the execution time of potentially
long-running operations.

Example:

@
-- Create an LLM with a 30-second timeout
let
  baseModel = OpenAI defaultConfig
  timeboxedModel = WithTimeout
    { timeoutRunnable = baseModel
    , timeoutMicroseconds = 30000000  -- 30 seconds
    }

-- If the API call takes longer than 30 seconds, it will be cancelled
result <- invoke timeboxedModel "Generate a detailed analysis of Haskell's type system"
@

Note: This implementation uses 'forkIO' and 'killThread', which may not always
cleanly terminate the underlying operation, especially for certain types of I/O.
For critical applications, consider implementing a more robust timeout mechanism.
-}
data WithTimeout r
  = (Runnable r) =>
  WithTimeout
  { timeoutRunnable :: r
  -- ^ The wrapped 'Runnable' instance
  , timeoutMicroseconds :: Int
  -- ^ Timeout duration in microseconds
  }

-- | Make WithTimeout a Runnable that times out
instance (Runnable r) => Runnable (WithTimeout r) where
  type RunnableInput (WithTimeout r) = RunnableInput r
  type RunnableOutput (WithTimeout r) = RunnableOutput r

  invoke (WithTimeout r timeout) input = do
    resultVar <- newEmptyMVar

    -- Fork a thread to run the computation
    tid <- forkIO $ do
      result <- invoke r input
      putMVar resultVar (Just result)

    -- Set up the timeout
    timeoutTid <- forkIO $ do
      threadDelay timeout
      putMVar resultVar Nothing

    -- Wait for either result or timeout
    result <- takeMVar resultVar

    -- Kill the other thread
    killThread tid
    killThread timeoutTid

    case result of
      Just r_ -> return r_
      Nothing -> return $ Left "Operation timed out"

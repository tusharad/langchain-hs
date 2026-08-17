{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Tool.Async
Description : Asynchronous tool execution with timeout, cancellation, and concurrency control
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides non-blocking, async tool execution primitives with per-tool timeout limits
and batch concurrent execution.
-}
module Langchain.Tool.Async
  ( executeToolAsync
  , executeToolWithTimeout
  , executeToolBatchConcurrently
  ) where

import Control.Concurrent.Async (Async, async, cancel, mapConcurrently, race)
import Control.Concurrent (threadDelay)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, toolError)
import Langchain.Tool.Core (Tool (..))

-- | Spawn tool execution in an asynchronous background thread
executeToolAsync
  :: (MonadIO m)
  => Tool IO
  -> Value
  -> m (Async (Either LangchainError Text))
executeToolAsync Tool {..} args = liftIO $ do
  async (toolExecute args)

-- | Execute a tool with a strict timeout limit in microseconds
executeToolWithTimeout
  :: (MonadIO m, MonadError LangchainError m)
  => Tool IO
  -> Value
  -> Int
  -> m Text
executeToolWithTimeout Tool {..} args timeoutMicros = do
  res <- liftIO $ race (threadDelay timeoutMicros) (toolExecute args)
  case res of
    Left () ->
      throwError $
        toolError
          ("Tool '" <> toolName <> "' timed out after " <> T.pack (show timeoutMicros) <> " microseconds")
          (Just toolName)
          Nothing
    Right (Left err) -> throwError err
    Right (Right output) -> pure output

-- | Execute a batch of tool calls concurrently in parallel
executeToolBatchConcurrently
  :: (MonadIO m, MonadError LangchainError m)
  => [(Tool IO, Value)]
  -> m [Text]
executeToolBatchConcurrently toolCalls = do
  results <- liftIO $ mapConcurrently (\(t, args) -> toolExecute t args) toolCalls
  case sequence results of
    Left err -> throwError err
    Right outputs -> pure outputs

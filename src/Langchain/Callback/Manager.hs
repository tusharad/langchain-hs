{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Callback.Manager
Description : Typed event-driven callback system with synchronous and asynchronous dispatch
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides typed callback lifecycle events across models, tools, chains, and state graphs,
with support for filtering and non-blocking asynchronous event dispatch.
-}
module Langchain.Callback.Manager
  ( CallbackEvent (..)
  , CallbackHandler (..)
  , CallbackManager (..)
  , newCallbackManager
  , registerHandler
  , dispatchEvent
  , dispatchEventAsync
  , newLoggingCallbackHandler
  ) where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

-- | Comprehensive lifecycle events emitted across Langchain components
data CallbackEvent
  = OnLLMStart !Text ![Text] !UTCTime -- Model name, Prompt inputs, Timestamp
  | OnLLMEnd !Text !Text !Int !UTCTime -- Model name, Output text, Latency micros, Timestamp
  | OnToolStart !Text !Value !UTCTime -- Tool name, Arguments, Timestamp
  | OnToolEnd !Text !Text !Int !UTCTime -- Tool name, Output text, Latency micros, Timestamp
  | OnChainStart !Text !Text !UTCTime -- Chain name, Input, Timestamp
  | OnChainEnd !Text !Text !Int !UTCTime -- Chain name, Output, Latency micros, Timestamp
  | OnGraphNodeStart !Text !Text !UTCTime -- NodeId, State summary, Timestamp
  | OnGraphNodeEnd !Text !Text !Int !UTCTime -- NodeId, Next node/state summary, Latency micros, Timestamp
  | OnError !Text !Text !UTCTime -- Component name, Error message, Timestamp
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Handler for processing emitted callback events
data CallbackHandler = CallbackHandler
  { handlerName :: !Text
  , handleEvent :: CallbackEvent -> IO ()
  }

-- | Thread-safe CallbackManager backed by STM TVar
newtype CallbackManager = CallbackManager
  { handlersVar :: TVar [CallbackHandler]
  }

-- | Construct an empty CallbackManager
newCallbackManager :: MonadIO m => m CallbackManager
newCallbackManager = liftIO $ do
  var <- newTVarIO []
  pure $ CallbackManager var

-- | Register a new callback handler
registerHandler :: MonadIO m => CallbackManager -> CallbackHandler -> m ()
registerHandler CallbackManager {..} handler = liftIO $ do
  atomically $ modifyTVar' handlersVar (\handlers -> handlers ++ [handler])

-- | Dispatch an event synchronously to all registered handlers
dispatchEvent :: MonadIO m => CallbackManager -> CallbackEvent -> m ()
dispatchEvent CallbackManager {..} event = liftIO $ do
  handlers <- readTVarIO handlersVar
  mapM_ (`handleEvent` event) handlers

-- | Dispatch an event asynchronously in background threads without blocking
dispatchEventAsync :: MonadIO m => CallbackManager -> CallbackEvent -> m ()
dispatchEventAsync CallbackManager {..} event = liftIO $ do
  handlers <- readTVarIO handlersVar
  mapM_ (\h -> async (handleEvent h event)) handlers

-- | Create a simple callback handler that logs event descriptions into an STM TVar
newLoggingCallbackHandler :: MonadIO m => Text -> m (CallbackHandler, TVar [Text])
newLoggingCallbackHandler name = liftIO $ do
  logsVar <- newTVarIO []
  let handler =
        CallbackHandler
          { handlerName = name
          , handleEvent = \event -> atomically $ modifyTVar' logsVar (\logs -> logs ++ [T.pack (show event)])
          }
  pure (handler, logsVar)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Agent.Middleware
Description : Effect-polymorphic Agent Middleware system
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides effect-polymorphic middleware hooks ('AgentMiddleware m') around agent execution steps.
-}
module Langchain.Agent.Middleware
  ( AgentMiddleware (..)
  , defaultMiddleware
  , chainMiddlewares
  , loggingMiddleware
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)

import Langchain.Agent.ReAct (AgentStep (..))
import Langchain.Core.Model (Message, ToolCall (..))

-- | Effect-polymorphic middleware hooks for agent execution
data AgentMiddleware m = AgentMiddleware
  { beforeStep :: [Message] -> m [Message]
  , afterStep :: AgentStep -> m AgentStep
  , beforeToolCall :: ToolCall -> m ToolCall
  , afterToolCall :: ToolCall -> Text -> m Text
  }

-- | Default no-op middleware
defaultMiddleware :: Monad m => AgentMiddleware m
defaultMiddleware =
  AgentMiddleware
    { beforeStep = pure
    , afterStep = pure
    , beforeToolCall = pure
    , afterToolCall = \_ out -> pure out
    }

-- | Chain multiple middlewares into a single composite middleware
chainMiddlewares :: Monad m => [AgentMiddleware m] -> AgentMiddleware m
chainMiddlewares mws =
  AgentMiddleware
    { beforeStep = \msgs -> foldl (\acc mw -> acc >>= beforeStep mw) (pure msgs) mws
    , afterStep = \step -> foldl (\acc mw -> acc >>= afterStep mw) (pure step) mws
    , beforeToolCall = \tc -> foldl (\acc mw -> acc >>= beforeToolCall mw) (pure tc) mws
    , afterToolCall = \tc out -> foldl (\acc mw -> acc >>= \o -> afterToolCall mw tc o) (pure out) mws
    }

-- | Logging middleware printing step events to stderr
loggingMiddleware :: MonadIO m => AgentMiddleware m
loggingMiddleware =
  AgentMiddleware
    { beforeStep = \msgs -> do
        liftIO $ hPutStrLn stderr $ "[AgentMiddleware] Executing step with " ++ show (length msgs) ++ " messages"
        pure msgs
    , afterStep = \step -> do
        liftIO $ hPutStrLn stderr $ "[AgentMiddleware] Step result: " ++ show step
        pure step
    , beforeToolCall = \tc -> do
        liftIO $ hPutStrLn stderr $ "[AgentMiddleware] Calling tool: " ++ T.unpack (toolCallName tc)
        pure tc
    , afterToolCall = \tc out -> do
        liftIO $ hPutStrLn stderr $ "[AgentMiddleware] Tool " ++ T.unpack (toolCallName tc) ++ " finished with output length " ++ show (T.length out)
        pure out
    }

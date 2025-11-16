{-# LANGUAGE RankNTypes #-}

{- |
Module      : Langchain.Agent.Middleware
Description : Built-in middlewares for LangChain agents
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides a comprehensive set of built-in middlewares for agents,
similar to Python LangChain's middleware system. Middlewares allow you to hook
into various points in the agent execution lifecycle.

Available middlewares:
- defaultMiddleware: No-op middleware (base implementation)
- humanInLoopMiddleware: Pause for human approval before tool execution
- toolCallLimitMiddleware: Limit the number of tool calls
-}
module Langchain.Agent.Middleware
  ( -- * Middleware Type
    AgentMiddleware (..)
  , applyMiddlewares

    -- * Built-in Middlewares
  , defaultMiddleware
  , humanInLoopMiddleware
  , toolCallLimitMiddleware
  ) where

import Control.Monad (foldM)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Langchain.Agent.Core
import Langchain.Error
  ( LangchainResult
  , agentError
  , fromString
  )
import Langchain.LLM.Core (Message (messageData), MessageData (toolCalls))
import Langchain.Memory.Core (BaseMemory (messages))

-- | Middleware hooks around agent execution steps.
data Agent a => AgentMiddleware a = AgentMiddleware
  { beforeModelCall :: (AgentState, a) -> IO (LangchainResult (AgentState, a))
  , afterModelCall :: (AgentState, a) -> IO (LangchainResult (AgentState, a))
  , beforeToolCall :: (AgentState, a) -> IO (LangchainResult (AgentState, a))
  , afterToolCall :: (AgentState, a) -> IO (LangchainResult (AgentState, a))
  , beforeAgent :: (AgentState, a) -> IO (LangchainResult (AgentState, a))
  , afterAgent :: (AgentState, a) -> IO (LangchainResult (AgentState, a))
  }

-- | Default middleware that does nothing (no-op).
defaultMiddleware :: Agent a => AgentMiddleware a
defaultMiddleware =
  AgentMiddleware
    { beforeModelCall = pure . Right
    , afterModelCall = pure . Right
    , beforeToolCall = pure . Right
    , afterToolCall = pure . Right
    , beforeAgent = pure . Right
    , afterAgent = pure . Right
    }

-- | Sequentially apply a list of middlewares for a given phase.
applyMiddlewares ::
  (AgentMiddleware a -> (AgentState, a) -> IO (LangchainResult (AgentState, a))) ->
  [AgentMiddleware a] ->
  (AgentState, a) ->
  IO (LangchainResult (AgentState, a))
applyMiddlewares f mws st =
  foldM
    ( \acc mw -> case acc of
        Left err -> pure $ Left err
        Right s -> f mw s
    )
    (Right st)
    mws

{- | Human-in-the-loop middleware.
Pauses execution before each tool call and asks for human approval.
This is useful for sensitive operations or debugging.

Example:
> runAgentExecutor agent config callbacks [humanInLoopMiddleware] "input"
-}
humanInLoopMiddleware :: Agent a => AgentMiddleware a
humanInLoopMiddleware =
  defaultMiddleware
    { beforeToolCall = \(st, a) -> do
        case agentMemory st of
          SomeMemory mem -> do
            eRes <- messages mem
            case eRes of
              Left err -> pure $ Left err
              Right msgs -> do
                let msg = NE.last msgs
                    toolCallLst = toolCalls $ messageData msg
                putStrLn $ "Approve this tool call? " ++ show toolCallLst
                putStrLn "(y/n): "
                resp <- getLine
                if resp == "y"
                  then pure $ Right (st, a)
                  else pure $ Left $ fromString "Tool call rejected by human"
    }

{- | Tool call limit middleware.
Limits the total number of tool calls during agent execution.
This helps prevent excessive tool usage and control costs.

Example:
> toolCallLimitMiddleware 20  -- Limit to 20 tool calls
-}
toolCallLimitMiddleware :: Agent a => Int -> IO (AgentMiddleware a)
toolCallLimitMiddleware maxCalls = do
  counter <- newIORef 0
  pure $
    defaultMiddleware
      { beforeToolCall = \(st, a) -> do
          count <- readIORef counter
          if count >= maxCalls
            then
              pure $
                Left $
                  agentError
                    (T.pack $ "Tool call limit exceeded: " <> show maxCalls)
                    Nothing
                    (Just (T.pack "toolCallLimitMiddleware"))
            else do
              modifyIORef' counter (+ 1)
              pure $ Right (st, a)
      }

{-# LANGUAGE RankNTypes #-}

module Langchain.Agent.Middleware
  ( AgentMiddleware (..)
  , applyMiddlewares
  , defaultMiddleware
  , humanInLoopMiddleware
  ) where

import Control.Exception (throw)
import Control.Monad (foldM)
import qualified Data.List.NonEmpty as NE
import Langchain.Agent.Core
import Langchain.Error (fromString)
import Langchain.LLM.Core (Message (messageData), MessageData (toolCalls))
import Langchain.Memory.Core (BaseMemory (messages))

-- | Middleware hooks around agent execution steps.
data Agent a => AgentMiddleware a = AgentMiddleware
  { beforeModelCall :: (AgentState, a) -> IO (AgentState, a)
  , afterModelCall :: (AgentState, a) -> IO (AgentState, a)
  , beforeToolCall :: (AgentState, a) -> IO (AgentState, a)
  , afterToolCall :: (AgentState, a) -> IO (AgentState, a)
  , beforeAgent :: (AgentState, a) -> IO (AgentState, a)
  , afterAgent :: (AgentState, a) -> IO (AgentState, a)
  }

-- | Default middleware that does nothing (no-op).
defaultMiddleware :: Agent a => AgentMiddleware a
defaultMiddleware =
  AgentMiddleware
    { beforeModelCall = pure
    , afterModelCall = pure
    , beforeToolCall = pure
    , afterToolCall = pure
    , beforeAgent = pure
    , afterAgent = pure
    }

-- | Sequentially apply a list of middlewares for a given phase.
applyMiddlewares ::
  (AgentMiddleware a -> (AgentState, a) -> IO (AgentState, a)) ->
  [AgentMiddleware a] ->
  (AgentState, a) ->
  IO (AgentState, a)
applyMiddlewares f mws st = foldM (flip f) st mws

humanInLoopMiddleware :: Agent a => AgentMiddleware a
humanInLoopMiddleware =
  defaultMiddleware
    { beforeToolCall = \(st, a) -> do
        case agentMemory st of
          SomeMemory mem -> do
            eRes <- messages mem
            case eRes of
              Left _ -> fail "message fetching failed"
              Right msgs -> do
                let msg = NE.last msgs
                    toolCallLst = toolCalls $ messageData msg
                putStrLn $ "Approve this tool call? " ++ show toolCallLst
                putStrLn "(y/n): "
                resp <- getLine
                if resp == "y"
                  then pure (st, a)
                  else throw $ fromString "Tool call rejected by human"
    }

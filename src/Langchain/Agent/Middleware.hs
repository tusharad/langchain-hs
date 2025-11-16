{-# LANGUAGE RankNTypes #-}

module Langchain.Agent.Middleware
  ( AgentMiddleware (..)
  , applyMiddlewares
  , defaultMiddleware
  , humanInLoopMiddleware
  ) where

import Control.Monad (foldM)
import qualified Data.List.NonEmpty as NE
import Langchain.Agent.Core
import Langchain.Error (LangchainResult, fromString)
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
                  then pure $ Right (st, a)
                  else pure $ Left $ fromString "Tool call rejected by human"
    }

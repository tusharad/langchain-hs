{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Agent.ReAct
Description : Effect-polymorphic ReAct (Reasoning + Acting) Agent engine
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Modernized ReAct agent operating over ChatModel, Tool m, and multi-modal Message history.
-}
module Langchain.Agent.ReAct
  ( AgentStep (..)
  , ReActAgent (..)
  , createReActAgent
  , reactStep
  , runReActAgent
  ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, agentError, toolError)
import Langchain.Core.Model
import qualified Langchain.Core.Model.Types as M
import Langchain.Core.Tool

-- | Step result of ReAct reasoning iteration
data AgentStep
  = AgentAction ToolCall
  | AgentFinish Message
  deriving (Eq, Show)

-- | Effect-polymorphic ReAct Agent configuration
data ReActAgent model m = ReActAgent
  { agentModel :: model
  , agentTools :: [Tool m]
  , agentMaxIterations :: Int
  }

-- | Construct a ReAct Agent instance
createReActAgent :: model -> [Tool m] -> ReActAgent model m
createReActAgent model tools =
  ReActAgent
    { agentModel = model
    , agentTools = tools
    , agentMaxIterations = 10
    }

-- | Run a single step of ReAct reasoning using ChatModel
reactStep ::
  (ChatModel model, MonadIO m, MonadError LangchainError m) =>
  model ->
  [Tool m] ->
  [Message] ->
  m AgentStep
reactStep model tools history = do
  responseMsg <- invoke model history Nothing
  case messageToolCalls responseMsg of
    Just (tc : _) -> pure $ AgentAction tc
    _ -> pure $ AgentFinish responseMsg

-- | Execute the full ReAct reasoning loop until AgentFinish or max iterations reached
runReActAgent ::
  (ChatModel model, MonadIO m, MonadError LangchainError m) =>
  ReActAgent model m ->
  [Message] ->
  m Message
runReActAgent agent initialHistory = go initialHistory (agentMaxIterations agent)
  where
    go history maxIter
      | maxIter <= 0 = throwError $ agentError "ReAct Agent exceeded maximum iterations" Nothing Nothing
      | otherwise = do
          step <- reactStep (agentModel agent) (agentTools agent) history
          case step of
            AgentFinish finalMsg -> pure finalMsg
            AgentAction tc -> do
              let tName = toolCallName tc
              case find (\t -> toolName t == tName) (agentTools agent) of
                Nothing -> throwError $ toolError ("Tool not found: " <> tName) (Just tName) Nothing
                Just tool -> do
                  eOut <- toolExecute tool (toolCallArguments tc)
                  case eOut of
                    Left err -> throwError err
                    Right outTxt -> do
                      let obsMsg = textMessage M.Tool outTxt
                          newHistory = history ++ [assistantMessage "", obsMsg]
                      go newHistory (maxIter - 1)

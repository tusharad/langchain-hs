{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Langchain.Agent.ReAct
Description : Effect-polymorphic ReAct (Reasoning + Acting) Agent engine
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Modernized ReAct agent operating over ChatModel, Tool m, and multi-modal Message history.
Uses 'ToolBinder' to pass tool definitions to the LLM provider in a provider-agnostic way.
-}
module Langchain.Agent.ReAct
  ( AgentStep (..)
  , ReActAgent (..)
  , createReActAgent
  , reactStep
  , runReActAgent
  ) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.List (find)

import Langchain.Core.Error (LangchainError, agentError, errorMessage)
import Langchain.Core.Model
import qualified Langchain.Core.Model.Types as M
import Langchain.Core.Tool
import Langchain.Tool.Binding (ToolBinder (..))

-- | Step result of ReAct reasoning iteration
data AgentStep
  = AgentAction Message [ToolCall]
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
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  model ->
  [Tool m] ->
  [Message] ->
  m AgentStep
reactStep model tools history = do
  let cfg = bindToolsConfig @model tools Nothing
  responseMsg <- invoke model history cfg
  case messageToolCalls responseMsg of
    Just tcs@(_ : _) -> pure $ AgentAction responseMsg tcs
    _ -> pure $ AgentFinish responseMsg

-- | Execute the full ReAct reasoning loop until AgentFinish or max iterations reached
runReActAgent ::
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
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
            AgentAction respMsg tcs -> do
              obsMsgs <- forM tcs $ \tc -> do
                let tName = toolCallName tc
                outTxt <- case find (\t -> toolName t == tName) (agentTools agent) of
                  Nothing ->
                    pure $ "Error: Tool not found: " <> tName
                  Just tool -> do
                    eOut <- toolExecute tool (toolCallArguments tc)
                    case eOut of
                      Left err ->
                        pure $ "Error executing tool " <> tName <> ": " <> errorMessage err
                      Right out ->
                        pure out
                pure $
                  (textMessage M.Tool outTxt)
                    { M.messageName = Just tName
                    , M.messageToolId = Just (toolCallId tc)
                    }
              let newHistory = history ++ [respMsg] ++ obsMsgs
              go newHistory (maxIter - 1)

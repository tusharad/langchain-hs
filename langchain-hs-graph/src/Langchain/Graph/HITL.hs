{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Graph.HITL
Description : Human-in-the-Loop (HITL) interrupt and state resume mechanisms
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides interrupt signal types and resume helpers for human review workflows.
-}
module Langchain.Graph.HITL
  ( hitlInterruptError
  , isHITLInterrupt
  , hitlNode
  , resumeGraph
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError (..), agentError, errorMessage)
import Langchain.Graph.Checkpointer (Checkpointer (..))
import Langchain.Graph.StateGraph (StateGraph, Node (..), NodeId, runGraph)

-- | Construct a special HITL Interrupt LangchainError
hitlInterruptError :: NodeId -> LangchainError
hitlInterruptError targetNodeId =
  agentError ("HITL_INTERRUPT:" <> targetNodeId) (Just "HITL") (Just targetNodeId)

-- | Check whether an error is a HITL Interrupt signal and return the NodeId
isHITLInterrupt :: LangchainError -> Maybe NodeId
isHITLInterrupt err =
  let msg = errorMessage err
   in if "HITL_INTERRUPT:" `T.isPrefixOf` msg
        then Just (T.drop (T.length ("HITL_INTERRUPT:" :: Text)) msg)
        else Nothing

-- | Create a Human-in-the-Loop Node that saves a checkpoint and interrupts execution for human review
hitlNode ::
  (Checkpointer cp m, ToJSON s, MonadIO m) =>
  cp ->
  Text ->
  NodeId ->
  (s -> m (Either LangchainError s)) ->
  Node s m
hitlNode cp threadId name _ =
  Node
    { nodeId = name
    , nodeAction = \state -> do
        _ <- saveCheckpoint cp threadId name state
        pure $ Left $ hitlInterruptError name
    }

-- | Resume an interrupted graph execution after human modification of checkpoint state
resumeGraph ::
  (Checkpointer cp m, FromJSON s, ToJSON s, MonadIO m, MonadError LangchainError m) =>
  StateGraph s m ->
  cp ->
  Text ->
  NodeId ->
  NodeId ->
  (s -> s) ->
  m s
resumeGraph stateGraph cp threadId checkpointNodeId resumeStartNodeId modifier = do
  mbState <- loadCheckpoint cp threadId checkpointNodeId
  case mbState of
    Left err -> throwError err
    Right Nothing ->
      throwError $
        agentError
          ("No checkpoint found to resume at node: " <> checkpointNodeId)
          (Just "resumeGraph")
          Nothing
    Right (Just savedState) -> do
      let modifiedState = modifier savedState
      _ <- saveCheckpoint cp threadId resumeStartNodeId modifiedState
      runGraph stateGraph resumeStartNodeId modifiedState

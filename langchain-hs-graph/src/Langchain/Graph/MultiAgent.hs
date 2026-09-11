{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Graph.MultiAgent
Description : Multi-Agent supervisor routing and sub-graph embedding nodes
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides multi-agent routing supervisor nodes and sub-graph composition primitives.
-}
module Langchain.Graph.MultiAgent
  ( supervisorNode
  , embedSubGraphNode
  , embedSubGraphNodeWithStart
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model (ChatModel (..), extractMessageText, userMessage)
import Langchain.Graph.StateGraph (Node (..), NodeId, StateGraph, runGraph, startNodeId)

-- | Construct a supervisor routing node that uses an LLM to select the target sub-agent NodeId
supervisorNode ::
  (ChatModel model, MonadIO m, MonadError LangchainError m) =>
  model ->
  NodeId ->
  [(Text, NodeId)] ->
  (s -> Text) ->
  (Text -> s -> s) ->
  Node s m
supervisorNode model name routes extractPrompt updateState =
  Node
    { nodeId = name
    , nodeAction = \state -> do
        let prompt =
              "You are a supervisor delegating tasks to sub-agents. Available routes:\n"
                <> T.unlines [rName <> " -> " <> targetId | (rName, targetId) <- routes]
                <> "\nTask context: "
                <> extractPrompt state
                <> "\nReply with ONLY the route name to execute."
        msg <- invoke model [userMessage prompt] Nothing
        let rawRoute = T.strip (extractMessageText msg)
            cleanRoute = T.filter (\c -> c /= '.' && c /= '"' && c /= '\'' && c /= '`') (T.toLower rawRoute)
            matchExact =
              lookup rawRoute routes
                <|> lookup (T.toLower rawRoute) [(T.toLower r, target) | (r, target) <- routes]
                <|> lookup
                  cleanRoute
                  [(T.toLower (T.filter (\c -> c /= '.' && c /= '"' && c /= '\'') r), target) | (r, target) <- routes]
            matchSubstring =
              case filter (\(r, _) -> T.toLower r `T.isInfixOf` cleanRoute) routes of
                ((_, target) : _) -> Just target
                [] -> Nothing
        case matchExact <|> matchSubstring of
          Just targetId -> pure $ Right $ updateState targetId state
          Nothing -> case routes of
            ((_, fallbackId) : _) -> pure $ Right $ updateState fallbackId state
            [] ->
              throwError $ agentError "No routes configured in supervisor node" (Just "supervisorNode") Nothing
    }

-- | Embed a compiled sub-graph into a parent graph node starting at a specific entry node
embedSubGraphNodeWithStart ::
  (MonadIO m, MonadError LangchainError m) =>
  NodeId ->
  NodeId ->
  StateGraph subState m ->
  (parentState -> subState) ->
  (parentState -> subState -> parentState) ->
  Node parentState m
embedSubGraphNodeWithStart name entryId subGraph toSubState mergeState =
  Node
    { nodeId = name
    , nodeAction = \parentSt -> do
        let initSubSt = toSubState parentSt
        finalSubSt <- runGraph subGraph entryId initSubSt
        pure $ Right $ mergeState parentSt finalSubSt
    }

-- | Embed a compiled sub-graph into a parent graph node starting at 'startNodeId'
embedSubGraphNode ::
  (MonadIO m, MonadError LangchainError m) =>
  NodeId ->
  StateGraph subState m ->
  (parentState -> subState) ->
  (parentState -> subState -> parentState) ->
  Node parentState m
embedSubGraphNode name = embedSubGraphNodeWithStart name startNodeId

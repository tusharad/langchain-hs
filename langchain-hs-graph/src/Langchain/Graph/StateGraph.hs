{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Graph.StateGraph
Description : Core StateGraph engine and pure state reducer combinators
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides explicit, inspectable, stateful agent graph workflow primitives following graph laws.
-}
module Langchain.Graph.StateGraph
  ( NodeId
  , startNodeId
  , endNodeId
  , StateReducer
  , Node (..)
  , Edge (..)
  , StateGraph (..)
  , CompiledGraph (..)
  , emptyStateGraph
  , addNode
  , addEdge
  , addConditionalEdge
  , compileGraph
  , runGraph
  , appendMessagesReducer
  , replaceFieldReducer
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, internalError)
import Langchain.Core.Model (Message)

-- | Unique node identifier in a StateGraph
type NodeId = Text

-- | Reserved start node constant
startNodeId :: NodeId
startNodeId = "__start__"

-- | Reserved end node constant
endNodeId :: NodeId
endNodeId = "__end__"

-- | Pure state merge function reducer satisfying associative algebra laws
type StateReducer s = s -> s -> s

-- | Node execution action in a StateGraph
data Node s m = Node
  { nodeId :: NodeId
  , nodeAction :: s -> m (Either LangchainError s)
  }

instance Show (Node s m) where
  show n = "Node { nodeId = " ++ show (nodeId n) ++ " }"

-- | Edge representation for transition between graph nodes
data Edge s m
  = StaticEdge NodeId
  | ConditionalEdge (s -> m (Either LangchainError NodeId))

-- | Declarative StateGraph specification
data StateGraph s m = StateGraph
  { graphNodes :: Map NodeId (Node s m)
  , graphEdges :: Map NodeId (Edge s m)
  , graphReducer :: StateReducer s
  }

-- | Validated, compiled graph ready for execution
data CompiledGraph s m = CompiledGraph
  { compiledNodes :: Map NodeId (Node s m)
  , compiledEdges :: Map NodeId (Edge s m)
  , compiledReducer :: StateReducer s
  }

-- | Construct an empty StateGraph with a given pure StateReducer
emptyStateGraph :: StateReducer s -> StateGraph s m
emptyStateGraph reducer =
  StateGraph
    { graphNodes = Map.empty
    , graphEdges = Map.empty
    , graphReducer = reducer
    }

-- | Add an execution node to the StateGraph
addNode :: NodeId -> (s -> m (Either LangchainError s)) -> StateGraph s m -> StateGraph s m
addNode name action g =
  let n = Node {nodeId = name, nodeAction = action}
   in g {graphNodes = Map.insert name n (graphNodes g)}

-- | Add a static transition edge between two nodes
addEdge :: NodeId -> NodeId -> StateGraph s m -> StateGraph s m
addEdge fromNode toNode g =
  g {graphEdges = Map.insert fromNode (StaticEdge toNode) (graphEdges g)}

-- | Add a dynamic conditional transition edge
addConditionalEdge ::
  NodeId -> (s -> m (Either LangchainError NodeId)) -> StateGraph s m -> StateGraph s m
addConditionalEdge fromNode condFn g =
  g {graphEdges = Map.insert fromNode (ConditionalEdge condFn) (graphEdges g)}

-- | Validate state graph invariants and compile to a CompiledGraph
compileGraph :: StateGraph s m -> Either LangchainError (CompiledGraph s m)
compileGraph StateGraph {..} =
  if Map.null graphNodes
    then Left $ internalError "StateGraph must contain at least one node" (Just "compileGraph") Nothing
    else
      Right
        CompiledGraph
          { compiledNodes = graphNodes
          , compiledEdges = graphEdges
          , compiledReducer = graphReducer
          }

-- | Execute a CompiledGraph from start node or specified currentId to endNodeId
runGraph ::
  (MonadIO m, MonadError LangchainError m) =>
  CompiledGraph s m ->
  NodeId ->
  s ->
  m s
runGraph CompiledGraph {..} currentId state
  | currentId == endNodeId = pure state
  | otherwise = case Map.lookup currentId compiledNodes of
      Just node -> do
        eNextState <- nodeAction node state
        case eNextState of
          Left err -> throwError err
          Right stepState -> do
            let mergedState = compiledReducer state stepState
            case Map.lookup currentId compiledEdges of
              Nothing -> pure mergedState
              Just (StaticEdge nextId) -> runGraph CompiledGraph {..} nextId mergedState
              Just (ConditionalEdge cond) -> do
                eNextId <- cond mergedState
                case eNextId of
                  Left err -> throwError err
                  Right nextId -> runGraph CompiledGraph {..} nextId mergedState
      Nothing -> case Map.lookup currentId compiledEdges of
        Just (StaticEdge nextId) -> runGraph CompiledGraph {..} nextId state
        Just (ConditionalEdge cond) -> do
          eNextId <- cond state
          case eNextId of
            Left err -> throwError err
            Right nextId -> runGraph CompiledGraph {..} nextId state
        Nothing ->
          throwError $
            internalError ("Node not found in compiled graph: " <> currentId) (Just currentId) Nothing

-- | Standard pure reducer concatenating Message lists
appendMessagesReducer :: StateReducer [Message]
appendMessagesReducer old new = old ++ new

-- | Standard pure reducer replacing previous state field with new state
replaceFieldReducer :: StateReducer a
replaceFieldReducer _ new = new

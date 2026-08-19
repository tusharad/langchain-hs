{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.DynamicFlow
Description : Declarative Dynamic Flow Engine (Langflow-style JSON workflow execution)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides runtime dynamic graph parsing, cycle detection, topological dependency execution,
and intermediate value routing across connected node sockets.
-}
module Langchain.Graph.DynamicFlow
  ( FlowNode (..)
  , FlowEdge (..)
  , DynamicFlow (..)
  , FlowExecutionResult (..)
  , NodeExecutor
  , ComponentRegistry
  , topologicalSortFlow
  , executeDynamicFlow
  , newDynamicFlow
  ) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  )
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError, agentError)

-- | Declarative node in a dynamic flow graph
data FlowNode = FlowNode
  { nodeId :: !Text
  , nodeType :: !Text
  , nodeParams :: !(Map Text Value)
  }
  deriving (Show, Eq, Generic)

instance ToJSON FlowNode
instance FromJSON FlowNode

-- | Directed edge connecting source node to target node
data FlowEdge = FlowEdge
  { edgeSource :: !Text
  , edgeTarget :: !Text
  , edgeHandle :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance ToJSON FlowEdge
instance FromJSON FlowEdge

-- | Dynamic JSON-serializable Flow Graph
data DynamicFlow = DynamicFlow
  { flowId :: !Text
  , flowNodes :: ![FlowNode]
  , flowEdges :: ![FlowEdge]
  }
  deriving (Show, Eq, Generic)

instance ToJSON DynamicFlow
instance FromJSON DynamicFlow

-- | Helper to create a new Dynamic Flow
newDynamicFlow :: Text -> [FlowNode] -> [FlowEdge] -> DynamicFlow
newDynamicFlow = DynamicFlow

-- | Node execution handler type
type NodeExecutor m = FlowNode -> Map Text Value -> m (Map Text Value)

-- | Component registry mapping nodeType string to executor function
type ComponentRegistry m = Map Text (NodeExecutor m)

-- | Flow execution output container
data FlowExecutionResult = FlowExecutionResult
  { flowOutputs :: !(Map Text (Map Text Value)) -- NodeId -> Output key/values
  , flowExecutionOrder :: ![Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON FlowExecutionResult
instance FromJSON FlowExecutionResult

-- | Topological sort with cycle detection using Kahn's algorithm
topologicalSortFlow :: DynamicFlow -> Either Text [Text]
topologicalSortFlow DynamicFlow {..} =
  let allNodeIds = map nodeId flowNodes
      inDegrees = foldl' countInDegrees (Map.fromList [(nId, 0 :: Int) | nId <- allNodeIds]) flowEdges
      adjList = foldl' buildAdj Map.empty flowEdges
      initialZeroNodes = [nId | (nId, deg) <- Map.toList inDegrees, deg == 0]
   in kahnLoop initialZeroNodes inDegrees adjList [] (length allNodeIds)
  where
    countInDegrees acc edge = Map.insertWith (+) (edgeTarget edge) 1 acc
    buildAdj acc edge = Map.insertWith (++) (edgeSource edge) [edgeTarget edge] acc

    kahnLoop [] _ _ order total
      | length order == total = Right (reverse order)
      | otherwise = Left "Cycle detected in dynamic flow graph"
    kahnLoop (cur : rest) inDegs adj order total =
      let neighbors = Map.findWithDefault [] cur adj
          (newInDegs, newlyZero) = foldl' (decrementNeighbor cur) (inDegs, []) neighbors
       in kahnLoop (rest ++ newlyZero) newInDegs adj (cur : order) total

    decrementNeighbor _ (curDegs, zeroes) target =
      let prevDeg = Map.findWithDefault 1 target curDegs
          newDeg = prevDeg - 1
          updatedMap = Map.insert target newDeg curDegs
       in if newDeg == 0
            then (updatedMap, zeroes ++ [target])
            else (updatedMap, zeroes)

-- | Execute a dynamic flow graph sequentially in topological order
executeDynamicFlow ::
  (MonadIO m, MonadError LangchainError m) =>
  ComponentRegistry m ->
  DynamicFlow ->
  -- | Initial global/flow inputs
  Map Text Value ->
  m FlowExecutionResult
executeDynamicFlow registry flow@DynamicFlow {..} initialInputs = do
  order <- case topologicalSortFlow flow of
    Left err -> throwError $ agentError ("Dynamic flow validation failed: " <> err) (Just flowId) Nothing
    Right ord -> pure ord

  let nodeLookup = Map.fromList [(nodeId n, n) | n <- flowNodes]
  let incomingEdgesLookup = foldl' (\m e -> Map.insertWith (++) (edgeTarget e) [e] m) Map.empty flowEdges

  (finalOutputs, _) <-
    foldM (executeStep nodeLookup incomingEdgesLookup) (Map.empty, initialInputs) order

  pure
    FlowExecutionResult
      { flowOutputs = finalOutputs
      , flowExecutionOrder = order
      }
  where
    executeStep nodeLookup inEdges (outputAcc, curEnv) nId = do
      node <- case Map.lookup nId nodeLookup of
        Nothing -> throwError $ agentError ("Node not found: " <> nId) (Just flowId) (Just nId)
        Just n -> pure n

      executor <- case Map.lookup (nodeType node) registry of
        Nothing ->
          throwError $ agentError ("Unknown node component type: " <> nodeType node) (Just flowId) (Just nId)
        Just ex -> pure ex

      -- Resolve inputs from upstream connected nodes
      let incomingEdges = Map.findWithDefault [] nId inEdges
      let upstreamInputs = foldl' (collectUpstreamInputs outputAcc) Map.empty incomingEdges

      -- Merge node parameters, upstream inputs, and global environment
      let mergedInputs = nodeParams node <> curEnv <> upstreamInputs
      nodeOutput <- executor node mergedInputs

      let newOutputAcc = Map.insert nId nodeOutput outputAcc
      pure (newOutputAcc, curEnv)

    collectUpstreamInputs outputAcc acc edge =
      let srcId = edgeSource edge
          srcOutputs = Map.findWithDefault Map.empty srcId outputAcc
       in case edgeHandle edge of
            Just handleName -> case Map.lookup handleName srcOutputs of
              Just val -> Map.insert handleName val acc
              Nothing -> acc <> srcOutputs
            Nothing -> acc <> srcOutputs

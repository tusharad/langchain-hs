{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.StateGraph
Description : Production StateGraph engine with Runnable & LLM node combinators
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Production-grade stateful agent orchestration engine. Supports:
  * First-class LLM ChatModel nodes ('addModelNode', 'addChatNode')
  * First-class Runnable & RunnableTree AST nodes ('addRunnableNode', 'addRunnableTreeNode')
  * Pure state transformation nodes ('addTransformNode')
  * Hierarchical sub-graph embedding ('addSubGraphNode')
  * Static and dynamic conditional edge transitions
  * Recursion limits to prevent runaway loops in cyclic graphs
  * Configurable execution tracing and step logging
-}
module Langchain.Graph.StateGraph
  ( -- * Core Types
    NodeId
  , startNodeId
  , endNodeId
  , StateReducer
  , Node (..)
  , Edge (..)
  , GraphConfig (..)
  , defaultGraphConfig
  , StateGraph (..)
  , CompiledGraph (..)

    -- * Graph Construction
  , emptyStateGraph
  , setRecursionLimit
  , setStepLogger
  , addEdge
  , addConditionalEdge

    -- * Node Combinators
  , addNode
  , addTransformNode
  , addRunnableNode
  , addRunnableTreeNode
  , addModelNode
  , addModelNodeWithConfig
  , addChatNode
  , addChatNodeWithConfig
  , addSubGraphNode
  , addStreamingNode
  , addStreamingModelNode
  , addStreamingModelNodeWithConfig

    -- * Compilation & Execution
  , compileGraph
  , runGraph
  , runGraphWithConfig

    -- * Streaming Execution
  , streamGraph
  , streamGraphWithConfig
  , streamGraphWith
  , streamGraphStream

    -- * Pure State Reducers
  , appendMessagesReducer
  , replaceFieldReducer
  ) where

import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON, Value, toJSON)
import Data.Conduit (ConduitT, fuseUpstream, runConduit, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Error (LangchainError, agentError, internalError)
import Langchain.Model (ChatModel, Message, ModelConfig, extractMessageText, userMessage)
import qualified Langchain.Model as M (invoke, stream)
import Langchain.Runnable (Runnable (..), RunnableTree, interpret)
import Langchain.Stream
  ( EventStream
  , StreamEvent (NodeEnd, NodeStart)
  , liftChatStream
  , tapStreamMessage
  )

-- | Unique node identifier in a StateGraph
type NodeId = Text

-- | Reserved start node identifier
startNodeId :: NodeId
startNodeId = "__start__"

-- | Reserved end node identifier
endNodeId :: NodeId
endNodeId = "__end__"

-- | Pure state merge reducer function
type StateReducer s = s -> s -> s

-- | Node execution action in a StateGraph
data Node s m = Node
  { nodeId :: !NodeId
  , nodeAction :: !(s -> m (Either LangchainError s))
  }

instance Show (Node s m) where
  show n = "Node { nodeId = " ++ show (nodeId n) ++ " }"

-- | Edge transition between graph nodes
data Edge s m
  = StaticEdge !NodeId
  | ConditionalEdge !(s -> m (Either LangchainError NodeId))

-- | Execution configuration for a StateGraph
data GraphConfig = GraphConfig
  { recursionLimit :: !Int
  , logStepTransitions :: !Bool
  , stepLogger :: !(Text -> IO ())
  }

-- | Default configuration with 25-step recursion limit and disabled logging
defaultGraphConfig :: GraphConfig
defaultGraphConfig =
  GraphConfig
    { recursionLimit = 25
    , logStepTransitions = False
    , stepLogger = const (pure ())
    }

-- | Declarative StateGraph specification
data StateGraph s m = StateGraph
  { graphNodes :: !(Map NodeId (Node s m))
  , graphStreamNodes :: !(Map NodeId (s -> ConduitT () StreamEvent m (Either LangchainError s)))
  , graphEdges :: !(Map NodeId (Edge s m))
  , graphReducer :: !(StateReducer s)
  , graphConfig :: !GraphConfig
  }

-- | A validated, compiled StateGraph ready for execution
data CompiledGraph s m = CompiledGraph
  { compiledNodes :: !(Map NodeId (Node s m))
  , compiledStreamNodes :: !(Map NodeId (s -> ConduitT () StreamEvent m (Either LangchainError s)))
  , compiledEdges :: !(Map NodeId (Edge s m))
  , compiledReducer :: !(StateReducer s)
  , compiledConfig :: !GraphConfig
  }

-- | Construct an empty StateGraph with a pure StateReducer
emptyStateGraph :: StateReducer s -> StateGraph s m
emptyStateGraph reducer =
  StateGraph
    { graphNodes = Map.empty
    , graphStreamNodes = Map.empty
    , graphEdges = Map.empty
    , graphReducer = reducer
    , graphConfig = defaultGraphConfig
    }

-- | Set the maximum step limit to prevent infinite loops in cyclic graphs
setRecursionLimit :: Int -> StateGraph s m -> StateGraph s m
setRecursionLimit limit g =
  g {graphConfig = (graphConfig g) {recursionLimit = limit}}

-- | Configure a custom step transition logger function
setStepLogger :: (Text -> IO ()) -> StateGraph s m -> StateGraph s m
setStepLogger logger g =
  g {graphConfig = (graphConfig g) {logStepTransitions = True, stepLogger = logger}}

-- | Add a raw execution node to the StateGraph
addNode :: NodeId -> (s -> m (Either LangchainError s)) -> StateGraph s m -> StateGraph s m
addNode name action g =
  let n = Node {nodeId = name, nodeAction = action}
   in g {graphNodes = Map.insert name n (graphNodes g)}

-- | Add a pure synchronous state transformation node (no error handling boilerplate)
addTransformNode :: Monad m => NodeId -> (s -> s) -> StateGraph s m -> StateGraph s m
addTransformNode name f =
  addNode name (pure . Right . f)

-- | Add a node backed by any Langchain 'Runnable' (e.g. Tools, ModelRunnable)
addRunnableNode ::
  (Runnable r m, Monad m) =>
  NodeId ->
  r ->
  (s -> RunnableInput r) ->
  (s -> RunnableOutput r -> s) ->
  StateGraph s m ->
  StateGraph s m
addRunnableNode name runnable toInput mergeOutput =
  addNode name $ \state -> do
    eRes <- invoke runnable (toInput state)
    pure $ fmap (mergeOutput state) eRes

-- | Add a node backed by an algebraic 'RunnableTree' pipeline AST
addRunnableTreeNode ::
  (MonadIO m) =>
  NodeId ->
  RunnableTree (ExceptT LangchainError IO) i o ->
  (s -> i) ->
  (s -> o -> s) ->
  StateGraph s m ->
  StateGraph s m
addRunnableTreeNode name tree toInput mergeOutput =
  addNode name $ \state -> liftIO $ do
    eRes <- runExceptT $ interpret tree (toInput state)
    pure $ fmap (mergeOutput state) eRes

-- | Add a dedicated ChatModel node taking and producing rich 'Message' lists
addModelNode ::
  (ChatModel c, MonadIO m) =>
  NodeId ->
  c ->
  (s -> [Message]) ->
  (s -> Message -> s) ->
  StateGraph s m ->
  StateGraph s m
addModelNode name chatModel toMessages mergeMessage =
  addNode name $ \state -> do
    eMsg <- liftIO $ runExceptT $ M.invoke chatModel (toMessages state) Nothing
    pure $ fmap (mergeMessage state) eMsg

-- | Add a dedicated ChatModel node with explicit model configuration
addModelNodeWithConfig ::
  (ChatModel c, MonadIO m) =>
  NodeId ->
  c ->
  ModelConfig c ->
  (s -> [Message]) ->
  (s -> Message -> s) ->
  StateGraph s m ->
  StateGraph s m
addModelNodeWithConfig name chatModel cfg toMessages mergeMessage =
  addNode name $ \state -> do
    eMsg <- liftIO $ runExceptT $ M.invoke chatModel (toMessages state) (Just cfg)
    pure $ fmap (mergeMessage state) eMsg

-- | Add a simple text-in, text-out ChatModel node
addChatNode ::
  (ChatModel c, MonadIO m) =>
  NodeId ->
  c ->
  (s -> Text) ->
  (s -> Text -> s) ->
  StateGraph s m ->
  StateGraph s m
addChatNode name chatModel toPrompt mergeText =
  addNode name $ \state -> do
    let msgs = [userMessage (toPrompt state)]
    eMsg <- liftIO $ runExceptT $ M.invoke chatModel msgs Nothing
    pure $ fmap (mergeText state . extractMessageText) eMsg

-- | Add a simple text-in, text-out ChatModel node with explicit model configuration
addChatNodeWithConfig ::
  (ChatModel c, MonadIO m) =>
  NodeId ->
  c ->
  ModelConfig c ->
  (s -> Text) ->
  (s -> Text -> s) ->
  StateGraph s m ->
  StateGraph s m
addChatNodeWithConfig name chatModel cfg toPrompt mergeText =
  addNode name $ \state -> do
    let msgs = [userMessage (toPrompt state)]
    eMsg <- liftIO $ runExceptT $ M.invoke chatModel msgs (Just cfg)
    pure $ fmap (mergeText state . extractMessageText) eMsg

-- | Embed a compiled sub-graph as a single node in a parent StateGraph
addSubGraphNode ::
  (MonadIO m, MonadError LangchainError m) =>
  NodeId ->
  NodeId ->
  CompiledGraph subState m ->
  (s -> subState) ->
  (s -> subState -> s) ->
  StateGraph s m ->
  StateGraph s m
addSubGraphNode name entryNode subGraph toSubState mergeSubState =
  addNode name $ \parentState -> do
    let initialSub = toSubState parentState
    finalSub <- runGraph subGraph entryNode initialSub
    pure $ Right $ mergeSubState parentState finalSub

-- | Add a static transition edge between two nodes
addEdge :: NodeId -> NodeId -> StateGraph s m -> StateGraph s m
addEdge fromNode toNode g =
  g {graphEdges = Map.insert fromNode (StaticEdge toNode) (graphEdges g)}

-- | Add a dynamic conditional transition edge
addConditionalEdge ::
  NodeId ->
  (s -> m (Either LangchainError NodeId)) ->
  StateGraph s m ->
  StateGraph s m
addConditionalEdge fromNode condFn g =
  g {graphEdges = Map.insert fromNode (ConditionalEdge condFn) (graphEdges g)}

-- | Reserved run identifier for streamed graph lifecycle events
graphRunId :: Text
graphRunId = "state-graph-run"

-- | Add a streaming node capable of emitting inner 'StreamEvent's during 'streamGraph'.
addStreamingNode ::
  Monad m =>
  NodeId ->
  (s -> ConduitT () StreamEvent m (Either LangchainError s)) ->
  StateGraph s m ->
  StateGraph s m
addStreamingNode name streamAct g =
  let syncAction state = runConduit (streamAct state `fuseUpstream` CL.sinkNull)
      n = Node {nodeId = name, nodeAction = syncAction}
   in g
        { graphNodes = Map.insert name n (graphNodes g)
        , graphStreamNodes = Map.insert name streamAct (graphStreamNodes g)
        }

addStreamingModelNodeMaybeConfig ::
  (ChatModel c, MonadIO m, MonadError LangchainError m) =>
  NodeId ->
  c ->
  Maybe (ModelConfig c) ->
  (s -> [Message]) ->
  (s -> Message -> s) ->
  StateGraph s m ->
  StateGraph s m
addStreamingModelNodeMaybeConfig name chatModel mbCfg toMessages mergeMessage =
  addStreamingNode name $ \state ->
    ( do
        finalMsg <- liftChatStream (M.stream chatModel (toMessages state) mbCfg) .| tapStreamMessage
        pure $ Right (mergeMessage state finalMsg)
    )
      `catchError` (pure . Left)

-- | Add a streaming ChatModel node that forwards inner 'LLMStart', 'LLMChunk', and 'LLMEnd' events.
addStreamingModelNode ::
  (ChatModel c, MonadIO m, MonadError LangchainError m) =>
  NodeId ->
  c ->
  (s -> [Message]) ->
  (s -> Message -> s) ->
  StateGraph s m ->
  StateGraph s m
addStreamingModelNode name chatModel = addStreamingModelNodeMaybeConfig name chatModel Nothing

-- | Add a streaming ChatModel node with explicit model configuration.
addStreamingModelNodeWithConfig ::
  (ChatModel c, MonadIO m, MonadError LangchainError m) =>
  NodeId ->
  c ->
  ModelConfig c ->
  (s -> [Message]) ->
  (s -> Message -> s) ->
  StateGraph s m ->
  StateGraph s m
addStreamingModelNodeWithConfig name chatModel cfg = addStreamingModelNodeMaybeConfig name chatModel (Just cfg)

-- | Validate state graph invariants and compile to an executable 'CompiledGraph'
compileGraph :: StateGraph s m -> Either LangchainError (CompiledGraph s m)
compileGraph StateGraph {..}
  | Map.null graphNodes =
      Left $ internalError "StateGraph must contain at least one node" (Just "compileGraph") Nothing
  | otherwise =
      Right
        CompiledGraph
          { compiledNodes = graphNodes
          , compiledStreamNodes = graphStreamNodes
          , compiledEdges = graphEdges
          , compiledReducer = graphReducer
          , compiledConfig = graphConfig
          }

-- | Stream graph execution events ('NodeStart', inner node events, and 'NodeEnd').
streamGraph ::
  (ToJSON s, MonadIO m, MonadError LangchainError m) =>
  CompiledGraph s m ->
  NodeId ->
  s ->
  ConduitT () StreamEvent m s
streamGraph cg = streamGraphWithConfig (compiledConfig cg) cg

-- | Stream graph execution events with an explicit 'GraphConfig' override.
streamGraphWithConfig ::
  (ToJSON s, MonadIO m, MonadError LangchainError m) =>
  GraphConfig ->
  CompiledGraph s m ->
  NodeId ->
  s ->
  ConduitT () StreamEvent m s
streamGraphWithConfig = streamGraphWith toJSON

-- | Stream graph execution events using a custom state summary function (supports non-'ToJSON' states).
streamGraphWith ::
  (MonadIO m, MonadError LangchainError m) =>
  (s -> Value) ->
  GraphConfig ->
  CompiledGraph s m ->
  NodeId ->
  s ->
  ConduitT () StreamEvent m s
streamGraphWith = executeGraphWith True

-- | Stream graph execution as an 'EventStream m' discarding the final state.
streamGraphStream ::
  (ToJSON s, MonadIO m, MonadError LangchainError m) =>
  CompiledGraph s m ->
  NodeId ->
  s ->
  EventStream m
streamGraphStream cg entry s = void (streamGraph cg entry s)

-- | Execute a CompiledGraph starting from the given entry NodeId
runGraph ::
  (MonadIO m, MonadError LangchainError m) =>
  CompiledGraph s m ->
  NodeId ->
  s ->
  m s
runGraph cg = runGraphWithConfig (compiledConfig cg) cg

-- | Execute a CompiledGraph with an explicit configuration override
runGraphWithConfig ::
  (MonadIO m, MonadError LangchainError m) =>
  GraphConfig ->
  CompiledGraph s m ->
  NodeId ->
  s ->
  m s
runGraphWithConfig cfg cg entry initState =
  runConduit $
    executeGraphWith False (const (toJSON ())) cfg cg entry initState `fuseUpstream` CL.sinkNull

executeGraphWith ::
  (MonadIO m, MonadError LangchainError m) =>
  Bool ->
  (s -> Value) ->
  GraphConfig ->
  CompiledGraph s m ->
  NodeId ->
  s ->
  ConduitT () StreamEvent m s
executeGraphWith emitEvents encodeState cfg cg =
  executeStep 0
  where
    CompiledGraph {..} = cg
    GraphConfig {..} = cfg

    logMsg msg =
      when logStepTransitions $ liftIO $ stepLogger msg

    emitNodeEvent mkEv nid st =
      when emitEvents $ yield (mkEv graphRunId nid (encodeState st))

    runNodeAction currentId node state
      | emitEvents = case Map.lookup currentId compiledStreamNodes of
          Just streamAct -> streamAct state
          Nothing -> lift (nodeAction node state)
      | otherwise = lift (nodeAction node state)

    executeStep stepCount currentId state
      | stepCount >= recursionLimit = do
          let errMsg =
                "StateGraph recursion limit exceeded (max "
                  <> T.pack (show recursionLimit)
                  <> " steps at node: "
                  <> currentId
                  <> ")"
          logMsg $ "[StateGraph ERROR] " <> errMsg
          throwError $ agentError errMsg (Just "runGraph") (Just currentId)
      | currentId == endNodeId = do
          logMsg "[StateGraph] Reached terminal node (__end__)"
          pure state
      | otherwise = case Map.lookup currentId compiledNodes of
          Just node -> do
            logMsg $
              "[StateGraph] Step " <> T.pack (show (stepCount + 1)) <> ": Entering node '" <> currentId <> "'"
            emitNodeEvent NodeStart currentId state
            eNextState <- runNodeAction currentId node state
            case eNextState of
              Left err -> do
                logMsg $ "[StateGraph ERROR] Node '" <> currentId <> "' failed: " <> T.pack (show err)
                throwError err
              Right stepState -> do
                let mergedState = compiledReducer state stepState
                logMsg $ "[StateGraph] Node '" <> currentId <> "' executed successfully"
                emitNodeEvent NodeEnd currentId mergedState
                case Map.lookup currentId compiledEdges of
                  Nothing -> do
                    logMsg $ "[StateGraph] No outgoing edge from '" <> currentId <> "', terminating"
                    pure mergedState
                  Just (StaticEdge nextId) -> do
                    logMsg $ "[StateGraph] Static edge: '" <> currentId <> "' -> '" <> nextId <> "'"
                    executeStep (stepCount + 1) nextId mergedState
                  Just (ConditionalEdge cond) -> do
                    eNextId <- lift $ cond mergedState
                    case eNextId of
                      Left err -> do
                        logMsg $
                          "[StateGraph ERROR] Conditional edge from '" <> currentId <> "' failed: " <> T.pack (show err)
                        throwError err
                      Right nextId -> do
                        logMsg $ "[StateGraph] Conditional edge: '" <> currentId <> "' -> '" <> nextId <> "'"
                        executeStep (stepCount + 1) nextId mergedState
          Nothing -> case Map.lookup currentId compiledEdges of
            Just (StaticEdge nextId) -> do
              logMsg $ "[StateGraph] Passthrough edge: '" <> currentId <> "' -> '" <> nextId <> "'"
              executeStep (stepCount + 1) nextId state
            Just (ConditionalEdge cond) -> do
              eNextId <- lift $ cond state
              case eNextId of
                Left err -> throwError err
                Right nextId -> do
                  logMsg $ "[StateGraph] Passthrough conditional edge: '" <> currentId <> "' -> '" <> nextId <> "'"
                  executeStep (stepCount + 1) nextId state
            Nothing -> do
              let errText = "Node not found in compiled graph: " <> currentId
              logMsg $ "[StateGraph ERROR] " <> errText
              throwError $ internalError errText (Just currentId) Nothing

-- | Standard pure reducer concatenating Message lists
appendMessagesReducer :: StateReducer [Message]
appendMessagesReducer old new = old ++ new

-- | Standard pure reducer replacing previous state with the newly returned state
replaceFieldReducer :: StateReducer a
replaceFieldReducer _ new = new

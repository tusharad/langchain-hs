{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Langchain.Agent.ReAct
Description : Effect-polymorphic ReAct (Reasoning + Acting) Agent
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

ReAct (Reasoning and Acting) agent loop that supports:

* __Effect-polymorphic tools__: Tools operate in the agent's monad @m@.
* __Memory integration__: Pluggable 'BaseMemory' via 'withMemory'.
* __Model-specific configs__: Merged with tool definitions via 'withModelConfig'.
* __Observability__: Lifecycle hooks via 'withCallbackManager'.
* __Error resilience__: Configurable 'ToolErrorStrategy'.
* __Iteration limits__: Prevents infinite loops via 'withMaxIterations'.
* __Early stop conditions__: User-defined stopping predicates via 'withStopCondition'.
* __Reasoning traces__: Inspection via 'runReActAgentWithTrace'.

=== Quick Start

@
let tools = [shellTool]
let agent = defaultReActAgent model tools
result <- runReActAgent agent [userMessage "What is 2+2?"]
@

=== With Memory and Tracing

@
mem <- newWindowBufferMemory 10 []
let agent =
      withMemory mem $
        withSystemPrompt "You are a helpful math tutor." $
          defaultReActAgent model tools
trace <- runReActAgentWithTrace agent [userMessage "What is the square root of 144?"]
@
-}
module Langchain.Agent.ReAct
  ( -- * Core Types
    AgentStep (..)
  , AgentTrace (..)
  , ReActAgent (..)
  , ToolErrorStrategy (..)

    -- * Construction
  , defaultReActAgent

    -- * Configuration (builder-style)
  , withMemory
  , withoutMemory
  , withSystemPrompt
  , withMaxIterations
  , withModelConfig
  , withCallbackManager
  , withStopCondition
  , withToolErrorStrategy
  , withStreamingTools

    -- * Execution
  , reactStep
  , runReActAgent
  , runReActAgentWithTrace

    -- * Streaming Execution
  , InterruptibleResult (..)
  , reactStepStream
  , streamReActAgent
  , streamReActAgentWithTrace
  , runReActAgentStream
  , runReActAgentStreamInterruptible
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson (toJSON)
import Data.Conduit (ConduitT, fuseUpstream, mapOutput, runConduit, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import Langchain.Callback.Manager (CallbackEvent (..), CallbackManager, dispatchEvent)
import Langchain.Error (LangchainError, agentError, errorMessage)
import Langchain.Memory.Core (BaseMemory (..), SomeMemory (..))
import Langchain.Model
import qualified Langchain.Model.Types as M
import Langchain.Stream
  ( CancelToken
  , EventStream
  , StreamEvent
    ( ChainEnd
    , ChainStart
    , LLMChunk
    , LLMEnd
    , LLMStart
    , ToolChunk
    , ToolEnd
    , ToolErrorEvent
    , ToolStart
    )
  , isCancelled
  , liftChatStream
  , liftChatStreamWithCancel
  , tapStreamMessage
  )
import Langchain.Tool
import Langchain.Tool.Binding (ToolBinder (..))

-- | How the agent handles tool execution failures.
data ToolErrorStrategy
  = -- | Feed the error message back to the LLM as a Tool observation so it can retry or recover.
    ContinueOnError
  | -- | Abort the entire agent loop immediately on any tool error.
    FailOnError
  deriving (Eq, Show)

-- | A single step in the ReAct reasoning chain.
data AgentStep
  = -- | The model decided to call one or more tools.
    AgentAction Message [ToolCall]
  | -- | The model produced a final answer (no tool calls).
    AgentFinish Message
  deriving (Eq, Show)

-- | Complete reasoning trace: intermediate steps + final answer.
data AgentTrace = AgentTrace
  { traceSteps :: [(AgentStep, [Message])]
  -- ^ Each pair is (the step, observation messages produced)
  , traceFinalAnswer :: Message
  -- ^ The final answer message
  }
  deriving (Eq, Show)

{- | ReAct Agent configuration.

Holds the LLM model, the effect-polymorphic tools @[Tool m]@, and optional
configuration for memory, callbacks, iterations, model config, and error strategy.
Use 'defaultReActAgent' to construct, then configure with the @with*@ builder functions.
-}
data ReActAgent model m = ReActAgent
  { agentModel :: model
  -- ^ The LLM model backing this agent
  , agentTools :: [Tool m]
  -- ^ Available tools the agent can invoke in monad @m@
  , agentMaxIterations :: Int
  -- ^ Maximum reasoning iterations before giving up (default: 100)
  , agentSystemPrompt :: Maybe Text
  -- ^ Optional system prompt prepended to every invocation
  , agentModelConfig :: Maybe (ModelConfig model)
  -- ^ Optional provider-specific model configuration (temperature, top_p, etc.)
  , agentMemory :: Maybe SomeMemory
  -- ^ Optional 'BaseMemory' for persistent conversation history
  , agentCallbackManager :: Maybe CallbackManager
  -- ^ Optional callback manager for observability hooks
  , agentStopCondition :: Maybe (Message -> Bool)
  -- ^ Optional early-stop predicate checked after each LLM response
  , agentToolErrorStrategy :: ToolErrorStrategy
  -- ^ How to handle tool execution errors (default: 'ContinueOnError')
  , agentStreamingTools :: [StreamingTool m]
  -- ^ Optional streaming tools capable of emitting incremental 'ToolChunk' events
  }

{- | Construct a ReAct agent with sensible defaults.

All optional fields start as 'Nothing' / defaults. Use the @with*@ functions to configure.
-}
defaultReActAgent ::
  -- | The LLM chat model backing this agent
  model ->
  -- | Available effect-polymorphic tools
  [Tool m] ->
  -- | Initialized ReActAgent with default settings
  ReActAgent model m
defaultReActAgent model tools =
  ReActAgent
    { agentModel = model
    , agentTools = tools
    , agentMaxIterations = 100
    , agentSystemPrompt = Nothing
    , agentModelConfig = Nothing
    , agentMemory = Nothing
    , agentCallbackManager = Nothing
    , agentStopCondition = Nothing
    , agentToolErrorStrategy = ContinueOnError
    , agentStreamingTools = []
    }

{- | Attach a 'BaseMemory' instance for persistent conversation history.

When memory is set, 'runReActAgent' will:

1. Load existing messages from memory before the first LLM call.
2. Save the user query + final answer to memory after completion.
-}
withMemory ::
  (BaseMemory mem) =>
  -- | BaseMemory instance to store conversation messages
  mem ->
  -- | Current agent instance
  ReActAgent model m ->
  -- | Agent configured with persistent memory
  ReActAgent model m
withMemory mem agent = agent {agentMemory = Just (SomeMemory mem)}

-- | Remove memory from the agent.
withoutMemory ::
  -- | Current agent instance
  ReActAgent model m ->
  -- | Agent with memory detached
  ReActAgent model m
withoutMemory agent = agent {agentMemory = Nothing}

-- | Set the system prompt that instructs the agent's persona and behavior.
withSystemPrompt ::
  -- | System instructions / persona text
  Text ->
  -- | Current agent instance
  ReActAgent model m ->
  -- | Agent configured with system prompt
  ReActAgent model m
withSystemPrompt prompt agent = agent {agentSystemPrompt = Just prompt}

-- | Override the maximum number of reasoning iterations (default: 10).
withMaxIterations ::
  -- | Maximum reasoning loop iterations before aborting
  Int ->
  -- | Current agent instance
  ReActAgent model m ->
  -- | Agent configured with iteration limit
  ReActAgent model m
withMaxIterations n agent = agent {agentMaxIterations = n}

{- | Attach a provider-specific model configuration (temperature, top_p, etc.).

This config is merged with the tool-binding config on every 'invoke' call,
giving you full control over LLM parameters without losing tool support.
-}
withModelConfig ::
  -- | Provider-specific model configuration (temperature, top_p, etc.)
  Maybe (ModelConfig model) ->
  -- | Current agent instance
  ReActAgent model m ->
  -- | Agent configured with model options
  ReActAgent model m
withModelConfig cfg agent = agent {agentModelConfig = cfg}

{- | Attach a 'CallbackManager' for observability.

When set, the agent emits 'OnToolStart' / 'OnToolEnd' / 'OnError' events
for each tool invocation in the reasoning loop.
-}
withCallbackManager ::
  -- | Callback manager for observability event hooks
  CallbackManager ->
  -- | Current agent instance
  ReActAgent model m ->
  -- | Agent configured with callback manager
  ReActAgent model m
withCallbackManager mgr agent = agent {agentCallbackManager = Just mgr}

{- | Set a custom stop condition checked after each LLM response.

If the predicate returns 'True' for the LLM's response message, the agent
immediately returns that message as the final answer, even if the LLM
included tool calls.
-}
withStopCondition ::
  -- | Predicate checked after each LLM response for early exit
  (Message -> Bool) ->
  -- | Current agent instance
  ReActAgent model m ->
  -- | Agent configured with custom stopping condition
  ReActAgent model m
withStopCondition cond agent = agent {agentStopCondition = Just cond}

{- | Set the tool error handling strategy.

* 'ContinueOnError' (default): feed error text back to the LLM as a Tool observation.
* 'FailOnError': abort the entire agent loop on the first tool error.
-}
withToolErrorStrategy ::
  -- | Strategy on tool failure ('ContinueOnError' or 'FailOnError')
  ToolErrorStrategy ->
  -- | Current agent instance
  ReActAgent model m ->
  -- | Agent configured with error handling strategy
  ReActAgent model m
withToolErrorStrategy strategy agent = agent {agentToolErrorStrategy = strategy}

-- | Attach 'StreamingTool's that emit incremental 'ToolChunk' events during streaming execution.
withStreamingTools ::
  [StreamingTool m] ->
  ReActAgent model m ->
  ReActAgent model m
withStreamingTools sTools agent = agent {agentStreamingTools = sTools}

-- | Classify a model response 'Message' as 'AgentAction' or 'AgentFinish'.
classifyStepResponse :: Message -> AgentStep
classifyStepResponse responseMsg =
  case messageToolCalls responseMsg of
    Just tcs@(_ : _) -> AgentAction responseMsg tcs
    _ -> AgentFinish responseMsg

allAgentTools :: ReActAgent model m -> [Tool m]
allAgentTools ReActAgent {..} = agentTools ++ map streamingToolDef agentStreamingTools

{- | Run a single step of ReAct reasoning.

Sends the current message history (with tools bound) to the LLM and
classifies the response as either 'AgentAction' (tool calls requested)
or 'AgentFinish' (final answer).
-}
reactStep ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  -- | The chat model to invoke
  model ->
  -- | Available tools to bind to the model
  [Tool m] ->
  -- | Current conversation message history
  [Message] ->
  -- | Optional provider-specific model configuration
  Maybe (ModelConfig model) ->
  -- | Step classification: 'AgentAction' or 'AgentFinish'
  m AgentStep
reactStep model tools history userCfg = do
  let toolCfg = bindToolsConfig @model tools userCfg
  responseMsg <- invoke model history toolCfg
  pure $ classifyStepResponse responseMsg

reactStepStreamWith ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  Maybe CancelToken ->
  model ->
  [Tool m] ->
  [Message] ->
  Maybe (ModelConfig model) ->
  ConduitT () StreamEvent m AgentStep
reactStepStreamWith mbTok model tools history userCfg = do
  let toolCfg = bindToolsConfig @model tools userCfg
      liftFn = maybe liftChatStream liftChatStreamWithCancel mbTok
  responseMsg <- liftFn (stream model history toolCfg) .| tapStreamMessage
  pure $ classifyStepResponse responseMsg

{- | Run a single step of ReAct reasoning using streaming LLM invocation.

Forwards all 'StreamEvent's from the underlying 'ChatModel' 'stream'
(including 'LLMStart', 'LLMChunk' with text and 'toolCallDelta', and 'LLMEnd')
downstream in real time, and returns the resulting 'AgentStep'.
-}
reactStepStream ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  -- | The chat model to stream
  model ->
  -- | Available tools to bind to the model
  [Tool m] ->
  -- | Current conversation message history
  [Message] ->
  -- | Optional provider-specific model configuration
  Maybe (ModelConfig model) ->
  -- | Stream of LLM events returning the step classification
  ConduitT () StreamEvent m AgentStep
reactStepStream = reactStepStreamWith Nothing

buildInitialHistory ::
  (MonadIO m, MonadError LangchainError m) =>
  ReActAgent model m ->
  [Message] ->
  Text ->
  m [Message]
buildInitialHistory ReActAgent {..} userMsgs caller = do
  memoryMsgs <- case agentMemory of
    Nothing -> pure []
    Just (SomeMemory mem) -> messages mem

  -- When an explicit agent system prompt is provided, avoid duplicate system prompts
  -- by stripping any default system prompt that may already exist in memory.
  let memWithoutSystem = case agentSystemPrompt of
        Just _ -> filter (\m -> messageRole m /= System) memoryMsgs
        Nothing -> memoryMsgs
      sysPrefix = maybe [] (\p -> [systemMessage p]) agentSystemPrompt
      initialHistory = sysPrefix ++ memWithoutSystem ++ userMsgs

  when (null initialHistory) $
    throwError $
      agentError
        "Cannot invoke ReAct agent with empty message history (no user query, memory, or system prompt)"
        (Just "ReActAgent")
        (Just caller)

  pure initialHistory

persistTurnMemory ::
  (MonadIO m, MonadError LangchainError m) =>
  ReActAgent model m ->
  [Message] ->
  Message ->
  m ()
persistTurnMemory ReActAgent {..} userMsgs finalMsg =
  case agentMemory of
    Nothing -> pure ()
    Just (SomeMemory mem) -> do
      mapM_ (addMessage mem) userMsgs
      addMessage mem finalMsg

toolObservationMessage :: Text -> Text -> Text -> Message
toolObservationMessage tName tcId txt =
  (textMessage M.Tool txt)
    { M.messageName = Just tName
    , M.messageToolId = Just tcId
    }

dispatchCb :: MonadIO m => Maybe CallbackManager -> CallbackEvent -> m ()
dispatchCb Nothing _ = pure ()
dispatchCb (Just mgr) ev = dispatchEvent mgr ev

reactRunId :: Text
reactRunId = "react-agent-run"

-- | Execute a ReAct loop step-by-step, parameterized over the LLM step and tool executors.
runLoopWith ::
  forall model m n.
  (MonadIO n, MonadError LangchainError n) =>
  Maybe CancelToken ->
  ReActAgent model m ->
  ([Message] -> n AgentStep) ->
  (ToolCall -> n Message) ->
  [Message] ->
  Int ->
  [(AgentStep, [Message])] ->
  n AgentTrace
runLoopWith mbTok agent@ReActAgent {..} runStep execTool = go
  where
    checkCancelled = maybe (pure False) isCancelled mbTok

    execToolsWithCancel [] acc = pure (reverse acc)
    execToolsWithCancel (tc : rest) acc = do
      cancelled <- checkCancelled
      if cancelled
        then pure (reverse acc)
        else do
          obs <- execTool tc
          execToolsWithCancel rest (obs : acc)

    go history maxIter accSteps
      | maxIter <= 0 = do
          let maxErr =
                "ReAct Agent exceeded maximum iterations ("
                  <> T.pack (show agentMaxIterations)
                  <> ")"
          now <- liftIO getCurrentTime
          dispatchCb agentCallbackManager (OnError "ReActAgent" maxErr now)
          throwError $
            agentError
              maxErr
              (Just "ReActAgent")
              (Just "runReActAgent")
      | otherwise = do
          cancelledBefore <- checkCancelled
          if cancelledBefore
            then pure $ AgentTrace accSteps (assistantMessage "")
            else do
              llmStartTime <- liftIO getCurrentTime
              dispatchCb
                agentCallbackManager
                (OnLLMStart "ReActAgent" (map extractMessageText history) llmStartTime)

              eStep <- (Right <$> runStep history) `catchError` (pure . Left)
              llmEndTime <- liftIO getCurrentTime
              let llmLatency = round (realToFrac (diffUTCTime llmEndTime llmStartTime) * 1000000 :: Double) :: Int
              case eStep of
                Left err -> do
                  dispatchCb agentCallbackManager (OnError "ReActAgent" (errorMessage err) llmEndTime)
                  throwError err
                Right step -> case step of
                  AgentFinish finalMsg -> do
                    dispatchCb
                      agentCallbackManager
                      (OnLLMEnd "ReActAgent" (extractMessageText finalMsg) llmLatency llmEndTime)
                    pure $ AgentTrace (accSteps ++ [(step, [])]) finalMsg
                  AgentAction respMsg tcs -> do
                    dispatchCb
                      agentCallbackManager
                      (OnLLMEnd "ReActAgent" (extractMessageText respMsg) llmLatency llmEndTime)
                    cancelledAfterStep <- checkCancelled
                    if cancelledAfterStep
                      then pure $ AgentTrace (accSteps ++ [(step, [])]) respMsg
                      else case agentStopCondition of
                        Just cond
                          | cond respMsg ->
                              pure $ AgentTrace (accSteps ++ [(step, [])]) respMsg
                        _ -> do
                          obsMsgs <- execToolsWithCancel tcs []
                          let newHistory = history ++ [respMsg] ++ obsMsgs
                              newSteps = accSteps ++ [(step, obsMsgs)]
                          let _ = agent
                          go newHistory (maxIter - 1) newSteps

handleToolOutcome ::
  (MonadIO n, MonadError LangchainError n) =>
  ReActAgent model m ->
  ToolCall ->
  (StreamEvent -> n ()) ->
  Either LangchainError Text ->
  Int ->
  n Message
handleToolOutcome ReActAgent {..} tc emitStream eRes durationMicros = do
  let tName = toolCallName tc
  endTime <- liftIO getCurrentTime
  case eRes of
    Left err -> do
      let errTxt = "Error executing tool " <> tName <> ": " <> errorMessage err
      dispatchCb agentCallbackManager (OnError tName errTxt endTime)
      emitStream $ ToolErrorEvent reactRunId tName err
      when (agentToolErrorStrategy == FailOnError) $
        throwError $
          agentError errTxt (Just "ReActAgent") (Just $ "executeTool:" <> tName)
      pure $ toolObservationMessage tName (toolCallId tc) errTxt
    Right outTxt -> do
      dispatchCb agentCallbackManager (OnToolEnd tName outTxt durationMicros endTime)
      emitStream $ ToolEnd reactRunId tName (toJSON outTxt)
      pure $ toolObservationMessage tName (toolCallId tc) outTxt

executeToolSync ::
  forall model m.
  (MonadIO m, MonadError LangchainError m) =>
  ReActAgent model m ->
  ToolCall ->
  m Message
executeToolSync agent@ReActAgent {..} tc = do
  let tName = toolCallName tc
      tools = allAgentTools agent
  now <- liftIO getCurrentTime
  dispatchCb agentCallbackManager (OnToolStart tName (toolCallArguments tc) now)
  case find (\t -> toolName t == tName) tools of
    Nothing -> do
      let notFoundMsg = "Error: Tool not found: " <> tName
          err = agentError notFoundMsg (Just "ReActAgent") (Just $ "executeTool:" <> tName)
      handleToolOutcome agent tc (const (pure ())) (Left err) 0
    Just tool -> do
      startTime <- liftIO getCurrentTime
      eRes <- toolExecute tool (toolCallArguments tc)
      endTime <- liftIO getCurrentTime
      let durationMicros = round (realToFrac (diffUTCTime endTime startTime) * 1000000 :: Double) :: Int
      handleToolOutcome agent tc (const (pure ())) eRes durationMicros

executeToolStream ::
  forall model m.
  (MonadIO m, MonadError LangchainError m) =>
  ReActAgent model m ->
  ToolCall ->
  ConduitT () StreamEvent m Message
executeToolStream agent@ReActAgent {..} tc = do
  let tName = toolCallName tc
  now <- liftIO getCurrentTime
  dispatchCb agentCallbackManager (OnToolStart tName (toolCallArguments tc) now)
  yield $ ToolStart reactRunId tName (toolCallArguments tc)
  case find (\st -> toolName (streamingToolDef st) == tName) agentStreamingTools of
    Just sTool -> do
      startTime <- liftIO getCurrentTime
      eRes <- mapOutput (ToolChunk reactRunId tName) (streamingToolExecute sTool (toolCallArguments tc))
      endTime <- liftIO getCurrentTime
      let durationMicros = round (realToFrac (diffUTCTime endTime startTime) * 1000000 :: Double) :: Int
      handleToolOutcome agent tc yield eRes durationMicros
    Nothing ->
      case find (\t -> toolName t == tName) agentTools of
        Nothing -> do
          let notFoundMsg = "Error: Tool not found: " <> tName
              err = agentError notFoundMsg (Just "ReActAgent") (Just $ "executeTool:" <> tName)
          handleToolOutcome agent tc yield (Left err) 0
        Just tool -> do
          startTime <- liftIO getCurrentTime
          eRes <- lift $ toolExecute tool (toolCallArguments tc)
          endTime <- liftIO getCurrentTime
          let durationMicros = round (realToFrac (diffUTCTime endTime startTime) * 1000000 :: Double) :: Int
          handleToolOutcome agent tc yield eRes durationMicros

{- | Execute the full ReAct reasoning loop, returning only the final answer.

For the full reasoning trace, use 'runReActAgentWithTrace'.
For streaming execution, use 'streamReActAgent' or 'runReActAgentStream'.

When the agent has a 'BaseMemory' attached:

1. Existing conversation history is loaded from memory.
2. The user's input messages are appended.
3. After the loop finishes, both the user input and final answer are saved to memory.
-}
runReActAgent ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  -- | Configured ReAct agent instance
  ReActAgent model m ->
  -- | Input user messages for this turn
  [Message] ->
  -- | The final assistant answer message
  m Message
runReActAgent agent userMsgs = traceFinalAnswer <$> runReActAgentWithTrace agent userMsgs

{- | Execute the full ReAct reasoning loop, returning both the final answer
and the complete intermediate reasoning trace.

Tool calls within a single step are executed in sequence within the @m@ monad,
and the observations are fed back to the LLM for the next reasoning step.

The trace includes every 'AgentStep' and the observation messages produced
at each iteration, which is invaluable for debugging and observability.
-}
runReActAgentWithTrace ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  -- | Configured ReAct agent instance
  ReActAgent model m ->
  -- | Input user messages for this turn
  [Message] ->
  -- | Full reasoning trace containing all intermediate steps and final answer
  m AgentTrace
runReActAgentWithTrace agent@ReActAgent {..} userMsgs = do
  initialHistory <- buildInitialHistory agent userMsgs "runReActAgentWithTrace"
  let tools = allAgentTools agent
  trace <-
    runLoopWith
      Nothing
      agent
      (\hist -> reactStep agentModel tools hist agentModelConfig)
      (executeToolSync agent)
      initialHistory
      agentMaxIterations
      []
  persistTurnMemory agent userMsgs (traceFinalAnswer trace)
  pure trace

streamReActAgentTraceWith ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  Maybe CancelToken ->
  ReActAgent model m ->
  [Message] ->
  ConduitT () StreamEvent m AgentTrace
streamReActAgentTraceWith mbTok agent@ReActAgent {..} userMsgs = do
  initialHistory <- lift $ buildInitialHistory agent userMsgs "streamReActAgentWithTrace"
  let tools = allAgentTools agent
  yield $ ChainStart reactRunId "ReActAgent" (toJSON initialHistory)
  trace <-
    runLoopWith
      mbTok
      agent
      (\hist -> reactStepStreamWith mbTok agentModel tools hist agentModelConfig)
      (executeToolStream agent)
      initialHistory
      agentMaxIterations
      []
  cancelled <- maybe (pure False) isCancelled mbTok
  unless cancelled $ do
    lift $ persistTurnMemory agent userMsgs (traceFinalAnswer trace)
    yield $ ChainEnd reactRunId "ReActAgent" (toJSON (traceFinalAnswer trace))
  pure trace

{- | Execute the full ReAct reasoning loop as a Conduit stream of 'StreamEvent's,
returning the complete 'AgentTrace'.

Emits 'ChainStart', followed by 'LLMStart' / 'LLMChunk' / 'LLMEnd' for each
reasoning turn and 'ToolStart' / 'ToolChunk' / 'ToolEnd' / 'ToolErrorEvent' for
each tool invocation, concluding with 'ChainEnd'.
-}
streamReActAgentWithTrace ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  -- | Configured ReAct agent instance
  ReActAgent model m ->
  -- | Input user messages for this turn
  [Message] ->
  -- | Event stream returning the full reasoning trace
  ConduitT () StreamEvent m AgentTrace
streamReActAgentWithTrace = streamReActAgentTraceWith Nothing

{- | Execute the full ReAct reasoning loop as an 'EventStream'.

Suitable for composition with 'collectEvents', 'runStreamWith', or any Conduit sink.
-}
streamReActAgent ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  -- | Configured ReAct agent instance
  ReActAgent model m ->
  -- | Input user messages for this turn
  [Message] ->
  -- | Stream of agent lifecycle, LLM, and tool events
  EventStream m
streamReActAgent agent userMsgs = do
  _ <- streamReActAgentWithTrace agent userMsgs
  pure ()

-- | Outcome of an interruptible streaming ReAct turn.
data InterruptibleResult
  = -- | Turn completed normally with the final assistant message.
    Completed !Message
  | -- | Turn was cancelled via 'CancelToken', returning any partial assistant text emitted.
    Cancelled !Message
  deriving (Eq, Show)

{- | Execute an interruptible streaming ReAct turn using a cooperative 'CancelToken'.

Safe for @LangchainT () IO@ callers without @ResourceT@. When cancelled mid-turn,
upstream streaming stops promptly, conversation memory is left unmodified, and
'Cancelled' returns the partial assistant message streamed so far.
-}
runReActAgentStreamInterruptible ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  CancelToken ->
  ReActAgent model m ->
  [Message] ->
  (StreamEvent -> m ()) ->
  m InterruptibleResult
runReActAgentStreamInterruptible tok agent userMsgs onEvent = do
  cancelledStart <- isCancelled tok
  if cancelledStart
    then pure $ Cancelled (assistantMessage "")
    else do
      chunksRef <- liftIO $ newIORef ([] :: [Text])
      let trackAndForward ev = do
            liftIO $ case ev of
              LLMStart {} -> writeIORef chunksRef []
              LLMChunk _ txt _ | not (T.null txt) -> modifyIORef' chunksRef (txt :)
              LLMEnd _ msg _ -> writeIORef chunksRef [extractMessageText msg]
              _ -> pure ()
            onEvent ev
      trace <-
        runConduit
          ( streamReActAgentTraceWith (Just tok) agent userMsgs
              `fuseUpstream` CL.mapM_ trackAndForward
          )
      cancelledEnd <- isCancelled tok
      if cancelledEnd
        then do
          partialTxt <- liftIO (T.concat . reverse <$> readIORef chunksRef)
          pure $ Cancelled (assistantMessage partialTxt)
        else pure $ Completed (traceFinalAnswer trace)

{- | Execute the ReAct agent in streaming mode, invoking a callback for every
'StreamEvent' as it arrives and returning the final assistant 'Message'.
-}
runReActAgentStream ::
  forall model m.
  (ToolBinder model m, MonadIO m, MonadError LangchainError m) =>
  -- | Configured ReAct agent instance
  ReActAgent model m ->
  -- | Input user messages for this turn
  [Message] ->
  -- | Callback invoked on each 'StreamEvent' in real time
  (StreamEvent -> m ()) ->
  -- | The final assistant answer message
  m Message
runReActAgentStream agent userMsgs onEvent =
  traceFinalAnswer
    <$> runConduit (streamReActAgentWithTrace agent userMsgs `fuseUpstream` CL.mapM_ onEvent)

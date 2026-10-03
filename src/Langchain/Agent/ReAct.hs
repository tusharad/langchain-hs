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

    -- * Execution
  , reactStep
  , runReActAgent
  , runReActAgentWithTrace
  ) where

import Control.Monad (forM, when)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import Langchain.Callback.Manager (CallbackEvent (..), CallbackManager, dispatchEvent)
import Langchain.Core.Error (LangchainError, agentError, errorMessage)
import Langchain.Core.Model
import qualified Langchain.Core.Model.Types as M
import Langchain.Core.Tool
import Langchain.Memory.Core (BaseMemory (..), SomeMemory (..))
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

Holds the LLM model, the effect-polymorphic tools @['Tool' m]@, and optional
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
  case messageToolCalls responseMsg of
    Just tcs@(_ : _) -> pure $ AgentAction responseMsg tcs
    _ -> pure $ AgentFinish responseMsg

{- | Execute the full ReAct reasoning loop, returning only the final answer.

For the full reasoning trace, use 'runReActAgentWithTrace'.

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
runReActAgentWithTrace ReActAgent {..} userMsgs = do
  -- 1. Build initial history: system prompt + memory + user messages
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
        (Just "runReActAgentWithTrace")

  -- 2. Run the reasoning loop
  trace <- go initialHistory agentMaxIterations []

  -- 3. Persist to memory if available
  case agentMemory of
    Nothing -> pure ()
    Just (SomeMemory mem) -> do
      -- Save each user message
      mapM_ (addMessage mem) userMsgs
      -- Save the final answer
      addMessage mem (traceFinalAnswer trace)

  pure trace
  where
    go :: [Message] -> Int -> [(AgentStep, [Message])] -> m AgentTrace
    go history maxIter accSteps
      | maxIter <= 0 = do
          let maxErr =
                "ReAct Agent exceeded maximum iterations ("
                  <> T.pack (show agentMaxIterations)
                  <> ")"
          case agentCallbackManager of
            Nothing -> pure ()
            Just mgr -> do
              now <- liftIO getCurrentTime
              dispatchEvent mgr (OnError "ReActAgent" maxErr now)
          throwError $
            agentError
              maxErr
              (Just "ReActAgent")
              (Just "runReActAgent")
      | otherwise = do
          -- Dispatch OnLLMStart callback before calling model
          llmStartTime <- liftIO getCurrentTime
          case agentCallbackManager of
            Nothing -> pure ()
            Just mgr ->
              dispatchEvent mgr (OnLLMStart "ReActAgent" (map extractMessageText history) llmStartTime)

          -- Invoke the model with error interception for observability
          eStep <-
            (Right <$> reactStep agentModel agentTools history agentModelConfig)
              `catchError` (pure . Left)
          llmEndTime <- liftIO getCurrentTime
          let llmLatency = round (realToFrac (diffUTCTime llmEndTime llmStartTime) * 1000000 :: Double) :: Int
          case eStep of
            Left err -> do
              case agentCallbackManager of
                Nothing -> pure ()
                Just mgr ->
                  dispatchEvent mgr (OnError "ReActAgent" (errorMessage err) llmEndTime)
              throwError err
            Right step -> case step of
              AgentFinish finalMsg -> do
                case agentCallbackManager of
                  Nothing -> pure ()
                  Just mgr ->
                    dispatchEvent mgr (OnLLMEnd "ReActAgent" (extractMessageText finalMsg) llmLatency llmEndTime)
                let allSteps = accSteps ++ [(step, [])]
                pure $ AgentTrace allSteps finalMsg
              AgentAction respMsg tcs -> do
                case agentCallbackManager of
                  Nothing -> pure ()
                  Just mgr ->
                    dispatchEvent mgr (OnLLMEnd "ReActAgent" (extractMessageText respMsg) llmLatency llmEndTime)

                -- Check custom stop condition
                case agentStopCondition of
                  Just cond | cond respMsg -> do
                    let allSteps = accSteps ++ [(step, [])]
                    pure $ AgentTrace allSteps respMsg
                  _ -> do
                    obsMsgs <- forM tcs executeTool
                    let newHistory = history ++ [respMsg] ++ obsMsgs
                        newSteps = accSteps ++ [(step, obsMsgs)]
                    go newHistory (maxIter - 1) newSteps

    executeTool :: ToolCall -> m Message
    executeTool tc = do
      let tName = toolCallName tc
      case agentCallbackManager of
        Nothing -> pure ()
        Just mgr -> do
          now <- liftIO getCurrentTime
          dispatchEvent mgr (OnToolStart tName (toolCallArguments tc) now)

      case find (\t -> toolName t == tName) agentTools of
        Nothing -> do
          let notFoundMsg = "Error: Tool not found: " <> tName
          case agentCallbackManager of
            Nothing -> pure ()
            Just mgr -> do
              now <- liftIO getCurrentTime
              dispatchEvent mgr (OnError tName notFoundMsg now)
          when (agentToolErrorStrategy == FailOnError) $
            throwError $
              agentError notFoundMsg (Just "ReActAgent") (Just $ "executeTool:" <> tName)
          pure $
            (textMessage M.Tool notFoundMsg)
              { M.messageName = Just tName
              , M.messageToolId = Just (toolCallId tc)
              }
        Just tool -> do
          startTime <- liftIO getCurrentTime
          eRes <- toolExecute tool (toolCallArguments tc)
          endTime <- liftIO getCurrentTime
          let durationMicros = round (realToFrac (diffUTCTime endTime startTime) * 1000000 :: Double) :: Int
          case eRes of
            Left err -> do
              let errTxt = "Error executing tool " <> tName <> ": " <> errorMessage err
              case agentCallbackManager of
                Nothing -> pure ()
                Just mgr -> dispatchEvent mgr (OnError tName errTxt endTime)
              when (agentToolErrorStrategy == FailOnError) $
                throwError $
                  agentError errTxt (Just "ReActAgent") (Just $ "executeTool:" <> tName)
              pure $
                (textMessage M.Tool errTxt)
                  { M.messageName = Just tName
                  , M.messageToolId = Just (toolCallId tc)
                  }
            Right outTxt -> do
              case agentCallbackManager of
                Nothing -> pure ()
                Just mgr -> dispatchEvent mgr (OnToolEnd tName outTxt durationMicros endTime)
              pure $
                (textMessage M.Tool outTxt)
                  { M.messageName = Just tName
                  , M.messageToolId = Just (toolCallId tc)
                  }

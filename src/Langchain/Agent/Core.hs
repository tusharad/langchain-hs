{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Langchain.Agent.Core
Description : Core types and abstractions for LangChain agents
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the foundational types and typeclasses for building agents
in langchain-hs. An LLM Agent runs tools in a loop to achieve a goal.
An agent runs until a stop condition is met -
when the model emits a final output or an iteration limit is reached.
-}
module Langchain.Agent.Core
  ( -- * Agent Typeclass
    Agent (..)

    -- * Agent Actions and Results
  , AgentAction (..)
  , AgentFinish (..)
  , AgentStep (..)
  , AgentScratchpad

    -- * Agent State and Configuration
  , AgentState (..)
  , AgentConfig (..)
  , AgentCallbacks (..)
  , defaultAgentConfig
  , defaultAgentCallbacks

    -- * Tool support
  , ToolAcceptingToolCall (..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Langchain.Error (LangchainError, LangchainResult)
import Langchain.LLM.Core (ChatHistory, ToolCall)
import Langchain.Tool.Core

-- | Represents an action that an agent has decided to take.
data AgentAction = AgentAction
  { actionToolCall :: [ToolCall]
  -- ^ tool call
  , actionLog :: Text
  -- ^ Agent's reasoning or thoughts (for debugging/logging)
  , actionMetadata :: Map Text Text
  -- ^ Additional metadata about the action
  }
  deriving (Show, Eq)

-- | Represents the final result when an agent completes its task.
data AgentFinish = AgentFinish
  { agentOutput :: Text
  -- ^ The final answer or result
  , finishMetadata :: Map Text Text
  -- ^ Additional information about the execution
  , finishLog :: Text
  -- ^ Final thoughts or reasoning
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Represents one step in the agent's execution.
data AgentStep = AgentStep
  { stepAction :: AgentAction
  -- ^ The action that was executed
  , stepObservation :: Text
  -- ^ The result/observation from the action
  , stepTimestamp :: UTCTime
  -- ^ When this step occurred
  }
  deriving (Show, Eq)

{- | The scratchpad maintains the history of actions and observations
during agent execution. This helps the agent track what it has done
and plan future actions.
-}
type AgentScratchpad = [AgentStep]

{- | Current state of the agent during execution.

Tracks:
- Chat history with the LLM
- Scratchpad (action-observation pairs)
- Current input being processed
-}
data AgentState = AgentState
  { agentChatHistory :: ChatHistory
  -- ^ Message history with the LLM
  , agentScratchpad :: AgentScratchpad
  -- ^ History of actions and observations
  , agentInput :: Text
  -- ^ Current user input/query
  , agentIterations :: Int
  -- ^ Number of iterations so far
  }
  deriving (Show, Eq, Generic)

data AgentConfig = AgentConfig
  { maxIterations :: Int
  -- ^ Maximum number of agent steps (default: 15)
  , maxExecutionTime :: Maybe Int
  -- ^ Maximum execution time in seconds (Nothing = no limit)
  , verboseLogging :: Bool
  -- ^ Enable verbose logging (default: False)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- | Callbacks for agent events.
Allows hooking into various points in the agent lifecycle.
-}
data AgentCallbacks = AgentCallbacks
  { onAgentStart :: Text -> IO ()
  -- ^ Called when agent starts with the input
  , onAgentAction :: AgentAction -> IO ()
  -- ^ Called before executing an action
  , onAgentObservation :: Text -> IO ()
  -- ^ Called after receiving an observation
  , onAgentFinish :: AgentFinish -> IO ()
  -- ^ Called when agent completes
  , onAgentError :: LangchainError -> IO ()
  -- ^ Called when an error occurs
  , onAgentStep :: AgentStep -> IO ()
  -- ^ Called after each complete step
  }

-- | Tool for Agents
data ToolAcceptingToolCall where
  ToolAcceptingToolCall ::
    ( Tool t
    , Input t ~ ToolCall
    , Output t ~ Text
    ) =>
    t -> ToolAcceptingToolCall

instance Eq ToolAcceptingToolCall where
  (ToolAcceptingToolCall t1) == (ToolAcceptingToolCall t2) = toolName t1 == toolName t2

instance Show ToolAcceptingToolCall where
  show (ToolAcceptingToolCall t) =
    "ToolAcceptingToolCall { name = " ++ show (toolName t) ++ " }"

{- | Core Agent typeclass.

An agent is a system that can plan and execute actions to accomplish a task.
Different agent types (ReAct, Plan-and-Execute, etc.) implement this interface.
-}
class Agent a where
  {- | Plan the next action or finish.

  Given the current state, decide:
  - What action to take next (Left AgentAction), or
  - That the task is complete (Right AgentFinish)
  -}
  plan ::
    a ->
    AgentState ->
    IO (LangchainResult (Either AgentAction AgentFinish))

  -- | Get the tools available to this agent.
  getTools :: a -> [ToolAcceptingToolCall]

  -- | Execute a tool.
  executeTool :: a -> ToolCall -> IO (LangchainResult Text)

  {- | Prepare the agent for execution.
  Initialize any necessary state before starting.
  Default implementation does nothing.
  -}
  initialize :: a -> AgentState -> IO (LangchainResult AgentState)
  initialize _ state = pure $ Right state

  {- | Clean up after agent execution.
  Release resources, save state, etc.
  Default implementation does nothing.
  -}
  finalize :: a -> AgentState -> IO ()
  finalize _ _ = pure ()

  -- | MonadIO version of plan
  planM ::
    MonadIO m =>
    a ->
    AgentState ->
    m (LangchainResult (Either AgentAction AgentFinish))
  planM agent state = liftIO $ plan agent state

  -- | MonadIO version of executeTool
  executeToolM :: MonadIO m => a -> ToolCall -> m (LangchainResult Text)
  executeToolM a i = liftIO $ executeTool a i

  -- | MonadIO version of initialize
  initializeM ::
    MonadIO m =>
    a ->
    AgentState ->
    m (LangchainResult AgentState)
  initializeM agent state = liftIO $ initialize agent state

  -- | MonadIO version of finalize
  finalizeM :: MonadIO m => a -> AgentState -> m ()
  finalizeM agent state = liftIO $ finalize agent state

{- | Default agent configuration.

Sensible defaults:
- 15 max iterations
- No time limit
- Don't return intermediate steps
- Handle parsing errors
- No verbose logging
-}
defaultAgentConfig :: AgentConfig
defaultAgentConfig =
  AgentConfig
    { maxIterations = 15
    , maxExecutionTime = Nothing
    , verboseLogging = False
    }

{- | Default agent callbacks (all no-ops).
Useful as a starting point for custom callbacks.
-}
defaultAgentCallbacks :: AgentCallbacks
defaultAgentCallbacks =
  AgentCallbacks
    { onAgentStart = \_ -> pure ()
    , onAgentAction = \_ -> pure ()
    , onAgentObservation = \_ -> pure ()
    , onAgentFinish = \_ -> pure ()
    , onAgentError = \_ -> pure ()
    , onAgentStep = \_ -> pure ()
    }

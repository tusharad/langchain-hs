{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Agent.Core
Description : Core types and abstractions for LangChain agents
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the foundational types and typeclasses for building agents
in langchain-hs. Agents are autonomous systems that use language models to determine
which actions to take and in what order.

The design is inspired by:
- LangChain Python: https://docs.langchain.com/oss/python/langchain/agents
- OpenAI Agents: https://openai.github.io/openai-agents-python/

Key concepts:

* 'Agent': The core decision-making component that uses an LLM to choose actions
* 'AgentAction': Represents an action to be executed (tool call)
* 'AgentFinish': Represents the final output when the agent completes its task
* 'AgentStep': Combines an action with its observation (result)
* 'AgentExecutor': Orchestrates the agent execution loop

Example usage:

@
import Langchain.Agent.Core
import Langchain.Agent.ReAct
import Langchain.Tool.Calculator

main :: IO ()
main = do
  let agent = createReActAgent llm tools
  result <- runAgent agent "What is 25 * 4?"
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right finish -> putStrLn $ agentOutput finish
@
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

    -- * Tool Integration
  , ToolDescriptor (..)
  , ToolResult (..)
  , ToolExecutor
  , AnyTool (..)
  , wrapTool
  , executeAnyTool

    -- * Agent Events
  , AgentEvent (..)

    -- * Utilities
  , formatScratchpad
  , formatToolDescriptors
  , toolToDescriptor
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Langchain.Error (LangchainError, LangchainResult, toolError)
import Langchain.LLM.Core (ChatHistory)
import Langchain.Tool.Core (Tool (..))

{- | Represents an action that an agent has decided to take.

An action consists of:
- A tool to execute
- Input to provide to that tool
- Optional reasoning/thoughts that led to this decision
-}
data AgentAction = AgentAction
  { actionTool :: Text
  -- ^ Name of the tool to execute
  , actionToolInput :: Text
  -- ^ Input to provide to the tool
  , actionLog :: Text
  -- ^ Agent's reasoning or thoughts (for debugging/logging)
  , actionMetadata :: Map Text Text
  -- ^ Additional metadata about the action
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- | Represents the final result when an agent completes its task.

Contains:
- The final output to return to the user
- Metadata about the execution
-}
data AgentFinish = AgentFinish
  { agentOutput :: Text
  -- ^ The final answer or result
  , finishMetadata :: Map Text Text
  -- ^ Additional information about the execution
  , finishLog :: Text
  -- ^ Final thoughts or reasoning
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- | Represents one step in the agent's execution.

Each step consists of:
- The action that was taken
- The observation (result) from executing that action
-}
data AgentStep = AgentStep
  { stepAction :: AgentAction
  -- ^ The action that was executed
  , stepObservation :: Text
  -- ^ The result/observation from the action
  , stepTimestamp :: UTCTime
  -- ^ When this step occurred
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- | The scratchpad maintains the history of actions and observations
during agent execution. This helps the agent track what it has done
and plan future actions.
-}
type AgentScratchpad = [AgentStep]

{- | Describes a tool available to the agent.

This is a simplified representation that can be passed to the LLM
to help it understand what tools are available.
-}
data ToolDescriptor = ToolDescriptor
  { toolDescName :: Text
  -- ^ Tool name
  , toolDescDescription :: Text
  -- ^ What the tool does
  , toolDescInputSchema :: Maybe A.Value
  -- ^ Optional JSON schema for tool input
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- | Result of executing a tool.

Can be either successful with output, or an error.
-}
data ToolResult
  = ToolSuccess Text
  | ToolFailure Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- | Function type for executing tools.

Takes a tool name and input, returns the result.
-}
type ToolExecutor = Text -> Text -> IO (LangchainResult Text)

{- | Wrapper for any Tool instance that can be used by agents.

Converts Tool typeclass instances to text-based execution.
-}
data AnyTool = forall t. Tool t => AnyTool
  { anyToolInstance :: t
  , anyToolInputParser :: Text -> Maybe (Input t)
  , anyToolOutputFormatter :: Output t -> Text
  }

-- | Convert a Tool instance to AnyTool with custom parsers.
wrapTool ::
  Tool t =>
  t ->
  (Text -> Maybe (Input t)) ->
  (Output t -> Text) ->
  AnyTool
wrapTool tool parser formatter =
  AnyTool
    { anyToolInstance = tool
    , anyToolInputParser = parser
    , anyToolOutputFormatter = formatter
    }

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

{- | Configuration for agent execution.

Controls:
- Maximum number of iterations before stopping
- Maximum execution time
- Whether to return intermediate steps
- Error handling behavior
-}
data AgentConfig = AgentConfig
  { maxIterations :: Int
  -- ^ Maximum number of agent steps (default: 15)
  , maxExecutionTime :: Maybe Int
  -- ^ Maximum execution time in seconds (Nothing = no limit)
  , returnIntermediateSteps :: Bool
  -- ^ Whether to include intermediate steps in result (default: False)
  , handleParsingErrors :: Bool
  -- ^ Whether to recover from parsing errors (default: True)
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

{- | Events that occur during agent execution.

Used for logging and monitoring.
-}
data AgentEvent
  = AgentStarted Text UTCTime
  | AgentActionPlanned AgentAction UTCTime
  | AgentToolExecuted AgentStep UTCTime
  | AgentFinished AgentFinish UTCTime
  | AgentErrorOccurred LangchainError UTCTime
  | AgentIteration Int UTCTime
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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

  {- | Get the tools available to this agent.

  Returns descriptions of all tools the agent can use.
  -}
  getTools :: a -> [ToolDescriptor]

  {- | Execute a tool.

  Takes the tool name and input, returns the observation.
  -}
  executeTool ::
    a ->
    Text ->
    Text ->
    IO (LangchainResult Text)

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
  executeToolM ::
    MonadIO m =>
    a ->
    Text ->
    Text ->
    m (LangchainResult Text)
  executeToolM a t i = liftIO $ executeTool a t i

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
    , returnIntermediateSteps = False
    , handleParsingErrors = True
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

{- | Format the scratchpad for display.

Converts the action-observation history into a readable format
that can be shown to the LLM.
-}
formatScratchpad :: AgentScratchpad -> Text
formatScratchpad steps =
  T.intercalate "\n\n" $ map formatStep steps
  where
    formatStep :: AgentStep -> Text
    formatStep AgentStep {..} =
      let AgentAction {..} = stepAction
       in T.unlines
            [ "Thought: " <> actionLog
            , "Action: " <> actionTool
            , "Action Input: " <> actionToolInput
            , "Observation: " <> stepObservation
            ]

{- | Format tool descriptors for the LLM.

Creates a readable description of available tools.
-}
formatToolDescriptors :: [ToolDescriptor] -> Text
formatToolDescriptors tools =
  T.intercalate "\n" $ map formatTool tools
  where
    formatTool :: ToolDescriptor -> Text
    formatTool ToolDescriptor {..} =
      "- " <> toolDescName <> ": " <> toolDescDescription

-- | Convert AnyTool to ToolDescriptor.
toolToDescriptor :: AnyTool -> ToolDescriptor
toolToDescriptor (AnyTool tool _ _) =
  ToolDescriptor
    { toolDescName = toolName tool
    , toolDescDescription = toolDescription tool
    , toolDescInputSchema = Nothing
    }

-- | Execute an AnyTool with text input.
executeAnyTool :: AnyTool -> Text -> IO (LangchainResult Text)
executeAnyTool (AnyTool tool parser formatter) input =
  case parser input of
    Nothing ->
      return $
        Left $
          toolError
            ("Failed to parse input for tool: " <> toolName tool)
            (Just $ toolName tool)
            Nothing
    Just parsedInput -> do
      output <- runTool tool parsedInput
      return $ Right $ formatter output

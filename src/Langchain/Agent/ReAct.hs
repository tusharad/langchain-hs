{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Agent.ReAct
Description : ReAct (Reasoning + Acting) agent implementation
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module implements the ReAct (Reasoning + Acting) agent pattern.
ReAct combines reasoning traces and task-specific actions in an interleaved manner.
-}
module Langchain.Agent.ReAct
  ( -- * Agent Creation
    ReActAgent (..)
  , createReActAgent
  , createReActAgentWithPrompt

    -- * Prompt Templates
  , reActSystemPrompt
  ) where

import Control.Monad.Trans.Except
import Data.List (find)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agent.Core
import qualified Langchain.Error as Error
import Langchain.LLM.Core
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Tool.Core

{- | ReAct agent.

Arguments:
- llm: The language model
- llmParams: The language model parameters
- systemPrompt: The system prompt
- maxThinkingSteps: The maximum number of thinking steps before forcing action
- tools: The tools available to the agent.
-}
data ReActAgent llm = ReActAgent
  { reactLLM :: llm
  -- ^ The language model for reasoning
  , reactLLMParams :: Maybe (LLMParams llm)
  -- ^ the llm params for language model
  , reactSystemPrompt :: Text
  -- ^ System prompt template
  , reactMaxThinkingSteps :: Int
  -- ^ Maximum consecutive thinking steps before forcing action (default: 3)
  , reactTools :: [ToolAcceptingToolCall]
  }

{- | Create a ReAct agent.

Arguments:
- llm: The language model
- llmParams: The language model parameters
- tools: The tools available to the agent

Important:
- It is user's responsibility to wrap the tools into ToolAcceptingToolCall.
- It is user's responsibility to pass tool_calls as part of LLMParams.
- The tool_calls shall be same as the reactTools (ToolAcceptingToolCall) list.
-}
createReActAgent ::
  -- | The language model
  llm ->
  -- | The language model parameters
  Maybe (LLMParams llm) ->
  -- | The tools available to the agent
  [ToolAcceptingToolCall] ->
  -- | The ReAct agent
  ReActAgent llm
createReActAgent llm mbLlmParams tools =
  ReActAgent
    { reactLLM = llm
    , reactLLMParams = mbLlmParams
    , reactSystemPrompt = reActSystemPrompt
    , reactMaxThinkingSteps = 3
    , reactTools = tools
    }

-- | Create a ReAct agent with a custom system prompt.
createReActAgentWithPrompt ::
  -- | The language model
  llm ->
  -- | The language model parameters
  Maybe (LLMParams llm) ->
  -- | The tools available to the agent
  [ToolAcceptingToolCall] ->
  -- | The custom system prompt
  Text ->
  -- | The ReAct agent
  ReActAgent llm
createReActAgentWithPrompt llm mbLlmParams tools prompt =
  ReActAgent
    { reactLLM = llm
    , reactLLMParams = mbLlmParams
    , reactSystemPrompt = prompt
    , reactMaxThinkingSteps = 3
    , reactTools = tools
    }

-- | Default system prompt for the ReAct agent.
reActSystemPrompt :: Text
reActSystemPrompt =
  "You are a helpful AI assistant that uses tools to answer user questions."

instance LLM llm => Agent (ReActAgent llm) where
  plan agent state = do
    let llm = reactLLM agent
        mbParams = reactLLMParams agent
    -- Get messages from memory - use case to handle existential type
    case agentMemory state of
      SomeMemory mem -> runExceptT $ do
        msgs <- ExceptT $ messages mem
        respMsg <- ExceptT $ chat llm msgs mbParams
        case toolCalls (messageData respMsg) of
          Nothing -> do
            -- No tool calls requested. Assume content as the final result
            pure $
              Done $
                AgentFinish
                  { agentOutput = content respMsg
                  , finishMetadata = Map.empty -- TODO: Add stuff from state
                  , finishLog = content respMsg
                  }
          Just toolCallList -> do
            pure $
              Continue
                AgentAction
                  { actionToolCall = toolCallList
                  , actionLog = content respMsg
                  , actionMetadata = Map.empty -- TODO: what to add here?
                  }

  getTools = reactTools

  executeTool agent toolCall = do
    let tools = getTools agent
    let inputFunctionName = toolFunctionName (toolCallFunction toolCall)
    case find (\(ToolAcceptingToolCall t) -> toolName t == inputFunctionName) tools of
      Nothing ->
        pure $
          Left $
            Error.fromString $
              "Cannot find tool with name: "
                <> T.unpack inputFunctionName
      Just (ToolAcceptingToolCall selectedTool) -> Right <$> runTool selectedTool toolCall

  initialize agent state = do
    let sysPrompt = reactSystemPrompt agent
        userInput = agentInput state
        sysMsg = defaultMessage {role = System, content = sysPrompt}
        userMsg = defaultMessage {role = User, content = userInput}
    case agentMemory state of
      SomeMemory mem -> runExceptT $ do
        memWithSys <- ExceptT $ addMessage mem sysMsg
        memWithUser <- ExceptT $ addMessage memWithSys userMsg
        pure
          AgentState
            { agentMemory = SomeMemory memWithUser
            , agentInput = userInput
            , agentIterations = 0
            }

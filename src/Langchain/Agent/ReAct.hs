{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Agent.ReAct
Description : ReAct (Reasoning + Acting) agent implementation
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module implements the ReAct (Reasoning and Acting) agent pattern.
ReAct combines reasoning traces and task-specific actions in an interleaved manner,
allowing the agent to:

1. Think about what to do next
2. Take an action
3. Observe the result
4. Repeat until the task is complete

The ReAct pattern is based on the paper:
\"ReAct: Synergizing Reasoning and Acting in Language Models\"
https://arxiv.org/abs/2210.03629
-}
module Langchain.Agent.ReAct
  ( -- * Agent Creation
    ReActAgent (..)
  , createReActAgent
  , createReActAgentWithPrompt

    -- * Prompt Templates
  , reActSystemPrompt
  ) where

import Data.List (find)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agent.Core
import qualified Langchain.Error as Error
import Langchain.LLM.Core
import Langchain.Tool.Core

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

createReActAgent ::
  llm -> Maybe (LLMParams llm) -> [ToolAcceptingToolCall] -> ReActAgent llm
createReActAgent llm mbLlmParams tools =
  ReActAgent
    { reactLLM = llm
    , reactLLMParams = mbLlmParams
    , reactSystemPrompt = reActSystemPrompt
    , reactMaxThinkingSteps = 3
    , reactTools = tools
    }

createReActAgentWithPrompt ::
  llm ->
  Maybe (LLMParams llm) ->
  [ToolAcceptingToolCall] ->
  Text ->
  ReActAgent llm
createReActAgentWithPrompt llm mbLlmParams tools prompt =
  ReActAgent
    { reactLLM = llm
    , reactLLMParams = mbLlmParams
    , reactSystemPrompt = prompt
    , reactMaxThinkingSteps = 3
    , reactTools = tools
    }

reActSystemPrompt :: Text
reActSystemPrompt =
  "You are a helpful AI assistant that uses tools to answer user questions."

instance LLM llm => Agent (ReActAgent llm) where
  plan agent state = do
    let llm = reactLLM agent
        mbParams = reactLLMParams agent
    let msgs = agentChatHistory state
    eRes <- chat llm msgs mbParams
    case eRes of
      Left err -> pure $ Left err
      Right respMsg -> do
        case toolCalls (messageData respMsg) of
          Nothing -> do
            -- No tool calls requested. Assume content as the final result
            pure $
              Right
                ( Right $
                    AgentFinish
                      { agentOutput = content respMsg
                      , finishMetadata = Map.empty -- TODO: Add stuff from state
                      , finishLog = content respMsg
                      }
                )
          Just toolCallList -> do
            pure $
              Right
                ( Left
                    AgentAction
                      { actionToolCall = toolCallList
                      , actionLog = content respMsg
                      , actionMetadata = Map.empty -- TODO: what to add here?
                      }
                )

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
    pure $
      Right
        AgentState
          { agentChatHistory =
              NE.fromList
                [ defaultMessage
                    { role = System
                    , content = sysPrompt
                    }
                , defaultMessage
                    { content = userInput
                    }
                ]
          , agentInput = userInput
          , agentIterations = 0
          }
    where
      sysPrompt = reactSystemPrompt agent
      userInput = agentInput state

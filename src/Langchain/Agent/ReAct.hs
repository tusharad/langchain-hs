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

Example usage:

@
import Langchain.Agent.ReAct
import Langchain.Agent.Executor
import Langchain.Tool.Calculator
import Langchain.Tool.WebScraper
import Langchain.LLM.Ollama

main :: IO ()
main = do
  let llm = Ollama "llama3.2" []
      tools = [calculatorTool, webScraperTool]
      agent = createReActAgent llm tools
      config = defaultAgentConfig { maxIterations = 10 }

  result <- runAgentExecutor agent config defaultAgentCallbacks
    "What is the population of Tokyo? Search online."

  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right execResult -> putStrLn $ agentOutput $ executionFinish execResult
@
-}
module Langchain.Agent.ReAct
  ( -- * Agent Creation
    ReActAgent (..)
  , createReActAgent
  , createReActAgentWithPrompt

    -- * Prompt Templates
  , reActPromptTemplate
  , reActSystemPrompt
  , formatReActPrompt

    -- * Output Parsing
  , parseReActOutput
  , ReActOutput (..)
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agent.Core
import Langchain.Error
  ( LangchainResult
  , agentError
  , parsingError
  )
import Langchain.LLM.Core (LLM, Message (..), Role (..), chat, defaultMessageData)
import Langchain.Tool.Core (toolName)

{- | ReAct agent configuration.

Contains:
- The LLM to use for reasoning
- Available tools
- System prompt (optional custom prompt)
-}
data ReActAgent llm = ReActAgent
  { reactLLM :: llm
  -- ^ The language model for reasoning
  , reactTools :: [AnyTool]
  -- ^ Available tools
  , reactSystemPrompt :: Text
  -- ^ System prompt template
  , reactMaxThinkingSteps :: Int
  -- ^ Maximum consecutive thinking steps before forcing action (default: 3)
  }

{- | Output from parsing the LLM's response.

Can be:
- Thought: The agent is thinking/reasoning
- Action: The agent wants to execute a tool
- FinalAnswer: The agent has completed the task
-}
data ReActOutput
  = ReActThought Text
  | ReActAction Text Text -- tool name, input
  | ReActFinalAnswer Text
  deriving (Show, Eq)

{- | Create a ReAct agent with default prompt.

Parameters:
- LLM instance
- List of tools

Returns a configured ReAct agent.
-}
createReActAgent :: llm -> [AnyTool] -> ReActAgent llm
createReActAgent llm tools =
  ReActAgent
    { reactLLM = llm
    , reactTools = tools
    , reactSystemPrompt = reActSystemPrompt
    , reactMaxThinkingSteps = 3
    }

{- | Create a ReAct agent with custom prompt.

Allows customization of the system prompt.
-}
createReActAgentWithPrompt ::
  llm ->
  [AnyTool] ->
  Text ->
  ReActAgent llm
createReActAgentWithPrompt llm tools prompt =
  ReActAgent
    { reactLLM = llm
    , reactTools = tools
    , reactSystemPrompt = prompt
    , reactMaxThinkingSteps = 3
    }

{- | Default ReAct system prompt.

Instructs the LLM on how to use the ReAct format.
-}
reActSystemPrompt :: Text
reActSystemPrompt =
  T.unlines
    [ "You are a helpful AI assistant that uses tools to answer user questions."
    , "You can use the following tools:"
    , "{tools}"
    , "When given a user question, if you want to make a tool call, use the following format:"
    , ""
    , "Thought: <your thought process>"
    , "Action: <tool name>"
    , "Action Input: <input to the tool>"
    , ""
    , "The format must be strictly followed. It should only be Thought, Action, and Action Input when taking an action."
    , "Only respond with Though, Action and Action Input and nothing else."
    , "After receiving the tool's output, continue your reasoning."
    , "When you have the final answer, use the format:"
    , ""
    , "Final Answer: <your final answer to the user>"
    , "Provide final answers only when you are certain and have completed all necessary tool calls."
    , "Do not make up tool calls or actions that are not in the provided tool list."
    , "If you are unsure about the answer, use the tools to find more information."
    , ""
    , "User Question: {input}"
    , "Below is the history of your previous thoughts and actions, you can make final answer or take another action, if any:"
    , "{scratchpad}"
    ]

{- | Format the ReAct prompt with tools and input.

Replaces placeholders in the template with actual values.
-}
formatReActPrompt :: Text -> [ToolDescriptor] -> AgentScratchpad -> Text -> Text
formatReActPrompt template tools scratchpad input =
  T.replace "{tools}" toolsDesc $
    T.replace "{tool_names}" toolNames $
      T.replace "{input}" input $
        T.replace
          "{scratchpad}"
          scratchpadText
          template
  where
    toolsDesc = formatToolDescriptors tools
    toolNames = T.intercalate ", " $ map toolDescName tools
    scratchpadText =
      if null scratchpad
        then ""
        else "\n" <> formatScratchpad scratchpad <> "\n"

parseReActOutput :: Text -> LangchainResult ReActOutput
parseReActOutput llmResponse = do
  let stripped = T.strip llmResponse
  case findActionAndActionInput stripped of
    Just (tool, input) -> Right $ ReActAction tool input
    Nothing -> do
      case findFinalAnswer stripped of
        Just answer -> Right $ ReActFinalAnswer answer
        Nothing -> do
          case findThought stripped of
            Just thought -> Right $ ReActThought thought
            Nothing ->
              Left $
                parsingError
                  "Could not parse ReAct output: "
                  (Just stripped)
                  Nothing

findThought :: Text -> Maybe Text
findThought text = do
  if "Thought:" `T.isInfixOf` text
    then
      let thought = T.strip $ T.drop (T.length "Thought:") (snd $ "Thought:" `T.breakOn` text)
       in Just thought
    else do
      let lowered = T.toLower text
      if "thought:" `T.isInfixOf` lowered
        then
          let thought = T.strip $ T.drop (T.length "thought:") (snd $ "thought:" `T.breakOn` lowered)
           in Just thought
        else Nothing

findActionAndActionInput :: Text -> Maybe (Text, Text)
findActionAndActionInput text = do
  if "Action:" `T.isInfixOf` text
    then
      let afterAction = T.strip $ T.drop (T.length "Action:") (snd $ "Action:" `T.breakOn` text)
       in case T.breakOn "Action Input:" afterAction of
            (toolName_, rest) -> do
              let input = T.strip $ T.drop (T.length "Action Input:") rest
              let inputClean =
                    case T.breakOn "\n" input of
                      (inp, _) -> T.strip inp
              Just (T.strip toolName_, inputClean)
    else do
      let lowered = T.toLower text
      if "action:" `T.isInfixOf` lowered
        then
          let afterAction = T.strip $ T.drop (T.length "action:") (snd $ "action:" `T.breakOn` lowered)
           in case T.breakOn "action input:" afterAction of
                (toolName_, rest) -> do
                  let input = T.strip $ T.drop (T.length "action input:") rest
                  let inputClean =
                        case T.breakOn "\n" input of
                          (inp, _) -> T.strip inp
                  Just (T.strip toolName_, inputClean)
        else Nothing

findFinalAnswer :: Text -> Maybe Text
findFinalAnswer text = do
  if "Final Answer:" `T.isInfixOf` text
    then
      let answer = T.strip $ T.drop (T.length "Final Answer:") (snd $ "Final Answer:" `T.breakOn` text)
       in Just answer
    else do
      let lowered = T.toLower text
      if "final answer:" `T.isInfixOf` lowered
        then
          let answer = T.strip $ T.drop (T.length "final answer:") (snd $ "final answer:" `T.breakOn` lowered)
           in Just answer
        else Nothing

-- | ReAct agent implementation of the Agent typeclass.
instance LLM llm => Agent (ReActAgent llm) where
  plan agent state = do
    let tools = getTools agent
        prompt =
          formatReActPrompt
            (reactSystemPrompt agent)
            tools
            (agentScratchpad state)
            (agentInput state)

    -- Build message for LLM
    let userMsg =
          Message
            { role = User
            , content = prompt
            , messageData = defaultMessageData
            }
        messages = NE.fromList [userMsg]

    -- Get LLM response
    eResponse <- chat (reactLLM agent) messages Nothing -- TODO: pass LLM params instead of Nothing
    case eResponse of
      Left err -> return $ Left err
      Right responseMsg -> do
        let responseText = content responseMsg
        -- Parse the response
        case parseReActOutput responseText of
          Left parseErr -> return $ Left parseErr
          Right (ReActThought thought) -> do
            -- Agent is thinking - this shouldn't directly become an action
            -- Instead, we should prompt it again or treat it as incomplete
            -- For now, return an error asking for an action
            return $
              Left $
                agentError
                  ("Agent only provided thought without action: " <> thought)
                  Nothing
                  Nothing
          Right (ReActAction toolName_ toolInput) -> do
            -- Agent wants to execute a tool
            let action =
                  AgentAction
                    { actionTool = toolName_
                    , actionToolInput = toolInput
                    , actionLog = responseText
                    , actionMetadata = Map.empty
                    }
            return $ Right $ Left action
          Right (ReActFinalAnswer answer) -> do
            -- Agent has finished
            let finish =
                  AgentFinish
                    { agentOutput = answer
                    , finishMetadata = Map.empty
                    , finishLog = responseText
                    }
            return $ Right $ Right finish

  getTools agent = map toolToDescriptor (reactTools agent)

  executeTool agent name input = do
    case findTool name (reactTools agent) of
      Nothing ->
        return $
          Left $
            agentError
              ("Tool not found: " <> name)
              (Just name)
              (Just "executeTool")
      Just anyTool -> executeAnyTool anyTool input
    where
      findTool :: Text -> [AnyTool] -> Maybe AnyTool
      findTool targetName tools =
        let matches = filter (\(AnyTool t _ _) -> toolName t == targetName) tools
         in case matches of
              (t : _) -> Just t
              [] -> Nothing

{- | Build ReAct prompt template.

Creates the full prompt by combining:
- System instructions
- Tool descriptions
- Scratchpad (previous steps)
- Current input
-}
reActPromptTemplate :: [ToolDescriptor] -> AgentScratchpad -> Text -> Text
reActPromptTemplate = formatReActPrompt reActSystemPrompt

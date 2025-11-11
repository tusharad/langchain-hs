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
    [ "You are a helpful AI assistant that uses tools to accomplish tasks."
    , ""
    , "You have access to the following tools:"
    , "{tools}"
    , ""
    , "Use the following format:"
    , ""
    , "Question: the input question you must answer"
    , "Thought: you should always think about what to do"
    , "Action: the action to take, should be one of [{tool_names}]"
    , "Action Input: the input to the action"
    , "Observation: the result of the action"
    , "... (this Thought/Action/Action Input/Observation can repeat N times)"
    , "Thought: I now know the final answer"
    , "Final Answer: the final answer to the original input question"
    , ""
    , "Important:"
    , "- Always start with 'Thought:' to reason about what to do"
    , "- Use 'Action:' only when you need to use a tool"
    , "- Use 'Final Answer:' only when you have the complete answer"
    , "- Be precise with tool names and inputs"
    , ""
    , "Begin!"
    , "{scratchpad}"
    , "Question: {input}"
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

{- | Parse the LLM's output in ReAct format.

Looks for patterns:
- "Thought: ..." -> ReActThought
- "Action: ... Action Input: ..." -> ReActAction
- "Final Answer: ..." -> ReActFinalAnswer
-}
parseReActOutput :: Text -> LangchainResult ReActOutput
parseReActOutput output =
  let stripped = T.strip output
   in -- Try to parse Final Answer first
      if "Final Answer:" `T.isInfixOf` stripped
        then do
          let answer = T.strip $ T.drop (T.length "Final Answer:") (snd $ "Final Answer:" `T.breakOn` stripped)
           in Right $ ReActFinalAnswer answer
        else
          if "final answer:" `T.isInfixOf` T.toLower stripped
            then
              let answer = T.strip $ T.drop (T.length "final answer:") (snd $ "final answer:" `T.breakOn` stripped)
               in Right $ ReActFinalAnswer answer
            else -- Try to parse Action/Action Input
              case parseAction stripped of
                Just (tool, input) -> Right $ ReActAction tool input
                Nothing ->
                  -- Try to parse Thought
                  case parseThought stripped of
                    Just thought -> Right $ ReActThought thought
                    Nothing ->
                      Left $
                        parsingError
                          ("Could not parse ReAct output: " <> output)
                          Nothing
                          Nothing

-- | Parse "Action: tool Action Input: input" pattern.
parseAction :: Text -> Maybe (Text, Text)
parseAction text = do
  -- Find "Action:"
  let text' = T.strip text
  actionPrefix <- findInfix ["Action:", "action:"] text'
  let afterAction = T.strip $ T.drop (T.length actionPrefix) text'

  -- Extract tool name (everything before Action Input)
  case T.breakOn "Action Input:" afterAction of
    (_, "") -> case T.breakOn "action input:" afterAction of
      (_, "") -> Nothing -- No Action Input found
      (toolName_, rest) ->
        let input = T.strip $ T.drop (T.length "action input:") rest
         in Just (T.strip toolName_, input)
    (toolName_, rest) ->
      let input = T.strip $ T.drop (T.length "Action Input:") rest
       in Just (T.strip toolName_, input)

-- | Parse "Thought: ..." pattern.
parseThought :: Text -> Maybe Text
parseThought text = do
  let text' = T.strip text
  prefix <- findInfix ["Thought:", "thought:"] text'
  let thought = T.strip $ T.drop (T.length prefix) text'
  if T.null thought
    then Nothing
    else Just thought

-- | Find the first matching infix from a list.
findInfix :: [Text] -> Text -> Maybe Text
findInfix [] _ = Nothing
findInfix (p : ps) text = if p `T.isInfixOf` text then Just (snd $ p `T.breakOn` text) else findInfix ps text

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
    eResponse <- chat (reactLLM agent) messages Nothing
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

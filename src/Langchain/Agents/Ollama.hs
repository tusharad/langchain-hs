{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agents.Ollama
Description : Ollama agent implementation with native tool calling support
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides an agent implementation that leverages Ollama's native tool calling
capabilities. The agent can use multiple tools to gather information and answer questions,
making decisions about when to use tools and when to provide a final answer.

Important: Not every Ollama model supports tool calling. It is the caller's responsibility
to use a model that has tool calling capabilities (e.g., llama3.1, llama3.2).

Example usage:

@
import Langchain.Agents.Ollama
import Langchain.Memory.Core (WindowBufferMemory(..))
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = do
  -- Create agent with default tools (WebScraper and Wikipedia)
  let agent = OllamaAgent
        { agentModelName = "llama3.2"
        , ollamaAvailableTools = defaultOllamaTools
        , agentSystemPrompt = defaultPrompt defaultOllamaTools
        }

  -- Create agent state with memory
  let initialState = AgentState
        { agentMemory = WindowBufferMemory 10 initialMessages
        , agentToolResults = []
        , agentSteps = []
        }

  -- Run the agent
  result <- runAgent agent initialState "What is the latest news about AI?"
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right finish -> putStrLn $ "Result: " ++ show (returnValues finish)
@
-}
module Langchain.Agents.Ollama
  ( OllamaAgent (..)
  , defaultOllamaTools
  , defaultPrompt
  ) where

import Control.Monad.Trans.Except
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as HM
import Data.Maybe (listToMaybe)
import Data.Ollama.Chat
  ( FunctionDef (..)
  , FunctionParameters (..)
  , InputTool (..)
  )
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agents.Core
import Langchain.LLM.Core
import Langchain.LLM.Ollama (Ollama (..))
import qualified Langchain.LLM.Ollama as LLMOllama
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.PromptTemplate
import Langchain.Tool.Core
import Langchain.Tool.WebScraper
import Langchain.Tool.WikipediaTool (defaultWikipediaTool)

{- | An Agent that uses Ollama's native tool calling
| Please note, not every model supports tool calling.
| Using the model that supports tool_calling is the responsibility of the caller.
-}
data OllamaAgent = OllamaAgent
  { agentModelName :: Text
  , ollamaAvailableTools :: [AnyTool]
  , agentSystemPrompt :: Text
  }

defaultOllamaTools :: [AnyTool]
defaultOllamaTools =
  [ customAnyTool WebScraper id (either T.pack id)
  , customAnyTool defaultWikipediaTool id id
  ]

defaultPrompt :: [AnyTool] -> Text
defaultPrompt tools =
  T.unlines
    [ "You have access to the following tools:"
    , T.unlines (map formatToolForPrompt tools)
    , "Use these tools when you need to gather additional "
        <> "information to answer questions."
    , "Always provide comprehensive and helpful responses."
    , "You can call tools as many times as you want."
    , "Keep calling tools till you are certain about the information"
    ]

formatToolForPrompt :: AnyTool -> Text
formatToolForPrompt (AnyTool tool _ _) =
  "- " <> toolName tool <> ": " <> toolDescription tool

ollamaWebScraperTool :: InputTool
ollamaWebScraperTool =
  InputTool
    { toolType = "function"
    , function =
        FunctionDef
          { functionName = "webScraper"
          , functionDescription =
              Just "Scrapes content from a webpage. Provide a valid URL."
          , functionParameters =
              Just $
                FunctionParameters
                  { parameterType = "object"
                  , requiredParams = Just ["url"]
                  , parameterProperties =
                      Just $
                        HM.fromList
                          [
                            ( "url"
                            , FunctionParameters "string" Nothing Nothing Nothing
                            )
                          ]
                  , additionalProperties = Just False
                  }
          , functionStrict = Nothing
          }
    }

-- | Wikipedia search tool definition for Ollama
ollamaWikiSearchTool :: InputTool
ollamaWikiSearchTool =
  InputTool
    { toolType = "function"
    , function =
        FunctionDef
          { functionName = "searchWiki"
          , functionDescription = Just "Search Wikipedia for information"
          , functionParameters =
              Just $
                FunctionParameters
                  { parameterType = "object"
                  , requiredParams = Just ["query"]
                  , parameterProperties =
                      Just $
                        HM.fromList
                          [
                            ( "query"
                            , FunctionParameters
                                "string"
                                Nothing
                                Nothing
                                Nothing
                            )
                          ]
                  , additionalProperties = Just False
                  }
          , functionStrict = Nothing
          }
    }

instance Agent OllamaAgent where
  planNextAction OllamaAgent {..} AgentState {..} =
    -- If Tool call exists, then continue else consider finished
    runExceptT $ do
      msgs <- ExceptT $ messages agentMemory
      let ollama = Ollama agentModelName []
          toolDefs = map anyToolToInputTool ollamaAvailableTools
          params =
            LLMOllama.defaultOllamaParams
              { LLMOllama.tools = Just toolDefs
              }
      msg <- ExceptT $ chat ollama msgs (Just params)
      case toolCalls (messageData msg) of
        Nothing ->
          -- No tool calls, this is a final answer
          let finalAnswer = content msg
              finishResult =
                AgentFinish
                  { returnValues = HM.singleton "output" finalAnswer
                  , finishLog = finalAnswer
                  }
           in return $ Finish finishResult
        Just callList -> do
          case listToMaybe callList of
            Nothing -> do
              let finalAnswer = content msg
                  finishResult =
                    AgentFinish
                      { returnValues = HM.singleton "output" finalAnswer
                      , finishLog = finalAnswer
                      }
               in return $ Finish finishResult
            Just toolCall -> do
              let action = toolCallToAgentAction toolCall
               in return $ Continue action

  agentPrompt OllamaAgent {..} =
    return $ PromptTemplate $ defaultPrompt ollamaAvailableTools

  agentTools OllamaAgent {..} = return ollamaAvailableTools

anyToolToInputTool :: AnyTool -> InputTool
anyToolToInputTool (AnyTool tool _ _) =
  case toolName tool of
    "webScraper" -> ollamaWebScraperTool
    "searchWiki" -> ollamaWikiSearchTool
    name -> createGenericInputTool name (toolDescription tool)

createGenericInputTool :: Text -> Text -> InputTool
createGenericInputTool name desc =
  InputTool
    { toolType = "function"
    , function =
        FunctionDef
          { functionName = name
          , functionDescription = Just desc
          , functionParameters =
              Just $
                FunctionParameters
                  { parameterType = "object"
                  , requiredParams = Just ["input"]
                  , parameterProperties =
                      Just $
                        HM.fromList
                          [
                            ( "input"
                            , FunctionParameters
                                "string"
                                Nothing
                                Nothing
                                Nothing
                            )
                          ]
                  , additionalProperties = Just False
                  }
          , functionStrict = Nothing
          }
    }

toolCallToAgentAction :: ToolCall -> AgentAction
toolCallToAgentAction (ToolCall callId _ toolFunction) =
  let functionName = toolFunctionName toolFunction
      arguments = toolFunctionArguments toolFunction
      -- Extract the appropriate input based on the tool
      input = case functionName of
        "webScraper" -> case HM.lookup "url" arguments of
          Just (String url) -> url
          _ -> "Invalid URL parameter"
        "searchWiki" -> case HM.lookup "query" arguments of
          Just (String query) -> query
          _ -> "Invalid query parameter"
        _ -> case HM.lookup "input" arguments of
          Just (String inp) -> inp
          _ -> "No input parameter found"
   in AgentAction
        { actionToolName = functionName
        , actionInput = input
        , actionLog =
            "Tool call ID: "
              <> callId
              <> ", Function: "
              <> functionName
        }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : App.Agent.SimpleAgent
Description : Simple agent example
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
-}
module App.Agent.SimpleAgent (runApp) where

import Data.Aeson
import qualified Data.Map as HM
import Data.Ollama.Chat
  ( FunctionDef (..)
  , FunctionParameters (..)
  , InputTool (..)
  )
import qualified Data.Text as T
import Langchain.Agent.Core
import Langchain.Agent.Executor
import Langchain.Agent.ReAct
import Langchain.Error (toString)
import Langchain.LLM.Ollama
import Langchain.Tool.Core

getAge :: T.Text -> T.Text
getAge name_ = case name_ of
  "Alice" -> "40"
  "Bob" -> "45"
  "Charlie" -> "35"
  _ -> "Unknown"

data AgeFinderTool = AgeFinderTool

instance Tool AgeFinderTool where
  type Input AgeFinderTool = ToolCall
  type Output AgeFinderTool = T.Text

  toolName _ = "age_finder"
  toolDescription _ = "Finds the age of a person given their name."
  runTool _ (ToolCall _ _ ToolFunction {..}) = do
    if toolFunctionName == "age_finder"
      then do
        case HM.lookup "name" toolFunctionArguments of
          Nothing -> pure "Unknown"
          Just (String name_) -> pure $ getAge name_
          _ -> pure "Unknown"
      else pure "Unknown"

-- | Main application entry point.
runApp :: IO ()
runApp = do
  let llm = Ollama "qwen3:4b" []
  let tools2 = [ToolAcceptingToolCall AgeFinderTool]
  let paramProp =
        HM.fromList
          [ ("name", FunctionParameters "string" Nothing Nothing Nothing)
          ]
      functionParams =
        FunctionParameters
          { parameterType = "object"
          , requiredParams = Just ["name"]
          , parameterProperties = Just paramProp
          , additionalProperties = Just False
          }
      functionDef =
        FunctionDef
          { functionName = "age_finder"
          , functionDescription = Just "Finds the age of a person given their name."
          , functionParameters = Just functionParams
          , functionStrict = Nothing
          }
      inputTool =
        InputTool
          { toolType = "function"
          , function = functionDef
          }
  let mbOllamaParams =
        Just $
          defaultOllamaParams
            { tools = Just [inputTool]
            }
  let agent2 = createReActAgent llm mbOllamaParams tools2
  result2 <-
    runAgentExecutor
      agent2
      defaultAgentConfig
      defaultAgentCallbacks
      "What is the age of Alice? If Alice's age is more than 30, find age of Bob as well."
  case result2 of
    Left err -> putStrLn $ "Error: " <> toString err
    Right execResult -> do
      putStrLn $ "Got answer: " <> T.unpack (agentOutput $ executionFinish execResult)
      putStrLn $ "Iterations: " <> show (metricsIterations $ executionMetrics execResult)

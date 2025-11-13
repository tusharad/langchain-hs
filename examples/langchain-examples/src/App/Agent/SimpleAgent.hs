{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : App.Agent.SimpleAgent
Description : Simple agent example using calculator and web scraper tools
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT

This example demonstrates how to create and use a simple ReAct agent
with basic tools like calculator and web scraper.

Usage:
> cabal run langchain-examples -- simple-agent
-}
module App.Agent.SimpleAgent (runApp) where

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
  type Input AgeFinderTool = T.Text
  type Output AgeFinderTool = (Either String T.Text)

  toolName _ = "age_finder"
  toolDescription _ = "Finds the age of a person given their name."
  runTool _ input = pure $ Right $ getAge input

-- | Main application entry point.
runApp :: IO ()
runApp = do
  putStrLn "=== Simple ReAct Agent Example ==="
  let llm = Ollama "gemma3" []
  let config =
        defaultAgentConfig
          { maxIterations = 10
          , verboseLogging = True
          , returnIntermediateSteps = True
          }
      callbacks_ =
        defaultAgentCallbacks
          { onAgentStart = \input -> do
              putStrLn $ "Agent started with input: " <> T.unpack input
              putStrLn ""
          , onAgentAction = \action -> do
              putStrLn $ "Tool: " <> T.unpack (actionTool action)
              putStrLn $ "Input: " <> T.unpack (actionToolInput action)
          , onAgentObservation = \obs -> do
              putStrLn $ "Observation: " <> T.unpack (T.take 100 obs)
              putStrLn ""
          , onAgentFinish = \_ -> do
              putStrLn "Agent finish!"
          , onAgentError = \err -> do
              putStrLn $ "Error: " <> toString err
          }

  let ageFinder = wrapTool AgeFinderTool Just (either T.pack id)
  let tools2 = [ageFinder]
  let agent2 = createReActAgent llm Nothing tools2
  result2 <-
    runAgentExecutor
      agent2
      config
      callbacks_
      "What is the age of Alice? If Alice's age is more than 30, find age of Bob as well."
  case result2 of
    Left err -> putStrLn $ "Error: " <> toString err
    Right execResult -> do
      putStrLn $ "Got answer: " <> T.unpack (agentOutput $ executionFinish execResult)
      putStrLn $ "Iterations: " <> show (metricsIterations $ executionMetrics execResult)

{-# LANGUAGE OverloadedStrings #-}

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
import Langchain.Tool.Calculator
import Langchain.Tool.WebScraper

-- | Main application entry point.
runApp :: IO ()
runApp = do
  putStrLn "=== Simple ReAct Agent Example ==="
  let llm = Ollama "gemma3" []
  let calculator = wrapTool CalculatorTool Just (either T.pack (T.pack . show))
      webScraper = wrapTool WebScraper Just (either T.pack id)

  let tools_ = [calculator, webScraper]
  let agent = createReActAgent llm tools_
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
              putStrLn "Agent finished!"
          , onAgentError = \err -> do
              putStrLn $ "Error: " <> toString err
          }
  putStrLn "--- Example 1: Calculator ---"
  result1 <- runAgentExecutor agent config callbacks_ "What is 25 * 4 + 10?"
  case result1 of
    Left err -> putStrLn $ "Error: " <> toString err
    Right execResult -> do
      putStrLn $ "Final Answer2: " <> T.unpack (agentOutput $ executionFinish execResult)
      putStrLn $ "Iterations: " <> show (metricsIterations $ executionMetrics execResult)

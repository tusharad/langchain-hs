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
module App.Agent.SimpleAgent () where

{-
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agent.Core
import Langchain.Agent.Executor
import Langchain.Agent.ReAct
import Langchain.Agent.Tools
import Langchain.Error (toString)
import Langchain.LLM.Core
import Langchain.LLM.Ollama
import Langchain.Tool.Calculator
import Langchain.Tool.WebScraper
-}

-- | Main application entry point.

-- runApp :: IO ()
-- runApp = do
--   putStrLn "=== Simple ReAct Agent Example ==="
--   putStrLn ""

--   -- Create LLM
--   let llm = Ollama "llama3.2" []

--   -- Create tools
--   let calculator = wrapToolWithTransform
--         CalculatorTool
--         (Just . id) -- Calculator takes Text input
--         (either id (T.pack . show)) -- Format output

--       webScraper = wrapToolWithTransform
--         WebScraper
--         (Just . id) -- Web scraper takes Text (URL)
--         (either id id) -- Format output

--   -- Create tool registry
--   let tools = createToolRegistry [calculator, webScraper]

--   -- Create agent
--   let agent = createReActAgent llm tools

--   -- Configure agent execution
--   let config =
--         defaultAgentConfig
--           { maxIterations = 10
--           , verboseLogging = True
--           , returnIntermediateSteps = True
--           }

--       callbacks =
--         defaultAgentCallbacks
--           { onAgentStart = \input -> do
--               putStrLn $ "🤖 Agent started with input: " <> T.unpack input
--               putStrLn ""
--           , onAgentAction = \action -> do
--               putStrLn $ "🔧 Tool: " <> T.unpack (actionTool action)
--               putStrLn $ "   Input: " <> T.unpack (actionToolInput action)
--           , onAgentObservation = \obs -> do
--               putStrLn $ "👁️  Observation: " <> T.unpack (T.take 100 obs)
--               putStrLn ""
--           , onAgentFinish = \finish -> do
--               putStrLn "✅ Agent finished!"
--           , onAgentError = \err -> do
--               putStrLn $ "❌ Error: " <> toString err
--           }

--   -- Example 1: Simple calculation
--   putStrLn "--- Example 1: Calculator ---"
--   result1 <- runAgentExecutor agent config callbacks "What is 25 * 4 + 10?"
--   case result1 of
--     Left err -> putStrLn $ "Error: " <> toString err
--     Right execResult -> do
--       putStrLn $ "Final Answer: " <> T.unpack (agentOutput $ executionFinish execResult)
--       putStrLn $ "Iterations: " <> show (metricsIterations $ executionMetrics execResult)
--       putStrLn ""

--   -- Example 2: Multi-step calculation
--   putStrLn "--- Example 2: Multi-step ---"
--   result2 <- runAgentExecutor agent config callbacks "Calculate (15 + 25) * 2, then add 50 to the result"
--   case result2 of
--     Left err -> putStrLn $ "Error: " <> toString err
--     Right execResult -> do
--       putStrLn $ "Final Answer: " <> T.unpack (agentOutput $ executionFinish execResult)
--       putStrLn $ "Tool calls: " <> show (metricsToolCalls $ executionMetrics execResult)
--       putStrLn ""

-- {- | Example showing how to create a custom tool.
-- -}
-- customToolExample :: IO ()
-- customToolExample = do
--   -- Create a custom greeting tool
--   let greetingTool =
--         AgentTool
--           { agentToolName = "greeter"
--           , agentToolDescription = "Greets a person by name"
--           , agentToolExecutor = \name -> do
--               return $ Right $ "Hello, " <> name <> "! Nice to meet you."
--           , agentToolSchema = Nothing
--           }

--   let tools = createToolRegistry [greetingTool]
--       llm = Ollama "llama3.2" []
--       agent = createReActAgent llm tools

--   result <- runAgentExecutor
--     agent
--     defaultAgentConfig
--     defaultAgentCallbacks
--     "Use the greeter tool to say hello to Alice"

--   case result of
--     Left err -> putStrLn $ "Error: " <> toString err
--     Right execResult ->
--       putStrLn $ T.unpack (agentOutput $ executionFinish execResult)

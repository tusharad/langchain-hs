{- |
Module      : App.Agent.ToolAgent
Description : Example showing agent with multiple tools
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT

This example demonstrates an agent with multiple tools including:
- Calculator for mathematical operations
- Web scraper for fetching content
- DuckDuckGo search for web searches
- Wikipedia tool for knowledge queries

Usage:
> cabal run langchain-examples -- tool-agent
-}
module App.Agent.ToolAgent () where

-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import Langchain.Agent.Core
-- import Langchain.Agent.Executor
-- import Langchain.Agent.ReAct
-- import Langchain.Agent.Tools
-- import Langchain.Error (toString)
-- import Langchain.LLM.Ollama
-- import Langchain.Tool.Calculator
-- import Langchain.Tool.DuckDuckGo
-- import Langchain.Tool.WebScraper
-- import Langchain.Tool.WikipediaTool

-- runApp :: IO ()
-- runApp = do
--   putStrLn "=== Multi-Tool Agent Example ==="
--   putStrLn ""

--   -- Create LLM
--   let llm = Ollama "llama3.2" []

--   -- Create multiple tools
--   let calculator = wrapToolWithTransform
--         CalculatorTool
--         (Just . id)
--         (either T.pack (T.pack . show))

--       webScraper = wrapToolWithTransform
--         WebScraper
--         (Just . id)
--         (either T.pack id)

--       duckDuckGo = wrapToolWithTransform
--         DuckDuckGo
--         (Just . id)
--         id

--       wikipedia = wrapToolWithTransform
--         WikipediaTool
--         (Just . (\x -> _))
--         (either id id)

--   -- Create tool registry with all tools
--   let tools = createToolRegistry [calculator, webScraper, duckDuckGo, wikipedia]

--   -- Create agent
--   let agent = createReActAgent llm tools

--   -- Configure for more verbose output
--   let config =
--         defaultAgentConfig
--           { maxIterations = 15
--           , verboseLogging = True
--           , handleParsingErrors = True
--           }

--       callbacks =
--         defaultAgentCallbacks
--           { onAgentStart = \input ->
--               putStrLn $ "🚀 Starting agent with: " <> T.unpack input
--           , onAgentAction = \action ->
--               putStrLn $ "🔧 Using " <> T.unpack (actionTool action)
--           , onAgentFinish = \_ ->
--               putStrLn "✨ Task completed!"
--           }

--   -- Example queries demonstrating different tools
--   let queries =
--         [ "What is the square root of 144?"
--         , "Search for the current population of Tokyo"
--         , "Tell me about Albert Einstein from Wikipedia"
--         , "Calculate 25 * 4, then search for information about that number"
--         ]

--   -- Run each query
--   mapM_ (runQuery agent config callbacks) queries

-- runQuery :: Agent a => a -> AgentConfig -> AgentCallbacks -> Text -> IO ()
-- runQuery agent config callbacks query = do
--   putStrLn $ "\n📝 Query: " <> T.unpack query
--   putStrLn "---"

--   result <- runAgentExecutor agent config callbacks query

--   case result of
--     Left err ->
--       putStrLn $ "❌ Error: " <> toString err
--     Right execResult -> do
--       let finish = executionFinish execResult
--           metrics = executionMetrics execResult
--       putStrLn $ "\n✅ Answer: " <> T.unpack (agentOutput finish)
--       putStrLn $ "   Iterations: " <> show (metricsIterations metrics)
--       putStrLn $ "   Tool calls: " <> show (metricsToolCalls metrics)
--       putStrLn $ "   Time: " <> show (metricsExecutionTime metrics) <> "s"

--   putStrLn "\n" <> replicate 60 '='

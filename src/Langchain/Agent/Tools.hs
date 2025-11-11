{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : Langchain.Agent.Tools
Description : Tool management utilities for agents
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides utilities for managing and organizing tools that agents can use.
It includes:

- Tool registry for storing and retrieving tools
- Conversion between Tool typeclass instances and agent-friendly formats
- Helper functions for tool execution
- Tool wrapping and composition utilities

Example usage:

@
import Langchain.Agent.Tools
import Langchain.Tool.Calculator
import Langchain.Tool.WebScraper

main :: IO ()
main = do
  let calculator = CalculatorTool
      webscraper = WebScraper

  -- Create a tool registry
  registry <- createToolRegistry
    [ wrapTool calculator
    , wrapTool webscraper
    ]

  -- Look up and execute a tool
  case lookupTool "calculator" registry of
    Just executor -> do
      result <- executor "2 + 2"
      print result
    Nothing -> putStrLn "Tool not found"
@
-}
module Langchain.Agent.Tools
  ( -- * Tool Registry
    ToolRegistry
  , createToolRegistry
  , emptyToolRegistry
  , addTool
  , removeTool
  , lookupTool
  , listTools

    -- * Tool Wrapping
  , AgentTool (..)
  , wrapTool
  , wrapToolWithTransform
  , toolToDescriptor

    -- * Tool Execution
  , executeSafely
  , executeWithTimeout
  , executeWithRetry

    -- * Utilities
  , toolRegistryFromList
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agent.Core (ToolDescriptor (..))
import Langchain.Error
  ( LangchainResult
  , toolError
  )
import Langchain.Tool.Core (Tool (..))
import Text.Read (readMaybe)

{- | Wrapped tool that can be used by agents.

Hides the specific Tool instance type and provides:
- Tool name and description
- Execution function that takes Text input and returns Text output
- Optional JSON schema for input validation
-}
data AgentTool = AgentTool
  { agentToolName :: Text
  -- ^ Name of the tool
  , agentToolDescription :: Text
  -- ^ Description of what the tool does
  , agentToolExecutor :: Text -> IO (LangchainResult Text)
  -- ^ Function to execute the tool
  , agentToolSchema :: Maybe Value
  -- ^ Optional JSON schema for input
  }

{- | Registry of tools available to an agent.

Maps tool names to their wrapped implementations.
-}
type ToolRegistry = Map.Map Text (Text -> IO (LangchainResult Text))

-- | Create an empty tool registry.
emptyToolRegistry :: ToolRegistry
emptyToolRegistry = Map.empty

-- | Create a tool registry from a list of agent tools.
createToolRegistry :: [AgentTool] -> ToolRegistry
createToolRegistry tools =
  Map.fromList [(agentToolName t, agentToolExecutor t) | t <- tools]

{- | Create a tool registry from a list of tuples.

Each tuple contains:
- Tool name
- Tool executor function
-}
toolRegistryFromList :: [(Text, Text -> IO (LangchainResult Text))] -> ToolRegistry
toolRegistryFromList = Map.fromList

-- | Add a tool to the registry.
addTool :: AgentTool -> ToolRegistry -> ToolRegistry
addTool tool = Map.insert (agentToolName tool) (agentToolExecutor tool)

-- | Remove a tool from the registry by name.
removeTool :: Text -> ToolRegistry -> ToolRegistry
removeTool = Map.delete

-- | Look up a tool executor by name.
lookupTool :: Text -> ToolRegistry -> Maybe (Text -> IO (LangchainResult Text))
lookupTool = Map.lookup

-- | List all tool names in the registry.
listTools :: ToolRegistry -> [Text]
listTools = Map.keys

{- | Wrap a Tool typeclass instance into an AgentTool.

This allows any Tool instance to be used by agents.
The tool's input/output types are converted to/from Text.

Note: This assumes:
- Input can be parsed from Text
- Output can be converted to Text (using Show for Either types)
-}
wrapTool ::
  forall t.
  ( Tool t
  , Show (Output t)
  , Read (Input t)
  ) =>
  t ->
  AgentTool
wrapTool tool =
  AgentTool
    { agentToolName = toolName tool
    , agentToolDescription = toolDescription tool
    , agentToolExecutor = executor
    , agentToolSchema = Nothing
    }
  where
    executor :: Text -> IO (LangchainResult Text)
    executor inputText =
      case readMaybe (T.unpack inputText) of
        Nothing ->
          return $
            Left $
              toolError
                ("Failed to parse input for tool: " <> toolName tool)
                (Just $ toolName tool)
                Nothing
        Just input -> do
          output <- runTool tool input
          return $ Right $ T.pack $ show output

{- | Wrap a tool with custom input/output transformers.

Allows more control over how text is converted to/from tool inputs/outputs.

Parameters:
- Tool instance
- Input parser (Text -> Maybe input)
- Output formatter (output -> Text)
-}
wrapToolWithTransform ::
  forall t.
  Tool t =>
  t ->
  (Text -> Maybe (Input t)) ->
  (Output t -> Text) ->
  AgentTool
wrapToolWithTransform tool parseInput formatOutput =
  AgentTool
    { agentToolName = toolName tool
    , agentToolDescription = toolDescription tool
    , agentToolExecutor = executor
    , agentToolSchema = Nothing
    }
  where
    executor :: Text -> IO (LangchainResult Text)
    executor inputText =
      case parseInput inputText of
        Nothing ->
          return $
            Left $
              toolError
                ("Failed to parse input for tool: " <> toolName tool)
                (Just $ toolName tool)
                Nothing
        Just input -> do
          output <- runTool tool input
          return $ Right $ formatOutput output

{- | Convert an AgentTool to a ToolDescriptor.

Extracts the information needed by the agent to understand the tool.
-}
toolToDescriptor :: (Text -> IO (LangchainResult Text)) -> ToolDescriptor
toolToDescriptor _ =
  ToolDescriptor
    { toolDescName = "unknown"
    , toolDescDescription = "No description available"
    , toolDescInputSchema = Nothing
    }

{- | Execute a tool safely, catching any exceptions.

Wraps the tool execution in exception handling to prevent crashes.
-}
executeSafely ::
  (Text -> IO (LangchainResult Text)) ->
  Text ->
  IO (LangchainResult Text)
executeSafely executor input = do
  result <- try (executor input) :: IO (Either SomeException (LangchainResult Text))
  case result of
    Left ex ->
      return $
        Left $
          toolError
            ("Tool execution failed with exception: " <> T.pack (show ex))
            Nothing
            Nothing
    Right langchainResult -> return langchainResult

{- | Execute a tool with a timeout.

Parameters:
- Timeout in seconds
- Tool executor
- Input

Returns Left with timeout error if execution takes too long.
-}
executeWithTimeout ::
  Int ->
  (Text -> IO (LangchainResult Text)) ->
  Text ->
  IO (LangchainResult Text)
executeWithTimeout timeoutSeconds executor input = do
  resultVar <- newEmptyMVar

  -- Fork thread to execute tool
  threadId <- forkIO $ do
    result <- executeSafely executor input
    putMVar resultVar result

  -- Fork thread to timeout
  timeoutThreadId <- forkIO $ do
    threadDelay (timeoutSeconds * 1000000)
    putMVar resultVar $
      Left $
        toolError
          ("Tool execution timed out after " <> T.pack (show timeoutSeconds) <> " seconds")
          Nothing
          Nothing

  -- Wait for first result
  result <- takeMVar resultVar

  -- Clean up threads
  killThread threadId
  killThread timeoutThreadId

  return result

{- | Execute a tool with retry logic.

Parameters:
- Maximum number of attempts
- Tool executor
- Input

Retries on failure up to maxAttempts times.
-}
executeWithRetry ::
  Int ->
  (Text -> IO (LangchainResult Text)) ->
  Text ->
  IO (LangchainResult Text)
executeWithRetry maxAttempts executor input =
  go 1
  where
    go :: Int -> IO (LangchainResult Text)
    go attempt
      | attempt > maxAttempts =
          return $
            Left $
              toolError
                ("Tool execution failed after " <> T.pack (show maxAttempts) <> " attempts")
                Nothing
                Nothing
      | otherwise = do
          result <- executeSafely executor input
          case result of
            Left err ->
              if attempt < maxAttempts
                then do
                  -- Wait a bit before retrying
                  threadDelay 1000000 -- 1 second
                  go (attempt + 1)
                else return $ Left err
            Right success -> return $ Right success

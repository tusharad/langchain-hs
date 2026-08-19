{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agent.Functions
Description : OpenAI / Ollama Functions and Tool Calling agent loop
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implements the standard tool-calling agent loop:
1. Passes tool definitions to the LLM.
2. If LLM returns tool calls (including parallel tool calls), executes them concurrently or sequentially.
3. Injects tool result messages back into the conversation until a final textual answer is returned.
-}
module Langchain.Agent.Functions
  ( FunctionsAgent (..)
  , newFunctionsAgent
  , runFunctionsAgent
  ) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model
  ( ChatModel (..)
  , ContentBlock (..)
  , Message (..)
  , ToolCall (..)
  , extractMessageText
  , systemMessage
  , userMessage
  )
import qualified Langchain.Core.Model as Model
import Langchain.Tool.Core (Tool (..))

-- | Functions / Tool Calling agent container
data FunctionsAgent model m = FunctionsAgent
  { agentModel :: model
  , agentTools :: [Tool m]
  , agentSystemPrompt :: Maybe Text
  , agentMaxIterations :: Int
  }

-- | Construct a new FunctionsAgent
newFunctionsAgent :: model -> [Tool m] -> Maybe Text -> FunctionsAgent model m
newFunctionsAgent model tools sysPrompt =
  FunctionsAgent
    { agentModel = model
    , agentTools = tools
    , agentSystemPrompt = sysPrompt
    , agentMaxIterations = 10
    }

-- | Execute a user query through the tool calling agent loop
runFunctionsAgent ::
  (ChatModel model, MonadIO m, MonadError LangchainError m) =>
  FunctionsAgent model m ->
  Text ->
  m Text
runFunctionsAgent FunctionsAgent {..} query = do
  let initMsgs = case agentSystemPrompt of
        Just sys -> [systemMessage sys, userMessage query]
        Nothing -> [userMessage query]
  loop initMsgs agentMaxIterations
  where
    toolMap = Map.fromList [(toolName t, t) | t <- agentTools]

    loop msgs attemptsLeft
      | attemptsLeft <= 0 =
          throwError $
            agentError
              "Functions agent reached maximum iteration limit"
              (Just "FunctionsAgent")
              Nothing
      | otherwise = do
          resp <- invoke agentModel msgs Nothing
          case messageToolCalls resp of
            Nothing -> pure $ extractMessageText resp
            Just [] -> pure $ extractMessageText resp
            Just tCalls -> do
              toolResultMsgs <- forM tCalls $ \tCall -> do
                let tName = toolCallName tCall
                    tArgs = toolCallArguments tCall
                    tId = toolCallId tCall
                toolOutput <- case Map.lookup tName toolMap of
                  Just tool -> do
                    eRes <- toolExecute tool tArgs
                    case eRes of
                      Left err -> pure $ "Error: " <> T.pack (show err)
                      Right out -> pure out
                  Nothing ->
                    pure $ "Error: Tool '" <> tName <> "' is not available."
                pure $
                  Message
                    { messageRole = Model.Tool
                    , messageContents = pure (TextBlock toolOutput)
                    , messageName = Just tName
                    , messageToolCalls = Nothing
                    , messageToolId = Just tId
                    }
              let nextMsgs = msgs ++ [resp] ++ toolResultMsgs
              loop nextMsgs (attemptsLeft - 1)

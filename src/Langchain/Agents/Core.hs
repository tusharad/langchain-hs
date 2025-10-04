{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agents.Core
Description : Core implementation of LangChain agents
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>

Agents use LLMs as reasoning engines to determine actions dynamically.
-}
module Langchain.Agents.Core
  ( AgentAction (..)
  , AgentFinish (..)
  , AgentStep (..)
  , Agent (..)
  , AnyTool (..)
  , AgentState (..)
  , runAgent
  , runAgentLoop
  , executeTool
  , runSingleStep
  , customAnyTool
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Error (LangchainResult, internalError, toolError)
import Langchain.LLM.Core (Message (Message), Role (..), defaultMessageData)
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.PromptTemplate (PromptTemplate)
import Langchain.Tool.Core (Tool (..))

{- |
Represents an action to be taken by the agent
-}
data AgentAction = AgentAction
  { actionToolName :: Text
  -- ^ Tool name
  , actionInput :: Text
  -- ^ Input
  , actionLog :: Text
  -- ^ Execution log
  }
  deriving (Eq, Show)

-- | Represents that agent has finished work with final value
data AgentFinish = AgentFinish
  { returnValues :: Map.Map Text Text
  , finishLog :: Text
  }
  deriving (Show, Eq)

{- | Type that will be return from LLM
Could be either Continue, making another call to LLM or Finish with final value
-}
data AgentStep
  = Continue AgentAction
  | Finish AgentFinish
  deriving (Eq, Show)

-- | Type for maintaining state of the agent
data (BaseMemory m) => AgentState m = AgentState
  { agentMemory :: m
  -- ^ Memory for storing chat history
  , agentToolResults :: [(Text, Text)]
  -- ^ Tool results
  , agentSteps :: [AgentAction]
  -- ^ Agent steps happened so far
  }
  deriving (Eq, Show)

{- | A type that helps various types of Tools
It encapsulates the Tool, and conversion functions
to and from Text for Tool input and output since Agent takes and returns Text.
If Tool takes or returns Text type itself you can use `id` at these places.
-}
data AnyTool = forall a. Tool a => AnyTool
  { anyTool :: a
  , textToInput :: Text -> Input a
  , outputToText :: Output a -> Text
  }

-- | Typeclass for Agent
class Agent a where
  planNextAction :: BaseMemory m => a -> AgentState m -> IO (LangchainResult AgentStep)
  agentPrompt :: a -> IO PromptTemplate
  agentTools :: a -> IO [AnyTool]

  planNextActionM ::
    (BaseMemory mem, MonadIO m) =>
    a ->
    AgentState mem ->
    m (LangchainResult AgentStep)
  planNextActionM agent agentState = liftIO $ planNextAction agent agentState

  agentPromptM :: MonadIO m => a -> m PromptTemplate
  agentPromptM = liftIO . agentPrompt

  agentToolsM :: MonadIO m => a -> m [AnyTool]
  agentToolsM = liftIO . agentTools

-- | Function that *starts* the agent process.
runAgent ::
  (Agent a, BaseMemory m) =>
  a ->
  AgentState m ->
  Text ->
  IO (LangchainResult AgentFinish)
runAgent agent initialState@AgentState {..} initialInput = do
  memWithInput <- addUserMessage agentMemory initialInput
  case memWithInput of
    Left err -> return $ Left err
    Right updatedMem ->
      let newState = initialState {agentMemory = updatedMem}
       in runAgentLoop agent newState 0 10

-- | Helper function for runAgent
runAgentLoop ::
  (Agent a, BaseMemory m) =>
  a ->
  AgentState m ->
  Int ->
  Int ->
  IO (LangchainResult AgentFinish)
runAgentLoop agent agentState@AgentState {..} currIter maxIter
  | currIter > maxIter =
      return $ Left $ internalError "Max iterations exceeded" (Just "Agent") (Just "runAgentLoop")
  | otherwise = do
      eStepResult <- runSingleStep agent agentState
      case eStepResult of
        Left err -> return $ Left err
        Right (Finish agentFinish) -> return $ Right agentFinish
        Right (Continue act@AgentAction {..}) -> do
          toolList <- agentTools agent
          toolResult <- executeTool toolList actionToolName actionInput
          case toolResult of
            Left err -> return $ Left err
            Right result -> do
              -- Add the tool result to memory as a tool message
              print ("tool result " :: String, result)
              let toolMsg = Message Tool result defaultMessageData
              updatedMemResult <- addMessage agentMemory toolMsg
              case updatedMemResult of
                Left err -> return $ Left err
                Right updatedMem ->
                  let updatedState =
                        agentState
                          { agentMemory = updatedMem
                          , agentToolResults =
                              agentToolResults
                                ++ [(actionToolName, result)]
                          , agentSteps = agentSteps ++ [act]
                          }
                   in runAgentLoop agent updatedState (currIter + 1) maxIter

-- | Alias for planNextAction
runSingleStep ::
  (Agent a, BaseMemory m) =>
  a ->
  AgentState m ->
  IO (LangchainResult AgentStep)
runSingleStep = planNextAction

{- |
Execute a single tool call
Handles tool lookup and input/output conversion.
-}
executeTool :: [AnyTool] -> Text -> Text -> IO (LangchainResult Text)
executeTool tools toolName_ input = do
  case find (\(AnyTool t _ _) -> toolName t == toolName_) tools of
    Nothing ->
      return $ Left $ toolError ("Tool not found: " <> toolName_) (Just toolName_) (Just "executeTool")
    Just (AnyTool {..}) -> do
      resultE <- try $ do
        let typedInput = textToInput input
        result <- runTool anyTool typedInput
        return $ outputToText result
      case resultE of
        Left ex ->
          return $
            Left $
              toolError
                ("Tool execution error: " <> T.pack (show (ex :: SomeException)))
                (Just toolName_)
                (Just "executeTool")
        Right output -> return $ Right output

{- |
Helper for creating custom tool wrappers
Requires conversion functions between Text and tool-specific types.
-}
customAnyTool :: Tool a => a -> (Text -> Input a) -> (Output a -> Text) -> AnyTool
customAnyTool = AnyTool

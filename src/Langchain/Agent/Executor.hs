{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agent.Executor
Description : Agent execution loop and orchestration
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the execution engine for agents. It orchestrates the
agent planning loop, tool execution, and result collection.

The executor handles:
- The main agent loop (plan -> execute -> observe)
- Error handling and recovery
- Iteration limits and timeouts
- Callbacks and logging
- State management
-}
module Langchain.Agent.Executor
  ( -- * Main Execution Functions
    runAgentExecutor

    -- * Result Types
  , AgentExecutionResult (..)
  , ExecutionMetrics (..)

    -- * Utilities
  , createInitialState
  )
where

import Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Langchain.Agent.Core
import Langchain.Error
  ( LangchainResult
  , agentError
  )
import Langchain.LLM.Core

data AgentExecutionResult = AgentExecutionResult
  { executionFinish :: AgentFinish
  -- ^ The final output
  , executionSteps :: [AgentStep]
  -- ^ All intermediate steps taken
  , executionMetrics :: ExecutionMetrics
  -- ^ Performance metrics
  }
  deriving (Show, Eq)

data ExecutionMetrics = ExecutionMetrics
  { metricsIterations :: Int
  -- ^ Number of plan-execute cycles
  , metricsExecutionTime :: Double
  -- ^ Total time in seconds
  , metricsToolCalls :: Int
  -- ^ Number of tool executions
  , metricsSuccess :: Bool
  -- ^ Whether execution completed successfully
  }
  deriving (Show, Eq)

createInitialState :: Text -> AgentState
createInitialState input =
  AgentState
    { agentChatHistory = NE.singleton systemMessage
    , agentScratchpad = []
    , agentInput = input
    , agentIterations = 0
    }
  where
    systemMessage =
      Message
        { role = System
        , content = "You are a helpful AI assistant."
        , messageData = defaultMessageData
        }

{-
Returns False if:
- Max iterations reached
- Max execution time exceeded
-}
shouldContinue :: AgentConfig -> AgentState -> Double -> Bool
shouldContinue AgentConfig {..} state elapsedSeconds =
  iterationsOk && timeOk
  where
    iterationsOk = agentIterations state < maxIterations
    timeOk = case maxExecutionTime of
      Nothing -> True
      Just maxTime -> elapsedSeconds < fromIntegral maxTime

executeAgentLoop ::
  Agent a =>
  a ->
  AgentConfig ->
  AgentCallbacks ->
  AgentState ->
  UTCTime ->
  IO (LangchainResult AgentExecutionResult)
executeAgentLoop agent config callbacks initialState startTime =
  loop initialState []
  where
    loop :: AgentState -> [AgentStep] -> IO (LangchainResult AgentExecutionResult)
    loop state steps = do
      currentTime <- getCurrentTime
      let elapsedSeconds = realToFrac $ diffUTCTime currentTime startTime
      -- Check termination conditions
      if not (shouldContinue config state elapsedSeconds)
        then do
          let err = agentError "Agent execution exceeded limits" Nothing Nothing
          onAgentError callbacks err
          return $ Left err
        else do
          -- Plan next action
          when (verboseLogging config) $
            putStrLn $
              "[Agent] Planning iteration " <> show (agentIterations state)
          ePlan <- plan agent state
          case ePlan of
            Left err -> do
              -- TODO: handling parsing error
              onAgentError callbacks err
              return $ Left err
            Right (Right finish) -> do
              -- Agent has finished
              let metrics =
                    ExecutionMetrics
                      { metricsIterations = agentIterations state
                      , metricsExecutionTime = elapsedSeconds
                      , metricsToolCalls = length steps
                      , metricsSuccess = True
                      }
              return $ Right $ AgentExecutionResult finish steps metrics
            Right (Left action) -> do
              -- Execute action
              onAgentAction callbacks action
              when (verboseLogging config) $
                putStrLn $
                  "[Agent] Executing: " <> show (actionToolCall action)
              -- add toolCalls in state chatHistory
              let stateWithToolCall =
                    state
                      { agentChatHistory =
                          agentChatHistory state
                            `NE.append` NE.singleton (actionToMsg action)
                      }
              eRes <- sequenceA <$> traverse (executeTool agent) (actionToolCall action)
              case eRes of
                Left err -> do
                  -- TODO: handle parsing error
                  onAgentError callbacks err
                  return $ Left err
                Right observations -> do
                  mapM_ (onAgentObservation callbacks) observations
                  when (verboseLogging config) $
                    putStrLn $
                      "[Agent] Observation: " <> mconcat (T.unpack <$> observations)
                  -- Record step
                  timestamp <- getCurrentTime
                  let newSteps = map (\obs -> AgentStep action obs timestamp) observations
                  mapM_ (onAgentStep callbacks) newSteps
                  -- Update state and continue
                  let newState =
                        stateWithToolCall
                          { agentChatHistory =
                              agentChatHistory state
                                `NE.append` NE.fromList (map toolResultToMsg observations)
                          , agentScratchpad = agentScratchpad state ++ newSteps
                          , agentIterations = agentIterations state + 1
                          }
                  loop newState (steps ++ newSteps)
    actionToMsg action =
      defaultMessage
        { role = Assistant
        , content = actionLog action
        , messageData =
            defaultMessageData
              { toolCalls = Just (actionToolCall action)
              }
        }
    toolResultToMsg res =
      defaultMessage
        { role = Tool
        , content = res
        }

runAgentExecutor ::
  Agent a =>
  a ->
  AgentConfig ->
  AgentCallbacks ->
  Text ->
  IO (LangchainResult AgentExecutionResult)
runAgentExecutor agent config callbacks input = do
  startTime <- getCurrentTime
  onAgentStart callbacks input
  let initialState = createInitialState input
  eInitState <- initialize agent initialState
  case eInitState of
    Left err -> do
      onAgentError callbacks err
      return $ Left err
    Right state -> do
      -- Run the agent loop
      result <- executeAgentLoop agent config callbacks state startTime
      -- Finalize agent
      case result of
        Left err -> do
          finalize agent state
          return $ Left err
        Right execResult -> do
          finalize agent state
          onAgentFinish callbacks (executionFinish execResult)
          return $ Right execResult

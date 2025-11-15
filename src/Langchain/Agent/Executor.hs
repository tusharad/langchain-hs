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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Langchain.Agent.Core
import Langchain.Error
  ( LangchainResult
  , agentError
  )
import Langchain.LLM.Core
import Langchain.Memory.Core (BaseMemory (..), WindowBufferMemory (..), initialChatMessage)

data AgentExecutionResult = AgentExecutionResult
  { executionFinish :: AgentFinish
  -- ^ The final result of the agent execution
  , executionSteps :: [AgentStep]
  -- ^ All tool calls made and their results
  , executionMetrics :: ExecutionMetrics
  -- ^ Performance metrics
  }
  deriving (Show, Eq)

data ExecutionMetrics = ExecutionMetrics
  { metricsIterations :: Int
  -- ^ Number of agent iterations
  , metricsExecutionTime :: Double
  -- ^ Total time in seconds
  , metricsToolCalls :: Int
  -- ^ Number of tool calls made
  , metricsSuccess :: Bool
  -- ^ Whether execution completed successfully
  }
  deriving (Show, Eq)

-- | Create the initial state of the agent with default memory.
createInitialState :: Maybe SomeMemory -> Text -> AgentState
createInitialState mbSomeMemory input =
  AgentState
    { agentMemory = fromMaybe (SomeMemory defaultMemory) mbSomeMemory
    , agentInput = input
    , agentIterations = 0
    }
  where
    defaultMemory =
      WindowBufferMemory
        { maxWindowSize = 100
        , windowBufferMessages = initialChatMessage "You are a helpful AI assistant."
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

-- | Helper function to add an action to the state's memory
addActionToState :: AgentState -> AgentAction -> IO (LangchainResult AgentState)
addActionToState state action =
  case agentMemory state of
    SomeMemory mem -> do
      eMemWithAction <- addMessage mem (actionToMsg action)
      case eMemWithAction of
        Left err -> pure $ Left err
        Right memWithAction -> pure $ Right $ state {agentMemory = SomeMemory memWithAction}
  where
    actionToMsg act =
      defaultMessage
        { role = Assistant
        , content = actionLog act
        , messageData =
            defaultMessageData
              { toolCalls = Just (actionToolCall act)
              }
        }

-- | Helper function to add observations to the state's memory
addObservationsToState :: AgentState -> [Text] -> IO (LangchainResult AgentState)
addObservationsToState state observations =
  case agentMemory state of
    SomeMemory mem -> do
      eMemsWithObs <- sequenceA <$> traverse (addMessage mem . toolResultToMsg) observations
      case eMemsWithObs of
        Left err -> pure $ Left err
        Right mems -> pure $ Right $ state {agentMemory = SomeMemory (last mems)}
  where
    toolResultToMsg res =
      defaultMessage
        { role = Tool
        , content = res
        }

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
              -- add toolCalls in state memory
              eStateWithAction <- addActionToState state action
              case eStateWithAction of
                Left err -> do
                  onAgentError callbacks err
                  return $ Left err
                Right stateWithToolCall -> do
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
                      -- Update state memory with tool results and continue
                      eStateWithObs <- addObservationsToState stateWithToolCall observations
                      case eStateWithObs of
                        Left err -> do
                          onAgentError callbacks err
                          return $ Left err
                        Right newStateWithObs -> do
                          let newState =
                                newStateWithObs
                                  { agentIterations = agentIterations newStateWithObs + 1
                                  }
                          loop newState (steps ++ newSteps)

{- |
 Runs the agent executor.

 This function initializes the agent, runs the agent loop, and returns the final result.

 Arguments:
 - agent: The agent to run
 - config: The agent configuration
 - callbacks: The agent callbacks
 - input: The input to the agent

 Returns:
 - The final result of the agent execution
 - The execution metrics
 - The execution steps
-}
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
  let initialState = createInitialState (stateMemory config) input
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Langchain.Agent.Core
import Langchain.Agent.Middleware
import Langchain.Error
  ( LangchainResult
  , agentError
  )
import Langchain.LLM.Core
import Langchain.Memory.Core

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
  [AgentMiddleware a] ->
  AgentState ->
  UTCTime ->
  IO (LangchainResult AgentExecutionResult)
executeAgentLoop agent config callbacks middlewares initialState startTime =
  loop agent initialState []
  where
    loop agent0 state0 steps = runExceptT $ do
      currentTime <- liftIO getCurrentTime
      let elapsedSeconds = realToFrac $ diffUTCTime currentTime startTime
      -- Check termination conditions
      if not (shouldContinue config state0 elapsedSeconds)
        then do
          let err = agentError "Agent execution exceeded limits" Nothing Nothing
          ExceptT . pure $ Left err
        else do
          -- Plan next action
          when (verboseLogging config) $
            liftIO $
              putStrLn $
                "[Agent] Planning iteration " <> show (agentIterations state0)
          (state1, agent1) <-
            liftIO $
              applyMiddlewares beforeModelCall middlewares (state0, agent0)
          plan_ <- ExceptT $ plan agent1 state1
          (state2, agent2) <-
            liftIO $
              applyMiddlewares afterModelCall middlewares (state1, agent1)
          case plan_ of
            (Done finish) -> do
              -- Agent has finished
              let metrics =
                    ExecutionMetrics
                      { metricsIterations = agentIterations state2
                      , metricsExecutionTime = elapsedSeconds
                      , metricsToolCalls = length steps
                      , metricsSuccess = True
                      }
              return $ AgentExecutionResult finish steps metrics
            (Continue action) -> do
              -- add toolCalls in state memory
              state3 <- ExceptT $ addActionToState state2 action
              -- Execute action
              liftIO $ onAgentAction callbacks action
              (state4, agent4) <-
                liftIO $
                  applyMiddlewares beforeToolCall middlewares (state3, agent2)
              when (verboseLogging config) $
                liftIO $
                  putStrLn $
                    "[Agent] Executing: " <> show (actionToolCall action)
              observations <-
                ExceptT $
                  sequenceA <$> traverse (executeTool agent4) (actionToolCall action)
              mapM_ (liftIO . onAgentObservation callbacks) observations
              when (verboseLogging config) $
                liftIO $
                  putStrLn $
                    "[Agent] Observation: " <> mconcat (T.unpack <$> observations)
              -- Record step
              timestamp <- liftIO getCurrentTime
              let newSteps = map (\obs -> AgentStep action obs timestamp) observations
              mapM_ (liftIO . onAgentStep callbacks) newSteps
              -- Update state memory with tool results and continue
              state5 <-
                ExceptT $
                  addObservationsToState state4 observations
              (state6, agent6) <-
                liftIO $
                  applyMiddlewares afterToolCall middlewares (state5, agent4)
              let newState =
                    state6
                      { agentIterations = agentIterations state6 + 1
                      }
              ExceptT (loop agent6 newState (steps ++ newSteps))

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
  [AgentMiddleware a] ->
  Text ->
  IO (LangchainResult AgentExecutionResult)
runAgentExecutor agent0 config callbacks middlewares input = do
  startTime <- getCurrentTime
  onAgentStart callbacks input
  let initialState = createInitialState (stateMemory config) input
  eInitState <- initialize agent0 initialState
  case eInitState of
    Left err -> do
      onAgentError callbacks err
      return $ Left err
    Right state0 -> do
      -- Run the agent loop
      (state1, agent1) <- applyMiddlewares beforeAgent middlewares (state0, agent0)
      result <- executeAgentLoop agent1 config callbacks middlewares state1 startTime
      -- Finalize agent
      case result of
        Left err -> do
          onAgentError callbacks err
          finalize agent1 state1
          return $ Left err
        Right execResult -> do
          finalize agent1 state1
          onAgentFinish callbacks (executionFinish execResult)
          return $ Right execResult

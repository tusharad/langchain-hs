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

Example usage:

@
import Langchain.Agent.Core
import Langchain.Agent.Executor
import Langchain.Agent.ReAct

main :: IO ()
main = do
  let agent = createReActAgent llm tools
      config = defaultAgentConfig { maxIterations = 10 }
      callbacks = defaultAgentCallbacks
        { onAgentAction = \\action -> putStrLn $ "Action: " <> show action
        }

  result <- runAgentExecutor agent config callbacks "What is 2+2?"
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right (finish, steps) -> do
      putStrLn $ "Result: " <> agentOutput finish
      when (not $ null steps) $ do
        putStrLn "\\nIntermediate steps:"
        mapM_ print steps
@
-}
module Langchain.Agent.Executor
  ( -- * Main Execution Functions
    runAgentExecutor
  , runAgentExecutorWithState
  , executeAgentStep

    -- * Result Types
  , AgentExecutionResult (..)
  , ExecutionMetrics (..)

    -- * Utilities
  , createInitialState
  , shouldContinue
  , recordStep
  ) where

import Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Langchain.Agent.Core
import Langchain.Error
  ( LangchainError
  , LangchainResult
  , agentError
  , chainError
  , toString
  )
import Langchain.LLM.Core (Message (..), Role (..), defaultMessageData)

{- | Result of agent execution.

Contains:
- The final result
- Intermediate steps (if requested)
- Execution metrics
-}
data AgentExecutionResult = AgentExecutionResult
  { executionFinish :: AgentFinish
  -- ^ The final output
  , executionSteps :: [AgentStep]
  -- ^ All intermediate steps taken
  , executionMetrics :: ExecutionMetrics
  -- ^ Performance metrics
  }
  deriving (Show, Eq)

{- | Metrics about agent execution.

Tracks:
- Number of iterations
- Total execution time
- Number of tool calls
- Success/failure status
-}
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

{- | Run an agent executor with the given configuration.

This is the main entry point for executing an agent.

Parameters:
- Agent instance
- Configuration
- Callbacks
- Input query

Returns:
- Either an error or the execution result

Example:

@
result <- runAgentExecutor myAgent defaultAgentConfig defaultAgentCallbacks "Hello"
case result of
  Left err -> handleError err
  Right execResult -> handleSuccess execResult
@
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

  -- Create initial state
  let initialState = createInitialState input

  -- Initialize agent
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

{- | Run agent executor with a custom initial state.

Useful when you want to provide pre-populated chat history or scratchpad.
-}
runAgentExecutorWithState ::
  Agent a =>
  a ->
  AgentConfig ->
  AgentCallbacks ->
  AgentState ->
  IO (LangchainResult AgentExecutionResult)
runAgentExecutorWithState agent config callbacks state = do
  startTime <- getCurrentTime
  onAgentStart callbacks (agentInput state)

  -- Initialize agent
  eInitState <- initialize agent state
  case eInitState of
    Left err -> do
      onAgentError callbacks err
      return $ Left err
    Right initState -> do
      -- Run the agent loop
      result <- executeAgentLoop agent config callbacks initState startTime

      -- Finalize agent
      case result of
        Left err -> do
          finalize agent initState
          return $ Left err
        Right execResult -> do
          finalize agent initState
          onAgentFinish callbacks (executionFinish execResult)
          return $ Right execResult

{- | The main agent execution loop.

Repeatedly:
1. Plan next action
2. Execute action (if not finished)
3. Record observation
4. Check termination conditions
-}
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
              if handleParsingErrors config
                then handlePlanError agent config callbacks state steps err
                else do
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
                  "[Agent] Executing: " <> T.unpack (actionTool action)

              eResult <- executeAgentStep agent action
              case eResult of
                Left err -> do
                  onAgentError callbacks err
                  if handleParsingErrors config
                    then do
                      -- Treat error as observation
                      let observation = "Error executing tool: " <> T.pack (toString err)
                      timestamp <- getCurrentTime
                      let step = AgentStep action observation timestamp
                      onAgentObservation callbacks observation
                      onAgentStep callbacks step
                      let newState =
                            state
                              { agentScratchpad = agentScratchpad state ++ [step]
                              , agentIterations = agentIterations state + 1
                              }
                      loop newState (steps ++ [step])
                    else return $ Left err
                Right observation -> do
                  onAgentObservation callbacks observation

                  when (verboseLogging config) $
                    putStrLn $
                      "[Agent] Observation: " <> T.unpack observation

                  -- Record step
                  timestamp <- getCurrentTime
                  let step = AgentStep action observation timestamp
                  onAgentStep callbacks step

                  -- Update state and continue
                  let newState =
                        state
                          { agentScratchpad = agentScratchpad state ++ [step]
                          , agentIterations = agentIterations state + 1
                          }
                  loop newState (steps ++ [step])

{- | Handle planning errors with recovery strategies.

When the agent fails to plan (e.g., parsing error), we can:
1. Provide feedback to the agent about the error
2. Give it another chance to plan correctly
-}
handlePlanError ::
  Agent a =>
  a ->
  AgentConfig ->
  AgentCallbacks ->
  AgentState ->
  [AgentStep] ->
  LangchainError ->
  IO (LangchainResult AgentExecutionResult)
handlePlanError agent config callbacks state _steps err = do
  when (verboseLogging config) $
    putStrLn $
      "[Agent] Planning error (will retry): " <> toString err

  -- Create a pseudo-observation with the error
  let observation = "Error: " <> T.pack (toString err) <> "\nPlease try again with correct format."
  timestamp <- getCurrentTime

  -- Create a dummy action to record the error
  let dummyAction =
        AgentAction
          { actionTool = "error"
          , actionToolInput = ""
          , actionLog = "Planning error occurred"
          , actionMetadata = Map.empty
          }
      step = AgentStep dummyAction observation timestamp

  onAgentObservation callbacks observation
  onAgentStep callbacks step

  -- Update state and continue
  let newState =
        state
          { agentScratchpad = agentScratchpad state ++ [step]
          , agentIterations = agentIterations state + 1
          }

  -- Check if we should continue after error
  if shouldContinue config newState 0
    then executeAgentLoop agent config callbacks newState =<< getCurrentTime
    else do
      let finalErr = chainError "Agent failed after error recovery attempts" err
      return $ Left finalErr

{- | Execute a single agent step.

Takes an action and executes the corresponding tool.
-}
executeAgentStep ::
  Agent a =>
  a ->
  AgentAction ->
  IO (LangchainResult Text)
executeAgentStep agent AgentAction {..} =
  executeTool agent actionTool actionToolInput

{- | Create initial agent state from input.

Sets up:
- Empty chat history (can be customized)
- Empty scratchpad
- The input query
- Zero iterations
-}
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

{- | Check if agent should continue executing.

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

{- | Record a step in the agent's scratchpad.

Utility function to add a step to the state.
-}
recordStep :: AgentState -> AgentStep -> AgentState
recordStep state step =
  state
    { agentScratchpad = agentScratchpad state ++ [step]
    , agentIterations = agentIterations state + 1
    }

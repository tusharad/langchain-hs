{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Langchain.Agent.PlanAndExecute
Description : Plan-and-Execute agent architecture using JSON structured output and effectful step executors
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Separates complex multi-step reasoning into a two-phase architecture:
1. Planner LLM generates an explicit sequence of structured steps as typed JSON.
2. Executor (an agent with tools, a function, or a model) executes each step sequentially with accumulated context.
-}
module Langchain.Agent.PlanAndExecute
  ( PlanStep (..)
  , Plan (..)
  , StepExecutor (..)
  , PlanAndExecuteAgent (..)
  , newPlanAndExecuteAgent
  , newPlanAndExecuteAgentWithTools
  , runPlanAndExecute
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), ToJSON, Value (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Agent.ReAct (ReActAgent, defaultReActAgent, runReActAgent)
import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , userMessage
  )
import Langchain.Core.Tool (Tool)
import Langchain.OutputParser.Structured (StructuredOutput, TypeSchema, structuredInvoke)
import Langchain.Tool.Binding (ToolBinder (..))

-- | Single step in an execution plan
data PlanStep = PlanStep
  { stepNumber :: !Int
  , stepDescription :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, TypeSchema)

instance FromJSON PlanStep where
  parseJSON = withObject "PlanStep" $ \o -> do
    num <-
      o .:? "stepNumber" >>= \case
        Just n -> pure n
        Nothing ->
          o .:? "step" >>= \case
            Just n -> pure n
            Nothing -> o .:? "number" .!= 1
    desc <-
      o .:? "stepDescription" >>= \case
        Just d -> pure d
        Nothing ->
          o .:? "description" >>= \case
            Just d -> pure d
            Nothing ->
              o .:? "task" >>= \case
                Just d -> pure d
                Nothing -> o .: "action"
    pure $ PlanStep num desc

-- | Collection of steps forming a plan
newtype Plan = Plan
  { planSteps :: [PlanStep]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, StructuredOutput, TypeSchema)

instance FromJSON Plan where
  parseJSON (Object o) = Plan <$> (o .: "planSteps" <|> o .: "steps" <|> o .: "plan")
  parseJSON (Array arr) = Plan <$> parseJSON (Array arr)
  parseJSON invalid = prependFailure "parsing Plan failed, " (typeMismatch "Object or Array" invalid)

-- | Abstraction for executing individual steps of a plan (agents, models with tools, or custom runners)
class StepExecutor e m where
  executeStep ::
    -- | Step executor instance (agent, model with tools, or custom runner)
    e ->
    -- | Step instruction prompt to execute
    Text ->
    -- | Textual result produced by executing the step
    m Text

instance
  {-# OVERLAPPING #-}
  (ToolBinder model n, MonadIO n, MonadError LangchainError n) =>
  StepExecutor (ReActAgent model n) n
  where
  executeStep agent prompt = do
    msg <- runReActAgent agent [userMessage prompt]
    pure $ extractMessageText msg

instance
  {-# OVERLAPPING #-}
  (ToolBinder model n, MonadIO n, MonadError LangchainError n) =>
  StepExecutor (model, [Tool n]) n
  where
  executeStep (model, tools) =
    executeStep (defaultReActAgent model tools)

instance {-# OVERLAPPING #-} (m ~ n) => StepExecutor (Text -> m Text) n where
  executeStep = id

instance {-# OVERLAPPABLE #-} (ChatModel model, MonadIO m, MonadError LangchainError m) => StepExecutor model m where
  executeStep model prompt = do
    msg <- invoke model [userMessage prompt] Nothing
    pure $ extractMessageText msg

-- | Plan-and-Execute agent container
data PlanAndExecuteAgent planner executor = PlanAndExecuteAgent
  { plannerModel :: planner
  , stepExecutor :: executor
  , planPromptTemplate :: Maybe Text
  }

-- | Construct a new PlanAndExecuteAgent with any StepExecutor (agent, function, or model)
newPlanAndExecuteAgent ::
  -- | LLM model responsible for creating the execution plan
  planner ->
  -- | StepExecutor instance used to carry out each plan step
  executor ->
  -- | Optional custom prompt template for the planner
  Maybe Text ->
  -- | Configured PlanAndExecuteAgent instance
  PlanAndExecuteAgent planner executor
newPlanAndExecuteAgent = PlanAndExecuteAgent

-- | Construct a PlanAndExecuteAgent with tools using a ReActAgent as the step executor
newPlanAndExecuteAgentWithTools ::
  -- | LLM model responsible for creating the execution plan
  planner ->
  -- | LLM model for the ReAct step executor
  model ->
  -- | Available tools for the ReAct step executor
  [Tool m] ->
  -- | Optional custom prompt template for the planner
  Maybe Text ->
  -- | Configured PlanAndExecuteAgent with tool-enabled ReAct executor
  PlanAndExecuteAgent planner (ReActAgent model m)
newPlanAndExecuteAgentWithTools planner model tools =
  PlanAndExecuteAgent planner (defaultReActAgent model tools)

-- | Execute a goal using the Plan-and-Execute workflow with structured JSON planning
runPlanAndExecute ::
  (ChatModel planner, StepExecutor executor m, MonadIO m, MonadError LangchainError m) =>
  -- | Configured PlanAndExecuteAgent instance
  PlanAndExecuteAgent planner executor ->
  -- | High-level user goal to plan and solve
  Text ->
  -- | Final synthesized answer solving the user goal
  m Text
runPlanAndExecute PlanAndExecuteAgent {..} userGoal = do
  let planPrompt = case planPromptTemplate of
        Just p -> p <> "\nGoal: " <> userGoal
        Nothing ->
          "You are an expert planner. For the following goal, generate a concise step-by-step execution plan.\n"
            <> "Output JSON format: {\"planSteps\": [{\"stepNumber\": 1, \"stepDescription\": \"...\"}]}\n"
            <> "Keep the plan focused and minimal (between 2 to 3 distinct, actionable steps).\n"
            <> "Goal: "
            <> userGoal
  plan <- structuredInvoke plannerModel [userMessage planPrompt]
  if null (planSteps plan)
    then throwError $ agentError "Planner generated an empty plan" (Just "PlanAndExecuteAgent") Nothing
    else executeSteps (planSteps plan) []
  where
    executeSteps [] stepOutputs = do
      let synthesisPrompt =
            "User Goal: "
              <> userGoal
              <> "\n\nStep Execution History:\n"
              <> T.unlines
                [T.pack (show num) <> ". " <> desc <> " -> " <> out | (PlanStep num desc, out) <- stepOutputs]
              <> "\n\nProvide the final synthesized answer satisfying the goal:"
      executeStep stepExecutor synthesisPrompt
    executeSteps (currStep : restSteps) prevOutputs = do
      let stepPrompt =
            "User Goal: "
              <> userGoal
              <> ( if null prevOutputs
                     then ""
                     else
                       "\n\nCompleted Steps So Far:\n"
                         <> T.unlines
                           [T.pack (show num) <> ". " <> desc <> " -> " <> out | (PlanStep num desc, out) <- prevOutputs]
                 )
              <> "\n\nCurrent Task To Execute (Step "
              <> T.pack (show (stepNumber currStep))
              <> "): "
              <> stepDescription currStep
              <> "\nExecute this task using any appropriate tools available and provide the outcome:"
      stepOut <- executeStep stepExecutor stepPrompt
      executeSteps restSteps (prevOutputs ++ [(currStep, stepOut)])

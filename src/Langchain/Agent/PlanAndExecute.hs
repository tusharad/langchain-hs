{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agent.PlanAndExecute
Description : Plan-and-Execute agent architecture
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Separates complex multi-step reasoning into a two-phase architecture:
1. Planner LLM generates an explicit sequence of structured steps.
2. Executor LLM/agent executes each step sequentially with accumulated context.
-}
module Langchain.Agent.PlanAndExecute
  ( PlanStep (..)
  , Plan (..)
  , PlanAndExecuteAgent (..)
  , newPlanAndExecuteAgent
  , parsePlanFromText
  , runPlanAndExecute
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , userMessage
  )

-- | Single step in an execution plan
data PlanStep = PlanStep
  { stepNumber :: !Int
  , stepDescription :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Collection of steps forming a plan
newtype Plan = Plan
  { planSteps :: [PlanStep]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Plan-and-Execute agent container
data PlanAndExecuteAgent planner executor = PlanAndExecuteAgent
  { plannerModel :: planner
  , executorModel :: executor
  , planPromptTemplate :: Maybe Text
  }

-- | Construct a new PlanAndExecuteAgent
newPlanAndExecuteAgent ::
  planner ->
  executor ->
  Maybe Text ->
  PlanAndExecuteAgent planner executor
newPlanAndExecuteAgent = PlanAndExecuteAgent

-- | Parse numbered plan steps from LLM text output
parsePlanFromText :: Text -> Plan
parsePlanFromText rawTxt =
  let rawLines = map T.strip (T.lines rawTxt)
      validLines = filter isPlanLine rawLines
      steps =
        if null validLines
          then [PlanStep 1 rawTxt]
          else zipWith PlanStep [1 ..] (map cleanBullet validLines)
   in Plan steps
  where
    isPlanLine l
      | T.null l = False
      | T.isPrefixOf "- " l || T.isPrefixOf "* " l = True
      | otherwise =
          let (digits, rest) = T.span (`elem` ['0' .. '9']) l
           in not (T.null digits)
                && ( T.isPrefixOf ". " rest
                       || T.isPrefixOf "." rest
                       || T.isPrefixOf ") " rest
                   )

    cleanBullet t
      | T.isPrefixOf "- " t = T.drop 2 t
      | T.isPrefixOf "* " t = T.drop 2 t
      | otherwise =
          let (_, rest) = T.span (`elem` ['0' .. '9']) t
           in if T.isPrefixOf ". " rest || T.isPrefixOf ") " rest
                then T.drop 2 rest
                else
                  if T.isPrefixOf "." rest || T.isPrefixOf ")" rest
                    then T.stripStart (T.drop 1 rest)
                    else t

-- | Execute a goal using the Plan-and-Execute workflow
runPlanAndExecute ::
  (ChatModel planner, ChatModel executor, MonadIO m, MonadError LangchainError m) =>
  PlanAndExecuteAgent planner executor ->
  Text ->
  m Text
runPlanAndExecute PlanAndExecuteAgent {..} userGoal = do
  let planPrompt = case planPromptTemplate of
        Just p -> p <> "\nGoal: " <> userGoal
        Nothing ->
          "You are an expert planner. For the following goal, output a step-by-step numbered plan.\n"
            <> "Goal: "
            <> userGoal
            <> "\nNumbered Plan:"
  planMsg <- invoke plannerModel [userMessage planPrompt] Nothing
  let plan = parsePlanFromText (extractMessageText planMsg)
  if null (planSteps plan)
    then throwError $ agentError "Planner generated an empty plan" (Just "PlanAndExecuteAgent") Nothing
    else executeSteps (planSteps plan) []
  where
    executeSteps [] stepOutputs = do
      let synthesisPrompt =
            "Goal: "
              <> userGoal
              <> "\n\nStep Execution History:\n"
              <> T.unlines
                [T.pack (show num) <> ". " <> desc <> " -> " <> out | (PlanStep num desc, out) <- stepOutputs]
              <> "\n\nProvide the final synthesized answer:"
      finalMsg <- invoke executorModel [userMessage synthesisPrompt] Nothing
      pure $ extractMessageText finalMsg
    executeSteps (currStep : restSteps) prevOutputs = do
      let stepPrompt =
            "Goal: "
              <> userGoal
              <> "\n\nPrevious Steps Completed:\n"
              <> T.unlines
                [T.pack (show num) <> ". " <> desc <> " -> " <> out | (PlanStep num desc, out) <- prevOutputs]
              <> "\n\nCurrent Step To Execute: "
              <> stepDescription currStep
              <> "\nExecute this step and provide the result:"
      stepMsg <- invoke executorModel [userMessage stepPrompt] Nothing
      let stepOut = extractMessageText stepMsg
      executeSteps restSteps (prevOutputs ++ [(currStep, stepOut)])

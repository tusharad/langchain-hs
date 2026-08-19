{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agent.Supervisor
Description : Multi-agent team orchestration with typed capabilities and delegation history
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Coordinates a team of specialized sub-agents via a supervisor model that routes sub-tasks,
tracks delegation history, and aggregates final solutions.
-}
module Langchain.Agent.Supervisor
  ( SpecialistAgent (..)
  , DelegationEvent (..)
  , DelegationStrategy (..)
  , SupervisorTeam (..)
  , newSupervisorTeam
  , runSupervisorTeam
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , userMessage
  )

-- | Specialized worker agent within the team
data SpecialistAgent m = SpecialistAgent
  { specialistName :: !Text
  , specialistDescription :: !Text
  , specialistCapabilities :: ![Text]
  , specialistAction :: Text -> m Text
  }

-- | Record of a task delegation event
data DelegationEvent = DelegationEvent
  { delegationStep :: !Int
  , delegatedTo :: !Text
  , delegationPrompt :: !Text
  , delegationResult :: !Text
  }
  deriving (Show, Eq)

-- | Strategy for routing tasks among agents
data DelegationStrategy
  = LLMGuided
  | CapabilityMatch
  | RoundRobin
  deriving (Show, Eq)

-- | Supervisor multi-agent team container
data SupervisorTeam supervisorModel m = SupervisorTeam
  { supervisorModel :: supervisorModel
  , specialistAgents :: [SpecialistAgent m]
  , delegationStrategy :: DelegationStrategy
  , maxDelegationTurns :: Int
  }

-- | Construct a new SupervisorTeam
newSupervisorTeam ::
  supervisorModel ->
  [SpecialistAgent m] ->
  SupervisorTeam supervisorModel m
newSupervisorTeam model agents =
  SupervisorTeam
    { supervisorModel = model
    , specialistAgents = agents
    , delegationStrategy = LLMGuided
    , maxDelegationTurns = 5
    }

-- | Run the multi-agent team supervisor loop to complete a goal
runSupervisorTeam ::
  (ChatModel supervisorModel, MonadIO m, MonadError LangchainError m) =>
  SupervisorTeam supervisorModel m ->
  Text ->
  m Text
runSupervisorTeam SupervisorTeam {..} goal = do
  if null specialistAgents
    then
      throwError $
        agentError "Supervisor team has no specialist agents registered" (Just "SupervisorTeam") Nothing
    else loop 1 []
  where
    agentMap = Map.fromList [(specialistName a, a) | a <- specialistAgents]

    loop turn history
      | turn > maxDelegationTurns = synthesizeFinalResult history
      | otherwise = do
          let rosterDesc =
                T.unlines
                  [ "- "
                      <> specialistName a
                      <> ": "
                      <> specialistDescription a
                      <> " (Capabilities: "
                      <> T.intercalate ", " (specialistCapabilities a)
                      <> ")"
                  | a <- specialistAgents
                  ]
              historyDesc =
                if null history
                  then "None yet."
                  else
                    T.unlines
                      [ "Step "
                          <> T.pack (show delegationStep)
                          <> " ["
                          <> delegatedTo
                          <> "]:\nTask: "
                          <> delegationPrompt
                          <> "\nResult: "
                          <> delegationResult
                      | DelegationEvent {..} <- history
                      ]
              decisionPrompt =
                "You are the Supervisor of a specialized AI team.\nGoal: "
                  <> goal
                  <> "\n\nAvailable Specialists:\n"
                  <> rosterDesc
                  <> "\nDelegation History:\n"
                  <> historyDesc
                  <> "\nIf the goal is fully achieved, respond with 'FINISH: <final response>'.\n"
                  <> "Otherwise, delegate the next step by responding strictly in the format:\n"
                  <> "DELEGATE: <AgentName> | <Specific sub-task description>"

          resp <- invoke supervisorModel [userMessage decisionPrompt] Nothing
          let decisionTxt = T.strip (extractMessageText resp)
          if "FINISH:" `T.isPrefixOf` decisionTxt
            then pure $ T.strip (T.drop 7 decisionTxt)
            else
              if "DELEGATE:" `T.isPrefixOf` decisionTxt
                then do
                  let afterPrefix = T.strip (T.drop 9 decisionTxt)
                      (targetAgentName, taskDesc) = case T.breakOn "|" afterPrefix of
                        (name, rest) | not (T.null rest) -> (T.strip name, T.strip (T.drop 1 rest))
                        _ -> (T.strip afterPrefix, goal)
                  case Map.lookup targetAgentName agentMap of
                    Just agent -> do
                      res <- specialistAction agent taskDesc
                      let event = DelegationEvent turn targetAgentName taskDesc res
                      loop (turn + 1) (history ++ [event])
                    Nothing -> do
                      -- Fallback to first agent if supervisor hallucinated a name
                      fallbackAgent <- case listToMaybe specialistAgents of
                        Just res -> pure res
                        Nothing -> throwError $ agentError "supervisor agent is empty" Nothing Nothing
                      res <- specialistAction fallbackAgent taskDesc
                      let event = DelegationEvent turn (specialistName fallbackAgent) taskDesc res
                      loop (turn + 1) (history ++ [event])
                else synthesizeFinalResult history

    synthesizeFinalResult history = do
      let summaryPrompt =
            "Goal: "
              <> goal
              <> "\n\nExecution History:\n"
              <> T.unlines [delegatedTo <> ": " <> delegationResult | DelegationEvent {..} <- history]
              <> "\n\nSynthesize a comprehensive final answer:"
      finalMsg <- invoke supervisorModel [userMessage summaryPrompt] Nothing
      pure $ extractMessageText finalMsg

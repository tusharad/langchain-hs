{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Cognitive.Evaluator
Description : Task Completability & Tool Activation Decision Engine
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Evaluates whether decomposed user tasks can be fully answered with the currently
retrieved Brain knowledge context or whether external tools must be activated.
-}
module Cortex.Cognitive.Evaluator
  ( TaskEvaluation (..)
  , CognitiveDecision (..)
  , evaluateTasks
  , evaluateSingleTask
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)

import Cortex.Cognitive.Decomposer (UserTask (..))
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel, Role (..), extractMessageText, invoke, textMessage)
import Langchain.DocumentLoader.Core (Document (..))

-- | Evaluation result for a single task
data TaskEvaluation = TaskEvaluation
  { evalTaskId :: !Text
  , evalIsCompletable :: !Bool
  , evalNeededTool :: !(Maybe Text)
  , evalReasoning :: !Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON TaskEvaluation
instance FromJSON TaskEvaluation

-- | Consolidated decision across all decomposed tasks
data CognitiveDecision = CognitiveDecision
  { evaluatedTasks :: ![TaskEvaluation]
  , toolsToActivate :: ![Text]
  , allCompletable :: !Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON CognitiveDecision
instance FromJSON CognitiveDecision

-- | Evaluate all tasks against retrieved knowledge context
evaluateTasks
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> [UserTask]
  -> [Document]         -- ^ Retrieved brain context passages
  -> [Text]             -- ^ Available tool names
  -> m CognitiveDecision
evaluateTasks model tasks contextDocs availableTools = do
  let contextText =
        if null contextDocs
          then "No knowledge base documents retrieved."
          else T.intercalate "\n---\n" [TL.toStrict (pageContent d) | d <- contextDocs]

  evals <- mapM (\t -> evaluateSingleTask model t contextText availableTools) tasks
  let activeTools = nub [tool | TaskEvaluation { evalNeededTool = Just tool } <- evals]
  let allComp = all evalIsCompletable evals

  pure CognitiveDecision
    { evaluatedTasks = evals
    , toolsToActivate = activeTools
    , allCompletable = allComp
    }

-- | Evaluate a single user task
evaluateSingleTask
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> UserTask
  -> Text               -- ^ Concatenated context
  -> [Text]             -- ^ Available tools
  -> m TaskEvaluation
evaluateSingleTask model UserTask {..} contextText availableTools = do
  let toolsListStr = if null availableTools then "None" else T.intercalate ", " availableTools
  let promptText =
        "You are an expert cognitive execution evaluator. Assess if the following task can be completely and accurately answered using ONLY the provided knowledge context, or if an external tool is required.\n\n"
          <> "Available Tools: " <> toolsListStr <> "\n\n"
          <> "Knowledge Context:\n" <> T.take 2500 contextText <> "\n\n"
          <> "Task to Evaluate:\n" <> taskQuery <> "\n\n"
          <> "Format your answer EXACTLY as:\n"
          <> "Completable: <yes or no>\n"
          <> "Tool: <name of required tool or none>\n"
          <> "Reasoning: <brief explanation>\n"

  let msg = textMessage User promptText
  aiMsg <- invoke model [msg] Nothing
  let resp = extractMessageText aiMsg
  pure $ parseTaskEvaluation taskId resp

parseTaskEvaluation :: Text -> Text -> TaskEvaluation
parseTaskEvaluation tId rawText =
  let lns = map T.strip (T.lines rawText)
      compStr = T.toLower (extractPrefix "Completable:" lns)
      toolStr = extractPrefix "Tool:" lns
      reasonStr = extractPrefix "Reasoning:" lns
      isComp = "yes" `T.isInfixOf` compStr || "true" `T.isInfixOf` compStr
      mbTool =
        if T.null toolStr || T.toLower toolStr == "none"
          then Nothing
          else Just toolStr
   in TaskEvaluation
        { evalTaskId = tId
        , evalIsCompletable = isComp
        , evalNeededTool = mbTool
        , evalReasoning = if T.null reasonStr then "Evaluated against context." else reasonStr
        }
  where
    extractPrefix pfx linesList =
      case filter (T.isPrefixOf pfx) linesList of
        (m : _) -> T.strip (T.drop (T.length pfx) m)
        _ -> ""

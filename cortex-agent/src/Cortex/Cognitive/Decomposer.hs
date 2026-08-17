{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Cognitive.Decomposer
Description : Quivr-inspired Cognitive Multi-Task Decomposer
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Decomposes complex multi-intent user requests and conversation history into
explicit system instructions, reasoning rationale, and a list of atomic, self-contained tasks.
-}
module Cortex.Cognitive.Decomposer
  ( UserTask (..)
  , SplittedInput (..)
  , decomposeQuery
  , parseDecomposerOutput
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel, Role (..), extractMessageText, invoke, textMessage)

-- | Atomic self-contained task derived from user question
data UserTask = UserTask
  { taskId :: !Text
  , taskQuery :: !Text
  , taskIsCompletable :: !Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON UserTask
instance FromJSON UserTask

-- | Structured output from cognitive task decomposition
data SplittedInput = SplittedInput
  { splitInstructions :: !Text
  , splitReasoning :: !Text
  , splitTasks :: ![UserTask]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SplittedInput
instance FromJSON SplittedInput

-- | Decompose a complex user input into atomic tasks using LLM
decomposeQuery
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> Text               -- ^ User input query
  -> [Text]             -- ^ Recent chat history messages
  -> m SplittedInput
decomposeQuery model query history = do
  let historySection =
        if null history
          then "No prior conversation."
          else T.intercalate "\n" history

  let promptText =
        "You are an expert cognitive task planner. Decompose the user request into clear instructions and atomic, self-contained sub-tasks.\n\n"
          <> "Recent Conversation History:\n" <> historySection <> "\n\n"
          <> "User Request:\n" <> query <> "\n\n"
          <> "Output your analysis in this EXACT format:\n"
          <> "Instructions: <overall intent or system guidance>\n"
          <> "Reasoning: <step-by-step reasoning on why the query was decomposed this way>\n"
          <> "Tasks:\n"
          <> "1. <First self-contained sub-question>\n"
          <> "2. <Second self-contained sub-question>\n"

  let msg = textMessage User promptText
  aiMsg <- invoke model [msg] Nothing
  let rawResponse = extractMessageText aiMsg
  pure $ parseDecomposerOutput query rawResponse

-- | Parse raw model text into structured SplittedInput
parseDecomposerOutput :: Text -> Text -> SplittedInput
parseDecomposerOutput fallbackQuery rawText =
  let linesList = map T.strip (T.lines rawText)
      inst = extractPrefix "Instructions:" linesList
      reason = extractPrefix "Reasoning:" linesList
      taskLines = extractTaskList linesList
      tasks =
        if null taskLines
          then [UserTask "task-1" fallbackQuery False]
          else [UserTask ("task-" <> T.pack (show i)) t False | (i, t) <- zip [1 :: Int ..] taskLines]
   in SplittedInput
        { splitInstructions = if T.null inst then "Process user request accurately." else inst
        , splitReasoning = if T.null reason then "Direct query execution." else reason
        , splitTasks = tasks
        }
  where
    extractPrefix pfx lns =
      case filter (T.isPrefixOf pfx) lns of
        (firstMatch : _) -> T.strip (T.drop (T.length pfx) firstMatch)
        _ -> ""

    extractTaskList lns =
      [ T.strip (T.dropWhile (\c -> isDigit c || c == '.' || c == '-' || c == ' ') line)
      | line <- lns
      , isTaskLine line
      ]

    isTaskLine l =
      not (T.null l)
        && not (T.isPrefixOf "Instructions:" l)
        && not (T.isPrefixOf "Reasoning:" l)
        && not (T.isPrefixOf "Tasks:" l)
        && (T.isPrefixOf "1." l || T.isPrefixOf "2." l || T.isPrefixOf "3." l || T.isPrefixOf "4." l || T.isPrefixOf "- " l || T.isPrefixOf "* " l)

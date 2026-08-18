{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Research.Planner
Description : Autonomous Deep Research Outline & Query Planner
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Generates structured research plans, subtopic outlines, and targeted search queries
from user queries and initial exploratory search signals (GPT-Researcher style).
-}
module Cortex.Research.Planner
  ( ResearchSubTopic (..)
  , ResearchPlan (..)
  , planResearchOutline
  , parseResearchPlan
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel, Role (..), extractMessageText, invoke, textMessage)

-- | Subtopic to explore during deep research
data ResearchSubTopic = ResearchSubTopic
  { subtopicTitle :: !Text
  , subtopicSearchQueries :: ![Text]
  , subtopicGoal :: !Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ResearchSubTopic
instance FromJSON ResearchSubTopic

-- | Comprehensive multi-pass research plan
data ResearchPlan = ResearchPlan
  { mainQuery :: !Text
  , researchOutline :: ![ResearchSubTopic]
  , estimatedDepth :: !Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ResearchPlan
instance FromJSON ResearchPlan

-- | Plan a comprehensive deep research outline using LLM
planResearchOutline
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> Text               -- ^ Main research topic
  -> [Text]             -- ^ Initial exploratory search snippets
  -> m ResearchPlan
planResearchOutline model topic initialSnippets = do
  let initialContext =
        if null initialSnippets
          then "No preliminary search results."
          else T.intercalate "\n" (take 3 initialSnippets)

  let promptText =
        "You are an expert Chief Research Editor. Create a comprehensive deep research outline for the following topic.\n\n"
          <> "Research Topic: " <> topic <> "\n\n"
          <> "Initial Exploratory Context:\n" <> initialContext <> "\n\n"
          <> "Generate 3 to 4 distinct subtopics. Format your response EXACTLY as:\n\n"
          <> "Subtopic: <Title of Subtopic 1>\n"
          <> "Goal: <What this section aims to discover>\n"
          <> "Queries: <query 1> | <query 2>\n\n"
          <> "Subtopic: <Title of Subtopic 2>\n"
          <> "Goal: <Goal for subtopic 2>\n"
          <> "Queries: <query 1> | <query 2>\n"

  let msg = textMessage User promptText
  aiMsg <- invoke model [msg] Nothing
  let resp = extractMessageText aiMsg
  pure $ parseResearchPlan topic resp

-- | Parse raw model outline text into structured ResearchPlan
parseResearchPlan :: Text -> Text -> ResearchPlan
parseResearchPlan topic rawText =
  let blocks = splitIntoSubtopicBlocks (T.lines rawText)
      subtopics = [st | Just st <- map parseBlock blocks]
      finalSubtopics =
        if null subtopics
          then [ResearchSubTopic topic [topic <> " overview", topic <> " details"] "Investigate primary topic"]
          else subtopics
   in ResearchPlan
        { mainQuery = topic
        , researchOutline = finalSubtopics
        , estimatedDepth = 2
        }
  where
    splitIntoSubtopicBlocks [] = []
    splitIntoSubtopicBlocks (l : ls)
      | T.isPrefixOf "Subtopic:" (T.strip l) =
          let (content, rest) = break (\x -> T.isPrefixOf "Subtopic:" (T.strip x)) ls
           in (l : content) : splitIntoSubtopicBlocks rest
      | otherwise = splitIntoSubtopicBlocks ls

    parseBlock blk =
      let lns = map T.strip blk
          title = extractPrefix "Subtopic:" lns
          goal = extractPrefix "Goal:" lns
          queriesStr = extractPrefix "Queries:" lns
          queries = filter (not . T.null) $ map T.strip (T.splitOn "|" queriesStr)
       in if T.null title
            then Nothing
            else
              Just
                ResearchSubTopic
                  { subtopicTitle = title
                  , subtopicSearchQueries = if null queries then [title] else queries
                  , subtopicGoal = if T.null goal then "Investigate " <> title else goal
                  }

    extractPrefix pfx linesList =
      case filter (T.isPrefixOf pfx) linesList of
        (m : _) -> T.strip (T.drop (T.length pfx) m)
        _ -> ""

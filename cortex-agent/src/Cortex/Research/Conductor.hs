{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Research.Conductor
Description : Recursive Subtopic Tree Exploration & Research Conductor
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Coordinates recursive subtopic exploration, invokes search and concurrent web scrapers,
extracts thematic insights with LLMs, and builds the cumulative research context graph.
-}
module Cortex.Research.Conductor
  ( SubTopicFindings (..)
  , AccumulatedResearch (..)
  , conductSubtopicResearch
  , conductFullResearch
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (nubBy)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Cortex.Research.Planner (ResearchPlan (..), ResearchSubTopic (..))
import Cortex.Research.Scraper (ScrapedSource (..), ScraperConfig (..), scrapeBatchUrls)
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel, Role (..), extractMessageText, invoke, textMessage)

-- | Findings collected for a specific subtopic
data SubTopicFindings = SubTopicFindings
  { findingSubtopic :: !ResearchSubTopic
  , findingSources :: ![ScrapedSource]
  , findingKeyInsights :: ![Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SubTopicFindings
instance FromJSON SubTopicFindings

-- | Full accumulated research state across all subtopics
data AccumulatedResearch = AccumulatedResearch
  { accTopic :: !Text
  , accPlan :: !ResearchPlan
  , accFindings :: ![SubTopicFindings]
  , accAllSources :: ![ScrapedSource]
  }
  deriving (Show, Eq, Generic)

instance ToJSON AccumulatedResearch
instance FromJSON AccumulatedResearch

-- | Conduct focused deep research for a single subtopic
conductSubtopicResearch
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> ScraperConfig
  -> (Text -> IO [Text])          -- ^ Search engine resolver (query -> URLs)
  -> ResearchSubTopic
  -> m SubTopicFindings
conductSubtopicResearch model scraperCfg searchFn subtopic@ResearchSubTopic {..} = do
  -- 1. Execute searches for subtopic queries
  allUrls <- liftIO $ do
    urlLists <- mapM searchFn subtopicSearchQueries
    pure $ concat urlLists

  -- 2. Scrape source contents concurrently
  sources <- scrapeBatchUrls scraperCfg allUrls

  -- 3. Extract key insights aligned with subtopic research goal
  let contextPassages = [sourceTitle s <> ":\n" <> sourceContent s | s <- sources]
  let contextText = T.intercalate "\n---\n" (take 5 contextPassages)

  let promptText =
        "You are an expert research analyst. Extract 3 to 5 key factual findings and insights that directly answer the research goal.\n\n"
          <> "Subtopic: " <> subtopicTitle <> "\n"
          <> "Research Goal: " <> subtopicGoal <> "\n\n"
          <> "Evidence:\n" <> T.take 4000 contextText <> "\n\n"
          <> "List the findings as clear numbered bullet points:"

  let msg = textMessage User promptText
  aiMsg <- invoke model [msg] Nothing
  let insights = filter (not . T.null) $ map T.strip (T.lines (extractMessageText aiMsg))

  pure SubTopicFindings
    { findingSubtopic = subtopic
    , findingSources = sources
    , findingKeyInsights = insights
    }

-- | Conduct full deep research across all planned subtopics
conductFullResearch
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> ScraperConfig
  -> (Text -> IO [Text])          -- ^ Search engine resolver
  -> ResearchPlan
  -> m AccumulatedResearch
conductFullResearch model scraperCfg searchFn plan@ResearchPlan {..} = do
  findings <- mapM (conductSubtopicResearch model scraperCfg searchFn) researchOutline
  let allSrcs = nubBy (\a b -> sourceUrl a == sourceUrl b) (concatMap findingSources findings)
  pure AccumulatedResearch
    { accTopic = mainQuery
    , accPlan = plan
    , accFindings = findings
    , accAllSources = allSrcs
    }

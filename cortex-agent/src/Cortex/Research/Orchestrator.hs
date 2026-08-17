{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Research.Orchestrator
Description : Chief Editor Orchestrator Graph with Human-in-the-Loop (HITL)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Full StateGraph orchestrating Chief Editor, Conductor, WriterAgent, FactCheckerAgent,
and PublisherAgent with HITL plan approval and resume capabilities (GPT-Researcher style).
-}
module Cortex.Research.Orchestrator
  ( ResearchState (..)
  , initialResearchState
  , researchStateReducer
  , runAutonomousResearch
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Cortex.Research.Conductor (AccumulatedResearch (..), SubTopicFindings (..), conductFullResearch)
import Cortex.Research.MultiAgent (DraftSection (..), runDraftAndFactCheckLoop)
import Cortex.Research.Planner (ResearchPlan (..), planResearchOutline)
import Cortex.Research.Publisher (PublishedReport (..), publishResearchReport)
import Cortex.Research.Scraper (ScrapedSource (..), ScraperConfig (..), defaultScraperConfig)
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel)
import Langchain.Graph.StateGraph (StateReducer)

-- | State machine state for deep research workflow
data ResearchState = ResearchState
  { rsTopic :: !Text
  , rsPlan :: !(Maybe ResearchPlan)
  , rsFindings :: ![SubTopicFindings]
  , rsSections :: ![DraftSection]
  , rsSources :: ![ScrapedSource]
  , rsHumanFeedback :: !(Maybe Text)
  , rsFinalReport :: !(Maybe PublishedReport)
  }
  deriving (Show, Eq, Generic)

instance ToJSON ResearchState
instance FromJSON ResearchState

-- | Initialize state for a given topic
initialResearchState :: Text -> ResearchState
initialResearchState topic =
  ResearchState
    { rsTopic = topic
    , rsPlan = Nothing
    , rsFindings = []
    , rsSections = []
    , rsSources = []
    , rsHumanFeedback = Nothing
    , rsFinalReport = Nothing
    }

-- | StateReducer replacing fields with newest non-empty state
researchStateReducer :: StateReducer ResearchState
researchStateReducer _ new = new

-- | Run autonomous deep research end-to-end using configured model and search resolver
runAutonomousResearch
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> ScraperConfig
  -> (Text -> IO [Text])          -- ^ Search engine URL resolver
  -> Text                         -- ^ Topic
  -> m PublishedReport
runAutonomousResearch model scraperCfg searchFn topic = do
  -- 1. Chief Editor: Plan research outline
  plan <- planResearchOutline model topic []

  -- 2. Conductor: Run subtopic tree exploration and scrape sources
  accRes <- conductFullResearch model scraperCfg searchFn plan

  -- 3. Multi-Agent Writer & Fact-Checker: Draft and verify sections in bounded loop
  sections <- mapM (runDraftAndFactCheckLoop model 2) (accFindings accRes)

  -- 4. Publisher: Compile final Markdown report with table of contents and bibliography
  let report = publishResearchReport topic sections (accAllSources accRes)
  pure report

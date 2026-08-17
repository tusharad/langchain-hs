{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Cortex.Prelude
Description : Canonical umbrella re-export module for Cortex-Agent
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Re-exports all Brain management, Knowledge ingestion, Hybrid RAG, Cognitive query routing,
Autonomous deep research, Dynamic flow components, and Telemetry broadcaster types and functions.
-}
module Cortex.Prelude
  ( -- * Brain & Multi-Tenant Knowledge
    BrainId (..)
  , BrainConfig (..)
  , Brain (..)
  , BrainStore (..)
  , defaultBrainConfig
  , newBrainStore
  , createBrain
  , getBrain
  , listBrains
  , updateBrainConfig
  , deleteBrain

    -- * Ingestion & Hybrid Retrieval
  , IngestionConfig (..)
  , IngestedDocument (..)
  , defaultIngestionConfig
  , ingestText
  , ingestFile
  , generateDocumentSummary
  , BrainRetriever (..)
  , newBrainRetriever
  , queryBrain
  , queryBrainWithRerank

    -- * Cognitive Routing & Task Decomposition (Quivr-style)
  , UserTask (..)
  , SplittedInput (..)
  , decomposeQuery
  , TaskEvaluation (..)
  , CognitiveDecision (..)
  , evaluateTasks
  , CognitiveFinalAnswer (..)
  , rewriteSystemPrompt
  , synthesizeCognitiveResponse

    -- * Autonomous Deep Research Engine (GPT-Researcher-style)
  , ScrapedSource (..)
  , ScraperConfig (..)
  , defaultScraperConfig
  , scrapeUrl
  , scrapeBatchUrls
  , ResearchSubTopic (..)
  , ResearchPlan (..)
  , planResearchOutline
  , SubTopicFindings (..)
  , AccumulatedResearch (..)
  , conductSubtopicResearch
  , conductFullResearch
  , DraftSection (..)
  , FactCheckReview (..)
  , runDraftAndFactCheckLoop
  , PublishedReport (..)
  , publishResearchReport
  , ResearchState (..)
  , runAutonomousResearch

    -- * Dynamic Flows (Langflow-style)
  , buildCortexComponentRegistry
  , promptComponent
  , llmComponent
  , brainRetrieverComponent
  , scraperComponent

    -- * Real-time Event Streaming Server
  , CortexEventBroadcaster (..)
  , newCortexEventBroadcaster
  , emitCortexEvent
  , subscribeCortexEvents
  , broadcastDecomposedTask
  , broadcastScrapeProgress
  , broadcastFactCheck
  , broadcastCitation

    -- * Langchain Core Re-exports
  , module Langchain.Prelude
  ) where

import Cortex.Brain
import Cortex.Cognitive.Decomposer
import Cortex.Cognitive.Evaluator
import Cortex.Cognitive.Synthesizer
import Cortex.Flow.Components
import Cortex.Knowledge.Ingestion
import Cortex.Knowledge.Retriever
import Cortex.Research.Conductor
import Cortex.Research.MultiAgent
import Cortex.Research.Orchestrator
import Cortex.Research.Planner
import Cortex.Research.Publisher
import Cortex.Research.Scraper
import Cortex.Server
import Langchain.Prelude

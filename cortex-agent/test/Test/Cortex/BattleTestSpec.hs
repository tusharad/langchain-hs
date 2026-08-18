{-# LANGUAGE OverloadedStrings #-}

module Test.Cortex.BattleTestSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Cortex.Brain (BrainId (..), defaultBrainConfig)
import Cortex.Cognitive.Decomposer (decomposeQuery, splitInstructions, splitTasks, taskQuery)
import Cortex.Cognitive.Evaluator (allCompletable, evaluateTasks, evaluatedTasks)
import Cortex.Cognitive.Synthesizer (ansDetails, ansTasksCompleted, synthesizeCognitiveResponse)
import Cortex.Knowledge.Ingestion (IngestedDocument (..), defaultIngestionConfig, ingestText)
import Cortex.Knowledge.Retriever (newBrainRetriever, queryBrain)
import Cortex.Research.MultiAgent (draftContent, draftTitle, writeDraftSection)
import Cortex.Research.Planner (planResearchOutline, researchOutline, subtopicTitle)
import Cortex.Research.Publisher (publishResearchReport, reportMarkdown)
import Cortex.Research.Scraper (ScrapedSource (..))
import Cortex.Research.Conductor (SubTopicFindings (..))
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Provider.Ollama (newOllama)

tests :: TestTree
tests = testGroup "Cortex.BattleTest (Live Ollama: Qwen 3.5 9b & Llama 3.2)"
  [ testCase "Deep Research: Live Llama 3.2 plans outline and drafts fact-checked section" $ do
      model <- newOllama "llama3.2"
      ePlan <- runExceptT $ planResearchOutline model "Haskell Software Transactional Memory" []
      case ePlan of
        Left err -> assertFailure ("Planner error: " ++ show err)
        Right plan -> do
          assertBool "Generated subtopics" (not (null (researchOutline plan)))
          let firstSubtopic = head (researchOutline plan)
          assertBool "Subtopic has title" (not (T.null (subtopicTitle firstSubtopic)))

          let mockSource = ScrapedSource
                { sourceUrl = "https://haskell.org/stm"
                , sourceTitle = "Haskell STM Invariants"
                , sourceContent = "Software Transactional Memory allows composable atomic memory transactions in Haskell using TVars and atomically blocks without deadlocks."
                , sourceWordCount = 20
                }
          let findings = SubTopicFindings firstSubtopic [mockSource] ["STM enables dead-lock free composable transactions."]

          eDraft <- runExceptT $ writeDraftSection model findings
          case eDraft of
            Left err -> assertFailure ("Draft error: " ++ show err)
            Right draft -> do
              assertBool "Draft has content" (not (T.null (draftContent draft)))
              let report = publishResearchReport "Haskell STM" [draft] [mockSource]
              assertBool "Report contains Markdown table of contents" (T.isInfixOf "## Table of Contents" (reportMarkdown report))

  , testCase "Cognitive Router: Live Llama 3.2 decomposes multi-part query and synthesizes response" $ do
      model <- newOllama "llama3.2"
      let userQuery = "How do pure functional languages handle state, and what is the difference between IORef and TVar in Haskell?"
      eDecomp <- runExceptT $ decomposeQuery model userQuery []
      case eDecomp of
        Left err -> assertFailure ("Decomposer error: " ++ show err)
        Right splitted -> do
          assertBool "Decomposed into tasks" (not (null (splitTasks splitted)))
          let doc = Document
                { pageContent = "IORef provides non-transactional mutable references in IO, while TVar provides transactional mutable variables managed by STM."
                , metadata = Map.fromList [("source", "https://haskell.org/state"), ("brain_id", "haskell-brain")]
                }

          eDec <- runExceptT $ evaluateTasks model (splitTasks splitted) [doc] ["web_search"]
          case eDec of
            Left err -> assertFailure ("Evaluator error: " ++ show err)
            Right dec -> do
              assertBool "Tasks evaluated" (not (null (evaluatedTasks dec)))
              eAns <- runExceptT $ synthesizeCognitiveResponse model "You are an expert Haskell compiler engineer." splitted [doc]
              case eAns of
                Left err -> assertFailure ("Synthesizer error: " ++ show err)
                Right ans -> do
                  ansTasksCompleted ans @?= True
                  assertBool "Details non-empty" (not (T.null (ansDetails ans)))

  , testCase "Enterprise Brain: Live Qwen 3.5 9b ingests with chunk headers and runs hybrid RRF retrieval" $ do
      model <- newOllama "qwen3.5:9b"
      let bId = BrainId "tech-brain"
          rawContent = "LangChain Haskell v3 is an effect-polymorphic framework for AI agent orchestration. It provides pure GADT pipelines, state graphs, and hybrid search combining BM25 and vector stores."

      ingested <- ingestText model (defaultIngestionConfig bId) "LangChain-HS Guide" rawContent
      assertBool "Chunks generated" (docChunksCount ingested >= 1)

      let docs = docChunks ingested
          mockVecSearch _ _ = pure docs
          brainRetriever = newBrainRetriever model bId docs mockVecSearch

      eRetrieved <- runExceptT $ queryBrain brainRetriever "effect-polymorphic framework pipelines"
      case eRetrieved of
        Left err -> assertFailure ("QueryBrain error: " ++ show err)
        Right results -> do
          assertBool "Retrieved matching chunk" (not (null results))
          let matchedContent = TL.toStrict (pageContent (head results))
          assertBool "Contains header" (T.isInfixOf "Header: brain_id" matchedContent)
  ]

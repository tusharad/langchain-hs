{-# LANGUAGE OverloadedStrings #-}

module Test.Cortex.ResearchSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Cortex.Research.Conductor
import Cortex.Research.MultiAgent
import Cortex.Research.Planner
import Cortex.Research.Publisher
import Cortex.Research.Scraper
import Langchain.Core.Model (MockModel (..), newMockModel)

tests :: TestTree
tests = testGroup "Cortex.Research"
  [ testCase "extractCleanHtmlText strips scripts and styles" $ do
      let html = "<html><head><title>Test Page</title><style>.cls{color:red}</style></head><body><script>alert(1)</script><h1>Heading</h1><p>Paragraph text content.</p></body></html>"
          cleaned = extractCleanHtmlText html
      assertBool "Heading present" (T.isInfixOf "Heading" cleaned)
      assertBool "Paragraph present" (T.isInfixOf "Paragraph text content" cleaned)
      assertBool "Script stripped" (not (T.isInfixOf "alert" cleaned))
      assertBool "Style stripped" (not (T.isInfixOf "color:red" cleaned))

  , testCase "parseResearchPlan parses subtopics and queries" $ do
      let raw = "Subtopic: Haskell Concurrency\nGoal: Discover STM mechanisms\nQueries: haskell stm | transactional memory\n\nSubtopic: Rust Concurrency\nGoal: Discover borrow checker\nQueries: rust lifetimes | send sync\n"
          plan = parseResearchPlan "Concurrency in Systems" raw
      mainQuery plan @?= "Concurrency in Systems"
      length (researchOutline plan) @?= 2
      subtopicTitle (head (researchOutline plan)) @?= "Haskell Concurrency"
      subtopicSearchQueries (head (researchOutline plan)) @?= ["haskell stm", "transactional memory"]

  , testCase "runDraftAndFactCheckLoop terminates and produces draft" $ do
      let draftText = "Transactional memory guarantees composable concurrency without deadlocks [1]."
          model = newMockModel draftText
          subtopic = ResearchSubTopic "STM" ["haskell stm"] "Understand STM"
          source = ScrapedSource "https://haskell.org" "Haskell" "STM content" 100
          findings = SubTopicFindings subtopic [source] ["STM is atomic"]
      eRes <- runExceptT $ runDraftAndFactCheckLoop model 2 findings
      case eRes of
        Left err -> assertFailure ("Draft loop error: " ++ show err)
        Right draft -> do
          draftTitle draft @?= "STM"
          assertBool "Content present" (T.isInfixOf "Transactional memory" (draftContent draft))

  , testCase "publishResearchReport formats table of contents and bibliography" $ do
      let draft = DraftSection "Concurrency" "Body content on concurrency." ["https://source.org"]
          source = ScrapedSource "https://source.org" "Source Title" "Content" 150
          report = publishResearchReport "Modern Concurrency" [draft] [source]
      reportTitle report @?= "Modern Concurrency"
      assertBool "TOC present" (T.isInfixOf "## Table of Contents" (reportMarkdown report))
      assertBool "Section present" (T.isInfixOf "## Concurrency" (reportMarkdown report))
      assertBool "Bibliography present" (T.isInfixOf "## References & Evidence Sources" (reportMarkdown report))
      assertBool "Source URL in table" (T.isInfixOf "https://source.org" (reportMarkdown report))
  ]

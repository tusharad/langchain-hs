{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Research.MultiAgent
Description : Multi-Agent Drafting & Fact-Checking Bounded Revision Loop
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Coordinates the DraftWriterAgent and FactCheckerAgent in a bounded verification loop.
Claims in each section are fact-checked against source evidence; if confidence < threshold,
the FactChecker sends critical feedback back to the Writer for iterative revision.
-}
module Cortex.Research.MultiAgent
  ( DraftSection (..)
  , FactCheckReview (..)
  , MultiAgentDraftResult (..)
  , writeDraftSection
  , factCheckSection
  , reviseDraftSection
  , runDraftAndFactCheckLoop
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import GHC.Generics (Generic)

import Cortex.Research.Conductor (SubTopicFindings (..))
import Cortex.Research.Planner (ResearchSubTopic (..))
import Cortex.Research.Scraper (ScrapedSource (..))
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel, Role (..), extractMessageText, invoke, textMessage)

-- | Drafted section of the research report
data DraftSection = DraftSection
  { draftTitle :: !Text
  , draftContent :: !Text
  , draftCitations :: ![Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON DraftSection
instance FromJSON DraftSection

-- | Fact-checking verification score and feedback
data FactCheckReview = FactCheckReview
  { fcPassed :: !Bool
  , fcConfidence :: !Double
  , fcCritique :: !Text
  , fcRequiresRevision :: !Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON FactCheckReview
instance FromJSON FactCheckReview

-- | Final result of multi-agent draft + fact-check pipeline
data MultiAgentDraftResult = MultiAgentDraftResult
  { finalSections :: ![DraftSection]
  , totalRevisionRounds :: !Int
  , overallFactCheck :: !FactCheckReview
  }
  deriving (Show, Eq, Generic)

instance ToJSON MultiAgentDraftResult
instance FromJSON MultiAgentDraftResult

-- | Draft a single section using WriterAgent
writeDraftSection
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> SubTopicFindings
  -> m DraftSection
writeDraftSection model SubTopicFindings {..} = do
  let insightsText = T.intercalate "\n" ["- " <> ins | ins <- findingKeyInsights]
  let sourcesText = T.intercalate "\n" ["[" <> T.pack (show idx) <> "] " <> sourceTitle s <> " (" <> sourceUrl s <> ")" | (idx, s) <- zip [1 :: Int ..] findingSources]

  let promptText =
        "You are an expert technical writer. Write a comprehensive, detailed section for a deep research report based on the collected findings.\n\n"
          <> "Section Title: " <> subtopicTitle findingSubtopic <> "\n"
          <> "Goal: " <> subtopicGoal findingSubtopic <> "\n\n"
          <> "Key Findings:\n" <> insightsText <> "\n\n"
          <> "Sources:\n" <> sourcesText <> "\n\n"
          <> "Write 2 to 3 detailed paragraphs explaining the concepts, evidence, and conclusions with inline citations (e.g. [1], [2])."

  let msg = textMessage User promptText
  aiMsg <- invoke model [msg] Nothing
  let content = extractMessageText aiMsg
  let urls = [sourceUrl s | s <- findingSources]

  pure DraftSection
    { draftTitle = subtopicTitle findingSubtopic
    , draftContent = content
    , draftCitations = urls
    }

-- | Fact-check a draft section against original scraped sources
factCheckSection
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> DraftSection
  -> [ScrapedSource]
  -> m FactCheckReview
factCheckSection model DraftSection {..} sources = do
  let evidenceText = T.intercalate "\n---\n" (take 3 [sourceTitle s <> ":\n" <> sourceContent s | s <- sources])

  let promptText =
        "You are a rigorous Fact-Checking Agent. Verify the claims in the draft section against the source evidence.\n\n"
          <> "Section Title: " <> draftTitle <> "\n"
          <> "Draft Content:\n" <> draftContent <> "\n\n"
          <> "Source Evidence:\n" <> T.take 3000 evidenceText <> "\n\n"
          <> "Evaluate whether all claims are supported. Format your answer EXACTLY as:\n"
          <> "Passed: <yes or no>\n"
          <> "Confidence: <0.0 to 1.0>\n"
          <> "Critique: <brief critique or corrections required>\n"

  let msg = textMessage User promptText
  aiMsg <- invoke model [msg] Nothing
  let rawReview = extractMessageText aiMsg
  pure $ parseFactCheckReview rawReview

parseFactCheckReview :: Text -> FactCheckReview
parseFactCheckReview raw =
  let lns = map T.strip (T.lines raw)
      passStr = T.toLower (extractPrefix "Passed:" lns)
      confStr = extractPrefix "Confidence:" lns
      crit = extractPrefix "Critique:" lns
      isPass = "yes" `T.isInfixOf` passStr || "true" `T.isInfixOf` passStr
      confVal = case TR.double (T.filter (\c -> isDigit c || c == '.') confStr) of
                  Right (v, _) -> v
                  Left _ -> if isPass then 0.9 else 0.5
      needsRev = not isPass || confVal < 0.7
   in FactCheckReview
        { fcPassed = isPass
        , fcConfidence = confVal
        , fcCritique = if T.null crit then "Review complete." else crit
        , fcRequiresRevision = needsRev
        }
  where
    extractPrefix pfx linesList =
      case filter (T.isPrefixOf pfx) linesList of
        (m : _) -> T.strip (T.drop (T.length pfx) m)
        _ -> ""

-- | Revise a draft section using writer incorporating fact-checker critique
reviseDraftSection
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> DraftSection
  -> FactCheckReview
  -> [ScrapedSource]
  -> m DraftSection
reviseDraftSection model draft review sources = do
  let promptText =
        "You are revising a section based on Fact-Checker feedback. Fix any unsupported claims or factual inaccuracies.\n\n"
          <> "Section: " <> draftTitle draft <> "\n"
          <> "Original Draft:\n" <> draftContent draft <> "\n\n"
          <> "Fact-Checker Feedback:\n" <> fcCritique review <> "\n\n"
          <> "Please write the revised section text incorporating all corrections:"

  let msg = textMessage User promptText
  aiMsg <- invoke model [msg] Nothing
  pure draft { draftContent = extractMessageText aiMsg }

-- | Run the full draft-and-fact-check loop with max revision bounds (default: max 3 rounds)
runDraftAndFactCheckLoop
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> Int                -- ^ Max revision rounds
  -> SubTopicFindings
  -> m DraftSection
runDraftAndFactCheckLoop model maxRounds findings = do
  initialDraft <- writeDraftSection model findings
  loop 1 initialDraft
  where
    loop roundNum currentDraft
      | roundNum > maxRounds = pure currentDraft
      | otherwise = do
          review <- factCheckSection model currentDraft (findingSources findings)
          if not (fcRequiresRevision review)
            then pure currentDraft
            else do
              revised <- reviseDraftSection model currentDraft review (findingSources findings)
              loop (roundNum + 1) revised

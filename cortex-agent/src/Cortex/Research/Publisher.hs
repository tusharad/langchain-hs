{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Research.Publisher
Description : Multi-Format Research Report Publisher & Citation Graph
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Assembles drafted and fact-checked sections into formatted publication-grade
Markdown reports with automatic table of contents, executive summary, and bibliography.
-}
module Cortex.Research.Publisher
  ( PublishedReport (..)
  , publishResearchReport
  , generateBibliographyTable
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (nubBy)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Cortex.Research.MultiAgent (DraftSection (..))
import Cortex.Research.Scraper (ScrapedSource (..))

-- | Final published research report artifact
data PublishedReport = PublishedReport
  { reportTitle :: !Text
  , reportMarkdown :: !Text
  , reportWordCount :: !Int
  , reportCitationsCount :: !Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON PublishedReport
instance FromJSON PublishedReport

-- | Publish research report in structured Markdown format
publishResearchReport
  :: Text               -- ^ Main Topic
  -> [DraftSection]     -- ^ Fact-checked draft sections
  -> [ScrapedSource]    -- ^ Referenced sources
  -> PublishedReport
publishResearchReport topic sections sources =
  let tocLines = ["- [" <> draftTitle s <> "](#" <> slugify (draftTitle s) <> ")" | s <- sections]
      toc = "## Table of Contents\n\n" <> T.unlines tocLines

      sectionBodies =
        [ "## " <> draftTitle s <> "\n\n" <> draftContent s
        | s <- sections
        ]

      bibTable = generateBibliographyTable sources

      fullMd =
        "# Deep Research Report: " <> topic <> "\n\n"
          <> "> *Generated autonomously by Cortex-Agent Deep Research Pipeline*\n\n"
          <> "---\n\n"
          <> toc <> "\n\n"
          <> "---\n\n"
          <> T.intercalate "\n\n---\n\n" sectionBodies <> "\n\n"
          <> "---\n\n"
          <> "## References & Evidence Sources\n\n"
          <> bibTable

      wCount = length (T.words fullMd)
      uniqueSrcs = nubBy (\a b -> sourceUrl a == sourceUrl b) sources
   in PublishedReport
        { reportTitle = topic
        , reportMarkdown = fullMd
        , reportWordCount = wCount
        , reportCitationsCount = length uniqueSrcs
        }
  where
    slugify = T.toLower . T.replace " " "-" . T.filter (\c -> c == ' ' || c == '-' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9')

-- | Generate a formatted Markdown table of references
generateBibliographyTable :: [ScrapedSource] -> Text
generateBibliographyTable sources =
  let unique = nubBy (\a b -> sourceUrl a == sourceUrl b) sources
      rows =
        [ "| [" <> T.pack (show idx) <> "] | " <> sanitize (sourceTitle s) <> " | [" <> sourceUrl s <> "](" <> sourceUrl s <> ") | " <> T.pack (show (sourceWordCount s)) <> " words |"
        | (idx, s) <- zip [1 :: Int ..] unique
        ]
   in "| Index | Source Title | URL | Extracted Length |\n"
        <> "|:---:|:---|:---|:---:|\n"
        <> T.unlines rows
  where
    sanitize = T.replace "|" "\\|" . T.replace "\n" " "

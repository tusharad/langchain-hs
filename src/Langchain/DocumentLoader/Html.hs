{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.DocumentLoader.Html
Description : HTML document loader with tag stripping and title/heading extraction
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Loads HTML files, strips styling/scripts/markup, and produces clean text documents.
-}
module Langchain.DocumentLoader.Html
  ( HtmlLoader (..)
  , defaultHtmlLoader
  , extractCleanHtml
  ) where

import Control.Exception (try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as TS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Text.HTML.TagSoup as TSoup

import Langchain.Core.Error (LangchainError, documentLoaderError)
import Langchain.DocumentLoader.Core (BaseLoader (..), Document (..))
import Langchain.TextSplitter.Character (defaultCharacterSplitterOps, splitText)

-- | Configuration options for HTML loader
data HtmlLoader = HtmlLoader
  { htmlFilePath :: FilePath
  , htmlPreserveHeadings :: Bool
  , htmlSplitter :: Maybe (Text -> [Text])
  }

-- | Default HTML loader configuration
defaultHtmlLoader :: FilePath -> HtmlLoader
defaultHtmlLoader path =
  HtmlLoader
    { htmlFilePath = path
    , htmlPreserveHeadings = True
    , htmlSplitter = Nothing
    }

instance BaseLoader HtmlLoader where
  load loader = do
    contentRes <- liftIO $ try $ TLIO.readFile (htmlFilePath loader)
    content <- case contentRes of
      Left err ->
        throwError $
          documentLoaderError
            (TS.pack $ "Failed to read HTML file: " ++ show (err :: IOError))
            (Just "HtmlLoader")
            Nothing
      Right c -> pure c

    let (title, text) = extractCleanHtml content
        baseMeta =
          Map.insert "source" (String $ TS.pack (htmlFilePath loader)) $
            case title of
              Just t -> Map.singleton "title" (String t)
              Nothing -> Map.empty
    pure [Document text baseMeta]

  loadAndSplit loader = do
    docs <- load loader
    let splitter = case htmlSplitter loader of
          Just s -> s
          Nothing -> splitText defaultCharacterSplitterOps
    pure $ concatMap (splitter . pageContent) docs

-- | Extract title and clean textual body from raw HTML string
extractCleanHtml :: Text -> (Maybe TS.Text, Text)
extractCleanHtml rawHtml =
  let strictHtml = TS.pack (TL.unpack rawHtml)
      tags = TSoup.parseTags strictHtml
      title = extractTitle tags
      cleanTxt = cleanTags tags
   in (title, TL.pack (TS.unpack cleanTxt))

extractTitle :: [TSoup.Tag TS.Text] -> Maybe TS.Text
extractTitle tags =
  let titleSections = TSoup.partitions (TSoup.isTagOpenName "title") tags
   in case listToMaybe titleSections of
        Nothing -> Nothing
        Just r ->
          let (titleInner, _) = break (TSoup.isTagCloseName "title") r
           in Just $ TS.strip $ TSoup.innerText titleInner

cleanTags :: [TSoup.Tag TS.Text] -> TS.Text
cleanTags tags =
  let filteredTags = filterNoiseTags False tags
      rawText = TSoup.innerText filteredTags
   in normalizeWhitespace rawText

-- Strip content within <script>, <style>, <noscript>
filterNoiseTags :: Bool -> [TSoup.Tag TS.Text] -> [TSoup.Tag TS.Text]
filterNoiseTags _ [] = []
filterNoiseTags inNoise (t : ts)
  | TSoup.isTagOpenName "script" t || TSoup.isTagOpenName "style" t || TSoup.isTagOpenName "noscript" t =
      filterNoiseTags True ts
  | TSoup.isTagCloseName "script" t || TSoup.isTagCloseName "style" t || TSoup.isTagCloseName "noscript" t =
      filterNoiseTags False ts
  | inNoise = filterNoiseTags True ts
  | otherwise = t : filterNoiseTags False ts

normalizeWhitespace :: TS.Text -> TS.Text
normalizeWhitespace t =
  let textLines = map TS.strip (TS.lines t)
      nonEmptyLines = filter (not . TS.null) textLines
   in TS.intercalate "\n" nonEmptyLines

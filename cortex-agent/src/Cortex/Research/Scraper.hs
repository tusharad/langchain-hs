{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Research.Scraper
Description : Autonomous Concurrent Web Scraper & Context Curator
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Asynchronously fetches web pages with concurrency controls, extracts clean article text,
deduplicates sources, and enforces word-budget context pruning (GPT-Researcher style).
-}
module Cortex.Research.Scraper
  ( ScrapedSource (..)
  , ScraperConfig (..)
  , defaultScraperConfig
  , scrapeUrl
  , scrapeBatchUrls
  , extractCleanHtmlText
  , pruneContextToWordBudget
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.List (nubBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Simple
import Text.HTML.TagSoup

-- | Scraped document source metadata and text
data ScrapedSource = ScrapedSource
  { sourceUrl :: !Text
  , sourceTitle :: !Text
  , sourceContent :: !Text
  , sourceWordCount :: !Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ScrapedSource
instance FromJSON ScrapedSource

-- | Configuration for web scraping and context curation
data ScraperConfig = ScraperConfig
  { maxConcurrentRequests :: !Int
  , requestTimeoutSeconds :: !Int
  , maxWordsPerSource :: !Int
  , maxTotalContextWords :: !Int
  , userAgent :: !String
  }
  deriving (Show, Eq)

-- | Default scraping configuration (25k max total words safety margin)
defaultScraperConfig :: ScraperConfig
defaultScraperConfig =
  ScraperConfig
    { maxConcurrentRequests = 5
    , requestTimeoutSeconds = 10
    , maxWordsPerSource = 3000
    , maxTotalContextWords = 25000
    , userAgent = "Mozilla/5.0 (compatible; CortexResearchBot/1.0; +https://github.com/tusharadhatrao/langchain-hs)"
    }

-- | Scrape a single URL with error recovery
scrapeUrl :: MonadIO m => ScraperConfig -> Text -> m (Maybe ScrapedSource)
scrapeUrl ScraperConfig {..} url = liftIO $ do
  eResp <- (try $ do
    req <- parseRequest (T.unpack url)
    let reqWithHeaders =
          setRequestMethod "GET" $
            setRequestHeader "User-Agent" [TE.encodeUtf8 (T.pack userAgent)] $
              setRequestHeader "Accept" ["text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"] $
                setRequestResponseTimeout (responseTimeoutMicro (requestTimeoutSeconds * 1000000)) req
    httpLBS reqWithHeaders) :: IO (Either SomeException (Response LBS.ByteString))

  case eResp of
    Left _ -> pure Nothing
    Right resp -> do
      let bodyStrict = TE.decodeUtf8With (\_ _ -> Just ' ') (LBS.toStrict (getResponseBody resp))
          cleanText = extractCleanHtmlText bodyStrict
          wordsList = T.words cleanText
          wCount = length wordsList
          cappedText = T.unwords (take maxWordsPerSource wordsList)
      pure $ Just ScrapedSource
        { sourceUrl = url
        , sourceTitle = extractTitle bodyStrict url
        , sourceContent = cappedText
        , sourceWordCount = min wCount maxWordsPerSource
        }

-- | Concurrently scrape a batch of URLs and deduplicate
scrapeBatchUrls :: MonadIO m => ScraperConfig -> [Text] -> m [ScrapedSource]
scrapeBatchUrls cfg urls = liftIO $ do
  let uniqueUrls = nubBy (==) urls
  results <- mapConcurrently (scrapeUrl cfg) uniqueUrls
  let successful = [s | Just s <- results, not (T.null (sourceContent s))]
  pure $ pruneContextToWordBudget (maxTotalContextWords cfg) successful

-- | Extract text while stripping script, style, and navigation tags
extractCleanHtmlText :: Text -> Text
extractCleanHtmlText html =
  let tags = parseTags (T.unpack html)
      filtered = filterTags tags
      textOnly = innerText filtered
      cleanedLines = filter (not . T.null) $ map (T.strip . T.pack) (lines textOnly)
   in T.unwords cleanedLines
  where
    filterTags [] = []
    filterTags (TagOpen name _ : rest)
      | name `elem` ["script", "style", "noscript", "svg", "nav", "footer", "header"] =
          filterTags (dropUntilClose name rest)
    filterTags (t : rest) = t : filterTags rest

    dropUntilClose _ [] = []
    dropUntilClose targetName (TagClose name : rest)
      | name == targetName = rest
    dropUntilClose targetName (_ : rest) = dropUntilClose targetName rest

-- | Extract title tag from HTML or fallback to URL
extractTitle :: Text -> Text -> Text
extractTitle html fallback =
  let tags = parseTags (T.unpack html)
      titleTags = dropWhile (~/= ("<title>" :: String)) tags
   in case titleTags of
        (_ : TagText t : _) -> T.strip (T.pack t)
        _ -> fallback

-- | Prune collected sources to fit within overall word budget
pruneContextToWordBudget :: Int -> [ScrapedSource] -> [ScrapedSource]
pruneContextToWordBudget budget = go 0 []
  where
    go _ acc [] = reverse acc
    go curWords acc (s : rest)
      | curWords + sourceWordCount s <= budget =
          go (curWords + sourceWordCount s) (s : acc) rest
      | otherwise =
          let remainingWords = budget - curWords
           in if remainingWords > 100
                then
                  let trimmed = s { sourceContent = T.unwords (take remainingWords (T.words (sourceContent s))), sourceWordCount = remainingWords }
                   in reverse (trimmed : acc)
                else reverse acc

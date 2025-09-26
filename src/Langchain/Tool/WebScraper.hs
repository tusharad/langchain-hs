{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Tool.WebScraper
Description : Tool for scrapping text content from URL
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

WebScraper is a tool that scrapes text content from a given URL.
It fetches the HTML content of the page, extracts the body text, removes scripts, and strips class/id/style attributes from the HTML tags.
It is designed to be used with the Langchain framework for building language models and applications.
-}
module Langchain.Tool.WebScraper (WebScraper (..), WebPageInfo (..), fetchAndScrape) where

import Control.Exception (SomeException, try)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Langchain.Tool.Core
import Langchain.Tool.Utils
import Network.HTTP.Simple
import qualified Text.HTML.TagSoup as TS

-- | Represents a web scraper tool that extracts content from web pages
data WebScraper = WebScraper
  deriving (Show)

-- | Stores the extracted webpage information
data WebPageInfo = WebPageInfo
  { pageTitle :: Maybe Text
  , pageContent :: Text
  }
  deriving (Show, Generic)

-- Make WebPageInfo serializable to JSON
instance ToJSON WebPageInfo

-- | Input type for the WebScraper - just a URL
type ScraperInput = Text

-- | Implement the Tool typeclass for WebScraper
instance Tool WebScraper where
  type Input WebScraper = ScraperInput
  type Output WebScraper = (Either String Text)

  toolName _ = "web_scraper"

  toolDescription _ =
    "Scrapes content from a webpage. Provide a valid URL, and it will extract only the textual body content "
      <> "with scripts removed and without class/id/style attributes."

  runTool _ url = do
    result <- fetchAndScrape url
    case result of
      Left err -> pure $ Left $ "Error scraping webpage: " <> err
      Right info -> pure $ Right $ pageContent info

-- | Fetch HTML content from a URL and extract webpage information
fetchAndScrape :: Text -> IO (Either String WebPageInfo)
fetchAndScrape url = do
  request_ <- parseRequest (T.unpack url)
  eResp <- try $ httpLBS request_ :: IO (Either SomeException (Response LBS.ByteString))
  case eResp of
    Left err -> pure $ Left (show err)
    Right r -> do
      let rBody = getResponseBody r
      let htmlContent = TE.decodeUtf8 $ LBS.toStrict rBody

      -- Clean and extract the content
      let tags = TS.parseTags htmlContent
      let title = extractTitle tags
      let cleanedContent = cleanBodyContent tags

      pure $ Right $ WebPageInfo title cleanedContent

-- | Extract the title from parsed HTML tags
extractTitle :: [TS.Tag Text] -> Maybe Text
extractTitle tags =
  let titleTags = TS.partitions (TS.isTagOpenName "title") tags
   in if null titleTags
        then Nothing
        else case listToMaybe titleTags of
          Nothing -> Nothing
          Just r -> Just $ T.strip $ TS.innerText r

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Tool.WebScraper
Description : Tool for scraping text content from URL
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Web scraper tool built on effect-polymorphic 'Tool m'.
-}
module Langchain.Tool.WebScraper
  ( WebScraper (..)
  , WebPageInfo (..)
  , webScraperTool
  , fetchAndScrape
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, Value (..), object, (.:?), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Simple
import qualified Text.HTML.TagSoup as TS

import Langchain.Core.Error (toolError)
import Langchain.Core.Tool (Tool (..), createTool)
import Langchain.Tool.Utils (cleanBodyContent)

-- | Represents a web scraper tool configuration
data WebScraper = WebScraper
  deriving (Show, Eq)

-- | Stores the extracted webpage information
data WebPageInfo = WebPageInfo
  { pageTitle :: Maybe Text
  , pageContent :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Fetch HTML content from a URL and extract webpage information
fetchAndScrape :: MonadIO m => Text -> m (Either Text WebPageInfo)
fetchAndScrape url = liftIO $ do
  eReq <- try (parseRequest (T.unpack url))
  case eReq of
    Left err -> pure $ Left $ T.pack $ show (err :: SomeException)
    Right request_ -> do
      eResp <- try (httpLBS request_) :: IO (Either SomeException (Response LBS.ByteString))
      case eResp of
        Left err -> pure $ Left $ T.pack (show err)
        Right r -> do
          let rBody = getResponseBody r
              htmlContent = TE.decodeUtf8 $ LBS.toStrict rBody
              tags = TS.parseTags htmlContent
              title = extractTitle tags
              cleanedContent = cleanBodyContent tags
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

-- | Effect-polymorphic WebScraper Tool
webScraperTool :: MonadIO m => Tool m
webScraperTool =
  createTool
    "web_scraper"
    "Scrapes content from a webpage URL, removing scripts and HTML formatting"
    ( object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
              ["url" .= object ["type" .= ("string" :: Text)]]
        , "required" .= (["url"] :: [Text])
        ]
    )
    ( \args -> case args of
        Object o -> case parseEither (.:? "url") o of
          Right (Just u) -> do
            eRes <- fetchAndScrape u
            case eRes of
              Left err -> pure $ Left $ toolError err (Just "web_scraper") Nothing
              Right info -> pure $ Right $ pageContent info
          _ -> pure $ Left $ toolError "Missing 'url' field" (Just "web_scraper") Nothing
        _ -> pure $ Left $ toolError "Invalid arguments object" (Just "web_scraper") Nothing
    )

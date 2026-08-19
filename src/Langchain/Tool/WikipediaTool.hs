{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Tool.WikipediaTool
Description : Tool for extracting wikipedia content.
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Langchain.Tool.WikipediaTool
  ( -- * Configuration
    WikipediaTool (..)
  , defaultWikipediaTool
  , wikipediaTool

    -- * Parameters
  , defaultTopK
  , defaultDocMaxChars
  , defaultLanguageCode

    -- * Responses & Search
  , SearchResponse (..)
  , SearchQuery (..)
  , Page (..)
  , SearchResult (..)
  , Pages (..)
  , PageResponse (..)
  , searchWikipedia
  ) where

import Control.Exception (try)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), Value (..), decode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (parseEither)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Langchain.Core.Error (toolError)
import Langchain.Core.Tool (Tool (..), createTool)
import Langchain.Tool.Utils (cleanHtmlContent)
import Network.HTTP.Simple

-- | Wikipedia search tool configuration
data WikipediaTool = WikipediaTool
  { topK :: Int
  , docMaxChars :: Int
  , languageCode :: Text
  }
  deriving (Eq, Show)

defaultTopK :: Int
defaultTopK = 1

defaultDocMaxChars :: Int
defaultDocMaxChars = 2000

defaultLanguageCode :: Text
defaultLanguageCode = "en"

defaultWikipediaTool :: WikipediaTool
defaultWikipediaTool =
  WikipediaTool
    { topK = defaultTopK
    , docMaxChars = defaultDocMaxChars
    , languageCode = defaultLanguageCode
    }

-- | Search result item
data SearchResult = SearchResult
  { ns :: Int
  , title_ :: Text
  , pageid :: Int
  , size :: Int
  , wordcount :: Int
  , snippet :: Text
  , timestamp :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \v ->
    SearchResult
      <$> v .: "ns"
      <*> v .: "title"
      <*> v .: "pageid"
      <*> v .: "size"
      <*> v .: "wordcount"
      <*> v .: "snippet"
      <*> v .: "timestamp"

newtype SearchQuery = SearchQuery
  { search :: [SearchResult]
  }
  deriving (Show, Generic, Eq)

instance FromJSON SearchQuery where
  parseJSON = withObject "SearchQuery" $ \v ->
    SearchQuery <$> v .: "search"

newtype SearchResponse = SearchResponse
  { query :: SearchQuery
  }
  deriving (Show, Generic, Eq)

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \v ->
    SearchResponse <$> v .: "query"

data Page = Page
  { title :: Text
  , extract :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON Page where
  parseJSON = withObject "Page" $ \v ->
    Page <$> v .: "title" <*> v .: "extract"

newtype Pages = Pages
  { pages :: Map Text Page
  }
  deriving (Show, Generic, Eq)

instance FromJSON Pages where
  parseJSON = withObject "Pages" $ \v ->
    Pages <$> v .: "pages"

newtype PageResponse = PageResponse
  { query :: Pages
  }
  deriving (Show, Generic, Eq)

instance FromJSON PageResponse where
  parseJSON = withObject "PageResponse" $ \v ->
    PageResponse <$> v .: "query"

-- | Execute Wikipedia query HTTP request
searchWikipedia :: MonadIO m => WikipediaTool -> Text -> m (Either Text Text)
searchWikipedia WikipediaTool {..} queryTxt = liftIO $ do
  let searchUrl =
        "https://"
          <> T.unpack languageCode
          <> ".wikipedia.org/w/api.php?action=query&list=search&srsearch="
          <> T.unpack queryTxt
          <> "&format=json"
  initReq <- parseRequest searchUrl
  eRes <- try (httpLBS initReq)
  case eRes of
    Left err -> pure $ Left (T.pack $ show (err :: IOError))
    Right res -> case decode (getResponseBody res) of
      Nothing -> pure $ Left "Failed to decode Wikipedia search response"
      Just SearchResponse {query = SearchQuery results} -> do
        let topResults = take topK results
        pageTexts <- forM topResults $ \r -> do
          let pageUrl =
                "https://"
                  <> T.unpack languageCode
                  <> ".wikipedia.org/w/api.php?action=query&prop=extracts&exintro&explaintext&pageids="
                  <> show (pageid r)
                  <> "&format=json"
          pReq <- parseRequest pageUrl
          epRes <- try (httpLBS pReq)
          case epRes of
            Left pErr -> pure $ "Error fetching page: " <> T.pack (show (pErr :: IOError))
            Right pRes -> case decode (getResponseBody pRes) of
              Nothing -> pure "Failed to decode page response"
              Just (PageResponse (Pages pMap)) -> case M.lookup (T.pack $ show (pageid r)) pMap of
                Nothing -> pure "Page not found"
                Just page -> pure $ T.take docMaxChars (cleanHtmlContent $ extract page)
        pure $ Right $ T.unlines pageTexts

-- | Create effect-polymorphic Tool from WikipediaTool
wikipediaTool :: MonadIO m => WikipediaTool -> Tool m
wikipediaTool cfg =
  createTool
    "Wikipedia"
    "A wrapper around Wikipedia. Useful for searching general knowledge."
    ( object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
              ["query" .= object ["type" .= ("string" :: Text)]]
        , "required" .= (["query"] :: [Text])
        ]
    )
    ( \case
        Object o -> case parseEither (.:? "query") o of
          Right (Just q) -> do
            eRes <- searchWikipedia cfg q
            case eRes of
              Left err -> pure $ Left $ toolError err (Just "Wikipedia") Nothing
              Right txt -> pure $ Right txt
          _ -> pure $ Left $ toolError "Missing 'query' field" (Just "Wikipedia") Nothing
        _ -> pure $ Left $ toolError "Invalid arguments object" (Just "Wikipedia") Nothing
    )

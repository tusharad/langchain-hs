{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Tool.WikipediaTool
Description : Tool for extracting wikipedia content.
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Langchain.Tool.WikipediaTool
  ( -- * Configuration
    WikipediaTool (..)
  , defaultWikipediaTool

    -- * Parameters
  , defaultTopK
  , defaultDocMaxChars
  , defaultLanguageCode

    -- * Internal types
  , SearchQuery (..)
  , SearchResponse (..)
  , Page (..)
  , SearchResult (..)
  , Pages (..)
  , PageResponse (..)
  ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), decode, withObject, (.:))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Langchain.Runnable.Core (Runnable (..))
import Langchain.Tool.Core
import Langchain.Tool.Utils (cleanHtmlContent)
import Network.HTTP.Simple

{- |
Wikipedia search tool configuration
The tool uses Wikipedia's API to perform searches and retrieve page extracts.

Example configuration:

> customTool = WikipediaTool
>   { topK = 3
>   , docMaxChars = 1000
>   , languageCode = "es"
>   }
-}
data WikipediaTool = WikipediaTool
  { topK :: Int
  -- ^ Number of Wikipedia pages to include in the result.
  , docMaxChars :: Int
  -- ^ Number of characters to take from each page.
  , languageCode :: Text
  -- ^ Language code to use (e.g., "en" for English).
  }
  deriving (Eq, Show)

-- | Default value for top K
defaultTopK :: Int
defaultTopK = 1

-- | Default value for max chars
defaultDocMaxChars :: Int
defaultDocMaxChars = 2000

-- | Default language
defaultLanguageCode :: Text
defaultLanguageCode = "en"

{- |
Wikipedia search tool configuration
The tool uses Wikipedia's API to perform searches and retrieve page extracts.

Example configuration:

> customTool = WikipediaTool
>   { topK = 3
>   , docMaxChars = 1000
>   , languageCode = "es"
>   }
-}
defaultWikipediaTool :: WikipediaTool
defaultWikipediaTool =
  WikipediaTool
    { topK = defaultTopK
    , docMaxChars = defaultDocMaxChars
    , languageCode = defaultLanguageCode
    }

-- | Tool instance for WikipediaTool.
instance Tool WikipediaTool where
  type Input WikipediaTool = Text

  -- \^ Natural language search query (e.g., "Quantum computing")

  type Output WikipediaTool = Text

  -- \^ Concatenated page extracts with separators

  -- \|
  --  Returns "Wikipedia" as the tool identifier
  --
  --  >>> toolName (undefined :: WikipediaTool)
  --  "Wikipedia"
  --
  toolName _ = "Wikipedia"

  -- \|
  --  Provides a description for LLM agents:
  --
  --  >>> toolDescription (undefined :: WikipediaTool)
  --  "A wrapper around Wikipedia. Useful for answering..."
  --
  toolDescription _ =
    "A wrapper around Wikipedia. Useful for answering general questions about people, places, companies, facts, historical events, or other subjects. Input should be a single worded search query."

  -- \|
  --  Executes Wikipedia search and content retrieval.
  --  Handles API calls and response parsing, returning concatenated extracts.
  --
  --  Example flow:
  --
  --  1. Perform search query
  --  2. Retrieve top K page IDs
  --  3. Fetch and truncate page content
  --  4. Combine results with separators
  --
  --  Throws exceptions on:
  --
  --  - API request failures
  --  - JSON parsing errors
  --  - Missing page content
  --
  runTool = searchWiki

-- | Perform a Wikipedia search and retrieve page extracts.
searchWiki :: WikipediaTool -> Text -> IO Text
searchWiki tool q = do
  SearchResponse {..} <- performSearch tool q
  if null (search query)
    then return "no wikipedia pages found"
    else do
      let pageIds = map pageid (take (topK tool) (search query))
      pages <- mapM (getPage tool) pageIds
      let extracts =
            map
              ( T.take (docMaxChars tool)
                  . cleanHtmlContent
                  . extract
              )
              pages
      return $ T.intercalate "\n\n" extracts

-- | Perform a search on Wikipedia.
performSearch :: WikipediaTool -> Text -> IO SearchResponse
performSearch tool q = do
  let params =
        M.fromList
          [ ("format", "json")
          , ("action", "query")
          , ("list", "search")
          , ("srsearch", T.unpack q)
          , ("srlimit", show (topK tool))
          ]
      url =
        T.pack $
          "https://"
            <> T.unpack (languageCode tool)
            <> ".wikipedia.org/w/api.php?"
            <> urlEncode params
  request <- parseRequest (T.unpack url)
  response <- httpLbs request
  let body = getResponseBody response
  case decode body of
    Just result -> return result
    Nothing -> throwIO $ userError "Failed to decode search response"

-- | Get a page extract from Wikipedia.
getPage :: WikipediaTool -> Int -> IO Page
getPage tool pageId = do
  let params =
        M.fromList
          [ ("format", "json")
          , ("action", "query")
          , ("prop", "extracts")
          , ("pageids", show pageId)
          ]
      url =
        T.pack $
          "https://"
            <> T.unpack (languageCode tool)
            <> ".wikipedia.org/w/api.php?"
            <> urlEncode params
  request <- parseRequest (T.unpack url)
  response <- httpLbs request
  let body = getResponseBody response
  case decode body of
    Just (PageResponse (Pages p)) -> case M.lookup (show pageId) p of
      Just page -> return page
      Nothing -> throwIO $ userError "Page not found in response"
    Nothing -> throwIO $ userError "Failed to decode page response"

-- | URL encode a map of parameters.
urlEncode :: Map String String -> String
urlEncode = concatMap (\(k, v) -> k ++ "=" ++ v ++ "&") . M.toList

-- | Data types for JSON parsing.
newtype SearchResponse = SearchResponse
  { query :: SearchQuery
  }
  deriving (Show, Generic, FromJSON)

-- | Type for list of search result
newtype SearchQuery = SearchQuery
  { search :: [SearchResult]
  }
  deriving (Show)

instance FromJSON SearchQuery where
  parseJSON = withObject "SearchQuery" $ \v ->
    SearchQuery
      <$> v .: "search"

-- | Result of SearchResult
data SearchResult = SearchResult
  { ns :: Int
  , title_ :: Text
  , pageid :: Int
  , size :: Int
  , wordcount :: Int
  , snippet :: Text
  , timestamp :: Text
  }
  deriving (Show)

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

-- | Wikipedia response
newtype PageResponse = PageResponse
  { query :: Pages
  }
  deriving (Generic, Eq, Show, FromJSON)

-- | Collection of Wikipedia pages, where key is page id
newtype Pages = Pages
  { pages :: Map String Page
  }
  deriving (Generic, Eq, Show, FromJSON)

-- | Represents wikipedia page
data Page = Page
  { title :: Text
  , extract :: Text
  }
  deriving (Show, Eq)

instance FromJSON Page where
  parseJSON = withObject "Page" $ \v ->
    Page
      <$> v .: "title"
      <*> v .: "extract"

{- |
Implements Runnable compatibility layer
Note: The current implementation returns 'Right' values only,
though the type signature allows for future error handling.

Example usage:

> response <- invoke defaultWikipediaTool "Artificial intelligence"
> case response of
>   Right content -> putStrLn content
>   Left err -> print err
-}
instance Runnable WikipediaTool where
  type RunnableInput WikipediaTool = Text
  type RunnableOutput WikipediaTool = Text

  -- TODO: runTool should return an Either
  invoke tool input = Right <$> runTool tool input

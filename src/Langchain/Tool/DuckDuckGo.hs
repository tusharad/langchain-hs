{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Tool.DuckDuckGo
Description : Tool for extracting DuckDuckGo search content
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

DuckDuckGo search tool built with effect-polymorphic 'Tool m'.
-}
module Langchain.Tool.DuckDuckGo
  ( duckDuckGoTool
  , searchDuckDuckGo
  , DuckDuckGoResponse (..)
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Simple

import Langchain.Core.Error (toolError)
import Langchain.Core.Tool (Tool (..), createTool)

-- | Icon data within related topics
newtype Icon = Icon
  { iconURL :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Icon where
  parseJSON = withObject "Icon" $ \v ->
    Icon <$> v .:? "URL"

-- | A single related topic
data RelatedTopic = RelatedTopic
  { topicFirstURL :: Maybe Text
  , topicIcon :: Maybe Icon
  , topicResult :: Maybe Text
  , topicText :: Maybe Text
  , topicName :: Maybe Text
  , topicTopics :: Maybe [RelatedTopic]
  }
  deriving (Show, Eq, Generic)

instance FromJSON RelatedTopic where
  parseJSON = withObject "RelatedTopic" $ \v ->
    RelatedTopic
      <$> v .:? "FirstURL"
      <*> v .:? "Icon"
      <*> v .:? "Result"
      <*> v .:? "Text"
      <*> v .:? "Name"
      <*> v .:? "Topics"

-- | Meta information about the source
data MetaDeveloper = MetaDeveloper
  { devName :: Text
  , devURL :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON MetaDeveloper where
  parseJSON = withObject "MetaDeveloper" $ \v ->
    MetaDeveloper
      <$> v .: "name"
      <*> v .: "url"

-- | Source options within meta information
data MetaSrcOptions = MetaSrcOptions
  { isMediaWiki :: Maybe Int
  , isWikipedia :: Maybe Int
  , language :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON MetaSrcOptions where
  parseJSON = withObject "MetaSrcOptions" $ \v ->
    MetaSrcOptions
      <$> v .:? "is_mediawiki"
      <*> v .:? "is_wikipedia"
      <*> v .:? "language"

-- | Meta information about the response
data Meta = Meta
  { metaDescription :: Maybe Text
  , metaDeveloper :: Maybe [MetaDeveloper]
  , metaName :: Maybe Text
  , metaPerlModule :: Maybe Text
  , metaSrcDomain :: Maybe Text
  , metaSrcName :: Maybe Text
  , metaSrcOptions :: Maybe MetaSrcOptions
  }
  deriving (Show, Eq, Generic)

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \v ->
    Meta
      <$> v .:? "description"
      <*> v .:? "developer"
      <*> v .:? "name"
      <*> v .:? "perl_module"
      <*> v .:? "src_domain"
      <*> v .:? "src_name"
      <*> v .:? "src_options"

-- | DuckDuckGo API response
data DuckDuckGoResponse = DuckDuckGoResponse
  { abstract :: Text
  , abstractSource :: Text
  , abstractText :: Text
  , abstractURL :: Text
  , answer :: Text
  , answerType :: Text
  , definition :: Text
  , definitionSource :: Text
  , definitionURL :: Text
  , entity :: Text
  , heading :: Text
  , image :: Text
  , imageHeight :: Int
  , imageIsLogo :: Int
  , imageWidth :: Int
  , infobox :: Text
  , redirect :: Text
  , relatedTopics :: [RelatedTopic]
  , results :: [Value]
  , resultType :: Text
  , meta :: Maybe Meta
  }
  deriving (Show, Eq, Generic)

instance FromJSON DuckDuckGoResponse where
  parseJSON = withObject "DuckDuckGoResponse" $ \v ->
    DuckDuckGoResponse
      <$> v .: "Abstract"
      <*> v .: "AbstractSource"
      <*> v .: "AbstractText"
      <*> v .: "AbstractURL"
      <*> v .: "Answer"
      <*> v .: "AnswerType"
      <*> v .: "Definition"
      <*> v .: "DefinitionSource"
      <*> v .: "DefinitionURL"
      <*> v .: "Entity"
      <*> v .: "Heading"
      <*> v .: "Image"
      <*> v .: "ImageHeight"
      <*> v .: "ImageIsLogo"
      <*> v .: "ImageWidth"
      <*> v .: "Infobox"
      <*> v .: "Redirect"
      <*> v .: "RelatedTopics"
      <*> v .: "Results"
      <*> v .: "Type"
      <*> v .:? "meta"

-- | Search DuckDuckGo given a query string
searchDuckDuckGo :: MonadIO m => Text -> m (Either Text Text)
searchDuckDuckGo queryData = liftIO $ do
  let searchTerm = T.replace " " "+" (T.strip queryData)
      urlString = "https://duckduckgo.com/?q=" <> T.unpack searchTerm <> "&format=json"
  eResult <-
    ( do
        request <- parseRequest urlString
        response <- httpLbs request
        let body = getResponseBody response
        case eitherDecode body of
          Left err -> pure $ Left $ T.pack $ show err
          Right ddgResponse_ -> pure $ Right ddgResponse_
    )
      `catch` \e -> pure $ Left $ T.pack $ show (e :: SomeException)
  case eResult of
    Left err -> pure $ Left err
    Right r -> pure $ Right $ ddgToText r

-- | Effect-polymorphic DuckDuckGo Tool
duckDuckGoTool :: MonadIO m => Tool m
duckDuckGoTool =
  createTool
    "duckduckgo"
    "Performs web searches using DuckDuckGo and returns structured information about results"
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
            eRes <- searchDuckDuckGo q
            case eRes of
              Left err -> pure $ Left $ toolError err (Just "duckduckgo") Nothing
              Right txt -> pure $ Right txt
          _ -> pure $ Left $ toolError "Missing 'query' field" (Just "duckduckgo") Nothing
        _ -> pure $ Left $ toolError "Invalid arguments object" (Just "duckduckgo") Nothing
    )

-- | Converts a DuckDuckGoResponse into a concise textual summary.
ddgToText :: DuckDuckGoResponse -> Text
ddgToText resp =
  T.intercalate "\n\n" $
    catMaybes
      [ Just ("# " <> heading resp)
      , abstractSection resp
      , answerSection resp
      , definitionSection resp
      , relatedTopicsSection (relatedTopics resp)
      ]

abstractSection :: DuckDuckGoResponse -> Maybe Text
abstractSection resp = do
  abst <- if T.null (abstract resp) then Nothing else Just (abstract resp)
  url <- if T.null (abstractURL resp) then Nothing else Just (abstractURL resp)
  Just $ "Abstract: " <> abst <> "\nSource: " <> url

answerSection :: DuckDuckGoResponse -> Maybe Text
answerSection resp =
  if T.null (answer resp)
    then Nothing
    else Just ("Answer: " <> answer resp)

definitionSection :: DuckDuckGoResponse -> Maybe Text
definitionSection resp = do
  def <- if T.null (definition resp) then Nothing else Just (definition resp)
  url <-
    if T.null (definitionURL resp)
      then Nothing
      else Just (definitionURL resp)
  Just $ "Definition: " <> def <> "\nSource: " <> url

relatedTopicsSection :: [RelatedTopic] -> Maybe Text
relatedTopicsSection rts =
  let processed = concatMap processRelatedTopic rts
   in if null processed then Nothing else Just (T.unlines processed)

processRelatedTopic :: RelatedTopic -> [Text]
processRelatedTopic rt =
  case (topicName rt, topicTopics rt) of
    (Just name, Just subtopics) ->
      ("*" <> name <> "*") : concatMap processRelatedTopic subtopics
    _ ->
      case (topicText rt, topicFirstURL rt) of
        (Just text, Just url) -> ["- [" <> text <> "](" <> url <> ")"]
        _ -> []

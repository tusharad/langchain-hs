{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Tool.DuckDuckGo
Description : Tool for extracting DuckDuckGo search content
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Please note: DuckDuckGo Tool only returns result if the search term has a abstract card
-}
module Langchain.Tool.DuckDuckGo (DuckDuckGo (..)) where

import Control.Exception (SomeException, catch)
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Langchain.Tool.Core
import Network.HTTP.Simple

-- | Icon data within related topics
data Icon = Icon
  { iconURL :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Icon where
  parseJSON = withObject "Icon" $ \v ->
    Icon
      <$> v .:? "URL"

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
  , resultType :: Text -- Called "Type" in the API
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

{-
-- | Error type for DuckDuckGo API calls
data DuckDuckGoError
  = NetworkError Text
  | ParseError Text
  | OtherError Text
  deriving (Show, Eq, Generic)

instance ToJSON DuckDuckGoError where
  toJSON (NetworkError msg) = object ["type" .= ("network" :: Text), "message" .= msg]
  toJSON (ParseError msg) = object ["type" .= ("parse" :: Text), "message" .= msg]
  toJSON (OtherError msg) = object ["type" .= ("other" :: Text), "message" .= msg]
  -}

-- | Query parameter for DuckDuckGo search
newtype DuckDuckGoQuery = DuckDuckGoQuery
  { query :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON DuckDuckGoQuery where
  toJSON q = object ["query" .= query q]

-- | The DuckDuckGo tool data type
data DuckDuckGo = DuckDuckGo
  deriving (Show, Eq)

-- | Tool instance for DuckDuckGo
instance Tool DuckDuckGo where
  type Input DuckDuckGo = Text
  type Output DuckDuckGo = Text

  toolName _ = "duckduckgo"

  toolDescription _ =
    "Performs web searches using DuckDuckGo and returns structured information about results"

  runTool _ queryData = do
    let searchTerm = T.replace " " "+" (T.strip queryData)
    let urlString =
          "https://duckduckgo.com/?q="
            <> T.unpack searchTerm
            <> "&format=json"
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
      Left err -> pure err
      Right r -> pure $ ddgToText r

-- | Converts a DuckDuckGoResponse into a concise textual summary suitable for LLM input.
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
  url <- if T.null (definitionURL resp) then Nothing else Just (definitionURL resp)
  Just $ "Definition: " <> def <> "\nSource: " <> url

relatedTopicsSection :: [RelatedTopic] -> Maybe Text
relatedTopicsSection rts =
  let processed = concatMap processRelatedTopic rts
   in if null processed then Nothing else Just (T.unlines processed)

processRelatedTopic :: RelatedTopic -> [Text]
processRelatedTopic rt =
  case (topicName rt, topicTopics rt) of
    -- Handle categorized group
    (Just name, Just subtopics) ->
      ("*" <> name <> "*") : concatMap processRelatedTopic subtopics
    -- Handle individual topic
    _ ->
      case (topicText rt, topicFirstURL rt) of
        (Just text, Just url) -> ["- [" <> text <> "](" <> url <> ")"]
        _ -> []

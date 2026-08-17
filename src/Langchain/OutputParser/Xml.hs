{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.OutputParser.Xml
Description : Tag-based XML output parser for extracting structured elements
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Parses XML-tagged responses from language models into typed key-value pairs or domain types.
-}
module Langchain.OutputParser.Xml
  ( XmlOutputParser (..)
  , XmlElement (..)
  , newXmlOutputParser
  , parseXmlElements
  , parseXmlOutput
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS

import Langchain.Core.Error (LangchainError, parsingError)
import Langchain.OutputParser.Core (OutputParser (..))

-- | Represents a parsed XML tag with its textual content
data XmlElement = XmlElement
  { xmlTag :: !Text
  , xmlContent :: !Text
  }
  deriving (Show, Eq)

-- | Configurable XML parser that parses XML into type 'a'
data XmlOutputParser a = XmlOutputParser
  { targetTags :: ![Text]
  , xmlTransform :: [XmlElement] -> Either Text a
  }

-- | Construct a new XmlOutputParser
newXmlOutputParser :: [Text] -> ([XmlElement] -> Either Text a) -> XmlOutputParser a
newXmlOutputParser = XmlOutputParser

-- | Parse raw text containing XML tags into a list of XmlElement
parseXmlElements :: Text -> [XmlElement]
parseXmlElements rawText =
  let tags = TS.parseTags rawText
   in go tags
  where
    go [] = []
    go (TS.TagOpen tagName _ : rest) =
      let (contentTags, after) = break (isMatchingClose tagName) rest
          innerContent = T.strip $ TS.innerText contentTags
          elemObj = XmlElement tagName innerContent
          children = go contentTags
          remaining = case after of
            (_ : r) -> r
            [] -> []
       in elemObj : children ++ go remaining
    go (_ : rest) = go rest

    isMatchingClose name (TS.TagClose closeName) = name == closeName
    isMatchingClose _ _ = False

-- | Parse XML text using an XmlOutputParser
parseXmlOutput :: XmlOutputParser a -> Text -> Either Text a
parseXmlOutput XmlOutputParser {..} rawText =
  let elems = parseXmlElements rawText
      filtered =
        if null targetTags
          then elems
          else filter (\e -> xmlTag e `elem` targetTags) elems
   in xmlTransform filtered

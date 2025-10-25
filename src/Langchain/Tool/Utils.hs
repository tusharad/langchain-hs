{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Tool.Utils
Description : Common utility functions for LangChain tool modules
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides utility functions used by various tool implementations,
particularly for HTML content processing and cleaning operations.
-}
module Langchain.Tool.Utils (cleanBodyContent, cleanHtmlContent) where

import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Text.HTML.TagSoup as TS
import qualified Text.StringLike as TS

-- | This function takes a text that contains html tags, and removes them while preserving links
cleanHtmlContent :: Text -> Text
cleanHtmlContent c = extractText (TS.parseTags c)

-- | Clean the HTML content: extract body, remove scripts, and strip attributes
cleanBodyContent :: [TS.Tag Text] -> Text
cleanBodyContent tags =
  let -- Extract only body content
      bodyTags = case TS.partitions (TS.isTagOpenName "body") tags of
        [] -> tags -- If no body tag is found, use all tags
        (bodySection : _) -> bodySection
      filteredTags = removeTags bodyTags
      content = extractText filteredTags
   in content

{-
If the tag is <a> anchor tag, then extract and append the link as well.
-}
extractText :: [TS.Tag Text] -> Text
extractText ts = TS.strConcat $ catMaybes (go ts)
  where
    go [] = []
    go ((TS.TagOpen "a" aAttrList) : xs) =
      ( Just "link: "
          <> L.lookup "href" aAttrList
          <> Just " for:"
      )
        : go xs
    go (x : xs) = TS.maybeTagText x : go xs

allowedTags :: [TS.Tag Text -> Bool]
allowedTags =
  textTag
    : ( mkIsTag
          <$> [ "p"
              , "button"
              , "a"
              , "div"
              , "h1"
              , "h2"
              , "h3"
              , "h4"
              , "h5"
              , "h6"
              , "span"
              , "ul"
              , "li"
              , "input"
              , "submit"
              , "label"
              , "option"
              , "select"
              , "textarea"
              , "blockquote"
              , "pre"
              , "code"
              , "strong"
              , "em"
              , "b"
              , "i"
              , "u"
              , "mark"
              , "small"
              , "big"
              ]
      )
  where
    textTag (TS.TagText _) = True
    textTag _ = False
    mkIsTag name tag = isTag tag name

isTag :: TS.Tag Text -> Text -> Bool
isTag (TS.TagOpen name _) t = name == t
isTag (TS.TagClose name) t = name == t
isTag _ _ = False

removeTags :: [TS.Tag Text] -> [TS.Tag Text]
removeTags = filter (\t -> any (\f -> f t) allowedTags)

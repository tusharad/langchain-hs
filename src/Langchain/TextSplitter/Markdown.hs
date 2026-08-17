{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.TextSplitter.Markdown
Description : Markdown document header-aware text splitter
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Splits markdown text based on structural headers (# Header 1, ## Header 2, etc.)
and generates chunks with inherited header context.
-}
module Langchain.TextSplitter.Markdown
  ( MarkdownSplitterOps (..)
  , MarkdownChunk (..)
  , defaultMarkdownSplitterOps
  , splitMarkdown
  , splitMarkdownToChunks
  ) where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Langchain.TextSplitter.RecursiveCharacter
  ( RecursiveCharacterSplitterOps (..)
  , defaultRecursiveCharacterSplitterOps
  , splitTextRecursive
  )

-- | Represents a chunk of markdown text with its associated header hierarchy
data MarkdownChunk = MarkdownChunk
  { chunkContent :: Text
  , chunkHeaders :: Map Text Text
  }
  deriving (Show, Eq)

-- | Configuration options for markdown text splitting
data MarkdownSplitterOps = MarkdownSplitterOps
  { mdChunkSize :: Int64
  , mdChunkOverlap :: Int64
  , headersToSplitOn :: [(Text, Text)] -- e.g. [("#", "Header 1"), ("##", "Header 2"), ("###", "Header 3")]
  }
  deriving (Show, Eq)

-- | Default markdown splitter options
defaultMarkdownSplitterOps :: MarkdownSplitterOps
defaultMarkdownSplitterOps =
  MarkdownSplitterOps
    { mdChunkSize = 1000
    , mdChunkOverlap = 100
    , headersToSplitOn =
        [ ("#", "Header 1")
        , ("##", "Header 2")
        , ("###", "Header 3")
        , ("####", "Header 4")
        ]
    }

-- | Split markdown document into MarkdownChunks with header metadata
splitMarkdownToChunks :: MarkdownSplitterOps -> Text -> [MarkdownChunk]
splitMarkdownToChunks _ "" = []
splitMarkdownToChunks ops text =
  let rawLines = T.lines text
      sections = groupLinesByHeaders (headersToSplitOn ops) Map.empty rawLines
   in concatMap (subSplitSection ops) sections

-- | Split markdown text into plain Text chunks
splitMarkdown :: MarkdownSplitterOps -> Text -> [Text]
splitMarkdown ops text = map chunkContent (splitMarkdownToChunks ops text)

-- Group lines into header-annotated sections
groupLinesByHeaders :: [(Text, Text)] -> Map Text Text -> [Text] -> [MarkdownChunk]
groupLinesByHeaders _ _ [] = []
groupLinesByHeaders headerRules currentHeaders ls = go currentHeaders [] ls
  where
    go :: Map Text Text -> [Text] -> [Text] -> [MarkdownChunk]
    go hdrs acc [] =
      if null acc
        then []
        else [MarkdownChunk (T.unlines (reverse acc)) hdrs]
    go hdrs acc (l : rest) =
      case matchHeader headerRules l of
        Just (hPrefix, hName, hTitle) ->
          let currentChunk =
                if null acc
                  then []
                  else [MarkdownChunk (T.unlines (reverse acc)) hdrs]
              -- update headers: clear deeper headers when higher header occurs
              newHdrs = updateHeaderMap headerRules hPrefix hName hTitle hdrs
           in currentChunk ++ go newHdrs [l] rest
        Nothing ->
          go hdrs (l : acc) rest

matchHeader :: [(Text, Text)] -> Text -> Maybe (Text, Text, Text)
matchHeader rules line =
  let stripped = T.stripStart line
   in findRule rules stripped
  where
    findRule [] _ = Nothing
    findRule ((prefix, name) : rs) s =
      let prefixWithSpace = prefix <> " "
       in if prefixWithSpace `T.isPrefixOf` s
            then Just (prefix, name, T.strip (T.drop (T.length prefixWithSpace) s))
            else findRule rs s

updateHeaderMap :: [(Text, Text)] -> Text -> Text -> Text -> Map Text Text -> Map Text Text
updateHeaderMap rules prefix name title curMap =
  let prefixDepth = T.length prefix
      -- keep only headers with depth < prefixDepth
      filtered =
        Map.filterWithKey
          (\k _ -> case lookupPrefixKey rules k of
              Just p -> T.length p < prefixDepth
              Nothing -> True
          )
          curMap
   in Map.insert name title filtered

lookupPrefixKey :: [(Text, Text)] -> Text -> Maybe Text
lookupPrefixKey [] _ = Nothing
lookupPrefixKey ((p, n) : rest) name
  | n == name = Just p
  | otherwise = lookupPrefixKey rest name

subSplitSection :: MarkdownSplitterOps -> MarkdownChunk -> [MarkdownChunk]
subSplitSection ops (MarkdownChunk content hdrs) =
  let cSize = mdChunkSize ops
      cOverlap = mdChunkOverlap ops
   in if T.length content <= cSize
        then [MarkdownChunk content hdrs]
        else
          let subOps =
                defaultRecursiveCharacterSplitterOps
                  { chunkSize = cSize
                  , chunkOverlap = cOverlap
                  , separators = ["\n\n", "\n", " ", ""]
                  }
              subPieces = splitTextRecursive subOps content
           in [MarkdownChunk piece hdrs | piece <- subPieces]

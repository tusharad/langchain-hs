{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.TextSplitter.RecursiveCharacter
Description : Hierarchical recursive character text splitting with chunk overlap
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Recursively splits text by trying different separators in order (paragraphs, lines, spaces, characters)
to keep semantically related pieces of text together.
-}
module Langchain.TextSplitter.RecursiveCharacter
  ( RecursiveCharacterSplitterOps (..)
  , defaultRecursiveCharacterSplitterOps
  , splitTextRecursive
  )
where

import Data.Int (Int64)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- | Configuration options for recursive character text splitter
data RecursiveCharacterSplitterOps = RecursiveCharacterSplitterOps
  { chunkSize :: Int64
  , chunkOverlap :: Int64
  , separators :: [Text]
  }
  deriving (Show, Eq)

-- | Default options: 1000 char chunks, 200 char overlap, standard hierarchy of separators
defaultRecursiveCharacterSplitterOps :: RecursiveCharacterSplitterOps
defaultRecursiveCharacterSplitterOps =
  RecursiveCharacterSplitterOps
    { chunkSize = 1000
    , chunkOverlap = 200
    , separators = ["\n\n", "\n", " ", ""]
    }

-- | Split text recursively using the specified separators hierarchy
splitTextRecursive :: RecursiveCharacterSplitterOps -> Text -> [Text]
splitTextRecursive _ "" = []
splitTextRecursive
  RecursiveCharacterSplitterOps
    { chunkSize = maxSize
    , chunkOverlap = maxOverlap
    , separators = ss
    }
  text = splitRecursive ss text
    where
      splitRecursive :: [Text] -> Text -> [Text]
      splitRecursive [] txt = splitByLen txt
      splitRecursive ("" : _) txt = splitByLen txt
      splitRecursive (sep : seps) txt = mergeWithOverlap sep splitParts
        where
          splitParts = concatMap (splitRecursive seps) $ filter (not . T.null) $ T.splitOn sep txt

      splitByLen :: Text -> [Text]
      splitByLen "" = []
      splitByLen txt = chunk : splitByLen remainder
        where
          (chunk, remainder) = T.splitAt maxSize txt

      mergeWithOverlap :: Text -> [Text] -> [Text]
      mergeWithOverlap _ [] = []
      mergeWithOverlap sep parts = reverse $ go [] 0 [] parts
        where
          sepLen = T.length sep
          toText = T.intercalate sep . reverse

          go :: [Text] -> Int64 -> [Text] -> [Text] -> [Text]
          go acc chunkLen chunkParts pss =
            case pss of
              [] -> acc'
              (part : restParts) ->
                let partLen = T.length part
                    chunkLen' = chunkLen + sepBefore chunkParts + partLen
                 in if chunkLen' <= maxSize
                      then go acc chunkLen' (part : chunkParts) restParts
                      else
                        let (overlapParts, overlapLen) = takeWhileOverlap chunkParts 0 []
                            carryLen = overlapLen + sepBefore overlapParts + partLen
                         in go acc' carryLen (part : reverse overlapParts) restParts
            where
              acc' = toText chunkParts : acc

              sepBefore ps = if null ps then 0 else sepLen

              takeWhileOverlap [] overlapLen overlapAcc = (overlapAcc, overlapLen)
              takeWhileOverlap (overlapPart : restOverlap) overlapLen overlapAcc
                | overlapLen' > maxOverlap = (overlapAcc, overlapLen)
                | otherwise = takeWhileOverlap restOverlap overlapLen' (overlapPart : overlapAcc)
                where
                  overlapLen' = overlapLen + sepBefore overlapAcc + T.length overlapPart

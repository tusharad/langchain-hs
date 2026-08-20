{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Langchain.TextSplitter.RecursiveCharacter
-- Description : Hierarchical recursive character text splitting with chunk overlap
-- Copyright   : (c) 2025-2026 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-- Stability   : experimental
--
-- Recursively splits text by trying different separators in order (paragraphs, lines, spaces, characters)
-- to keep semantically related pieces of text together.
module Langchain.TextSplitter.RecursiveCharacter
  ( RecursiveCharacterSplitterOps (..),
    defaultRecursiveCharacterSplitterOps,
    splitTextRecursive,
  )
where

import Data.Int (Int64)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- | Configuration options for recursive character text splitter
data RecursiveCharacterSplitterOps = RecursiveCharacterSplitterOps
  { chunkSize :: Int64,
    chunkOverlap :: Int64,
    separators :: [Text]
  }
  deriving (Show, Eq)

-- | Default options: 1000 char chunks, 200 char overlap, standard hierarchy of separators
defaultRecursiveCharacterSplitterOps :: RecursiveCharacterSplitterOps
defaultRecursiveCharacterSplitterOps =
  RecursiveCharacterSplitterOps
    { chunkSize = 1000,
      chunkOverlap = 200,
      separators = ["\n\n", "\n", " ", ""]
    }

-- | Split text recursively using the specified separators hierarchy
splitTextRecursive :: RecursiveCharacterSplitterOps -> Text -> [Text]
splitTextRecursive _ "" = []
splitTextRecursive
  RecursiveCharacterSplitterOps
    { chunkSize = maxSize,
      chunkOverlap = maxOverlap,
      separators = ss
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
      mergeWithOverlap sep parts = go [] 0 [] parts
        where
          sepLen = T.length sep
          toText = T.intercalate sep . reverse

          go :: [Text] -> Int64 -> [Text] -> [Text] -> [Text]
          go acc _ [] [] = reverse acc
          go acc _ chunkParts [] = reverse (toText chunkParts : acc)
          go acc chunkLen chunkParts (part : restParts)
            | chunkLen' <= maxSize = go acc chunkLen' (part : chunkParts) restParts
            | partLen > maxSize = go (part : acc') 0 [] restParts
            | otherwise =
                let (overlapParts, overlapLen) = takeWhileOverlap chunkParts 0 []
                    sepBefore = if null overlapParts then 0 else sepLen
                    carryLen = overlapLen + sepBefore + partLen
                 in go acc' carryLen (part : reverse overlapParts) restParts
            where
              partLen = T.length part
              chunkLen' = chunkLen + partLen + if null chunkParts then 0 else sepLen
              acc' = toText chunkParts : acc

              takeWhileOverlap [] overlapLen overlapAcc = (overlapAcc, overlapLen)
              takeWhileOverlap (overlapPart : restOverlap) overlapLen overlapAcc
                | overlapLen' > maxOverlap = (overlapAcc, overlapLen)
                | otherwise = takeWhileOverlap restOverlap overlapLen' (overlapPart : overlapAcc)
                where
                  overlapLen' = overlapLen + T.length overlapPart + if null overlapAcc then 0 else sepLen

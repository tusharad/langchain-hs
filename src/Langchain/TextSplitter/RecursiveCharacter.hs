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
  ) where

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
splitTextRecursive ops text =
  filter (not . T.null) $ splitRecursive (separators ops) text
  where
    cSize = chunkSize ops
    cOverlap = chunkOverlap ops

    splitRecursive :: [Text] -> Text -> [Text]
    splitRecursive [] t
      | T.length t <= cSize = [t]
      | otherwise = splitByLength cSize t
    splitRecursive (sep : restSeps) t
      | T.length t <= cSize = [t]
      | otherwise =
          if sep == ""
            then splitByLength cSize t
            else
              let parts = if T.null sep then map T.singleton (T.unpack t) else T.splitOn sep t
                  goodParts = filter (not . T.null) parts
               in if length goodParts <= 1
                    then splitRecursive restSeps t
                    else mergeAndRecurse restSeps sep goodParts

    mergeAndRecurse :: [Text] -> Text -> [Text] -> [Text]
    mergeAndRecurse restSeps sep parts =
      let subChunks = concatMap (\p -> if T.length p > cSize then splitRecursive restSeps p else [p]) parts
       in mergeChunksWithOverlap cSize cOverlap sep subChunks

    splitByLength :: Int64 -> Text -> [Text]
    splitByLength len t
      | T.null t = []
      | otherwise =
          let (chunk, remainder) = T.splitAt len t
           in chunk : splitByLength len remainder

-- | Merge smaller pieces into chunks up to max chunk size, respecting chunk overlap
mergeChunksWithOverlap :: Int64 -> Int64 -> Text -> [Text] -> [Text]
mergeChunksWithOverlap _ _ _ [] = []
mergeChunksWithOverlap maxLen overlapLen sep pieces = go [] 0 [] pieces
  where
    sepLen = T.length sep

    go :: [Text] -> Int64 -> [Text] -> [Text] -> [Text]
    go acc _ currentAcc [] =
      if null currentAcc
        then reverse acc
        else reverse (joinPieces sep (reverse currentAcc) : acc)
    go acc currentLen currentAcc (p : ps) =
      let pieceLen = T.length p
          additionalLen = if null currentAcc then pieceLen else pieceLen + sepLen
       in if currentLen + additionalLen <= maxLen
            then go acc (currentLen + additionalLen) (p : currentAcc) ps
            else
              let finishedChunk = joinPieces sep (reverse currentAcc)
                  newAcc = finishedChunk : acc
                  -- compute overlap from end of currentAcc
                  overlapPieces = computeOverlapPieces overlapLen sep (reverse currentAcc)
                  overlapLenActual = sum (map T.length overlapPieces) + fromIntegral (max 0 (length overlapPieces - 1)) * sepLen
               in if pieceLen > maxLen
                    then go (p : newAcc) 0 [] ps
                    else
                      go
                        newAcc
                        (overlapLenActual + pieceLen + if null overlapPieces then 0 else sepLen)
                        (p : reverse overlapPieces)
                        ps

    joinPieces :: Text -> [Text] -> Text
    joinPieces = T.intercalate

    computeOverlapPieces :: Int64 -> Text -> [Text] -> [Text]
    computeOverlapPieces targetOverlap s ps
      | targetOverlap <= 0 = []
      | otherwise = takeWhileOverlap targetOverlap s (reverse ps) []

    takeWhileOverlap :: Int64 -> Text -> [Text] -> [Text] -> [Text]
    takeWhileOverlap _ _ [] acc = acc
    takeWhileOverlap target s (p : ps) acc =
      let curLen = sum (map T.length (p : acc)) + fromIntegral (length acc) * T.length s
       in if curLen <= target
            then takeWhileOverlap target s ps (p : acc)
            else acc

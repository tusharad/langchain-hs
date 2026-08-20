{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitter.RecursiveCharacterSpec (tests) where

import Data.Int (Int64)
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.TextSplitter.RecursiveCharacter

splitTextRecursiveLegacy :: RecursiveCharacterSplitterOps -> TL.Text -> [TL.Text]
splitTextRecursiveLegacy _ "" = []
splitTextRecursiveLegacy ops text =
  filter (not . TL.null) $ splitRecursive (separators ops) text
  where
    cSize = chunkSize ops
    cOverlap = chunkOverlap ops

    splitRecursive :: [TL.Text] -> TL.Text -> [TL.Text]
    splitRecursive [] t
      | TL.length t <= cSize = [t]
      | otherwise = splitByLength cSize t
    splitRecursive (sep : restSeps) t
      | TL.length t <= cSize = [t]
      | otherwise =
          if sep == ""
            then splitByLength cSize t
            else
              let parts = if TL.null sep then map TL.singleton (TL.unpack t) else TL.splitOn sep t
                  goodParts = filter (not . TL.null) parts
               in if length goodParts <= 1
                    then splitRecursive restSeps t
                    else mergeAndRecurse restSeps sep goodParts

    mergeAndRecurse :: [TL.Text] -> TL.Text -> [TL.Text] -> [TL.Text]
    mergeAndRecurse restSeps sep parts =
      let subChunks = concatMap (\p -> if TL.length p > cSize then splitRecursive restSeps p else [p]) parts
       in mergeChunksWithOverlapLegacy cSize cOverlap sep subChunks

    splitByLength :: Int64 -> TL.Text -> [TL.Text]
    splitByLength len t
      | TL.null t = []
      | otherwise =
          let (chunk, remainder) = TL.splitAt len t
           in chunk : splitByLength len remainder

mergeChunksWithOverlapLegacy :: Int64 -> Int64 -> TL.Text -> [TL.Text] -> [TL.Text]
mergeChunksWithOverlapLegacy _ _ _ [] = []
mergeChunksWithOverlapLegacy maxLen overlapLen sep pieces = go [] 0 [] pieces
  where
    sepLen = TL.length sep

    go :: [TL.Text] -> Int64 -> [TL.Text] -> [TL.Text] -> [TL.Text]
    go acc _ currentAcc [] =
      if null currentAcc
        then reverse acc
        else reverse (joinPieces sep (reverse currentAcc) : acc)
    go acc currentLen currentAcc (p : ps) =
      let pieceLen = TL.length p
          additionalLen = if null currentAcc then pieceLen else pieceLen + sepLen
       in if currentLen + additionalLen <= maxLen
            then go acc (currentLen + additionalLen) (p : currentAcc) ps
            else
              let finishedChunk = joinPieces sep (reverse currentAcc)
                  newAcc = finishedChunk : acc
                  overlapPieces = computeOverlapPieces overlapLen sep (reverse currentAcc)
                  overlapLenActual = sum (map TL.length overlapPieces) + fromIntegral (max 0 (length overlapPieces - 1)) * sepLen
               in if pieceLen > maxLen
                    then go (p : newAcc) 0 [] ps
                    else
                      go
                        newAcc
                        (overlapLenActual + pieceLen + if null overlapPieces then 0 else sepLen)
                        (p : reverse overlapPieces)
                        ps

    joinPieces :: TL.Text -> [TL.Text] -> TL.Text
    joinPieces = TL.intercalate

    computeOverlapPieces :: Int64 -> TL.Text -> [TL.Text] -> [TL.Text]
    computeOverlapPieces targetOverlap s ps
      | targetOverlap <= 0 = []
      | otherwise = takeWhileOverlap targetOverlap s (reverse ps) []

    takeWhileOverlap :: Int64 -> TL.Text -> [TL.Text] -> [TL.Text] -> [TL.Text]
    takeWhileOverlap _ _ [] acc = acc
    takeWhileOverlap target s (p : ps) acc =
      let curLen = sum (map TL.length (p : acc)) + fromIntegral (length acc) * TL.length s
       in if curLen <= target
            then takeWhileOverlap target s ps (p : acc)
            else acc

legacyEqCase :: RecursiveCharacterSplitterOps -> TL.Text -> Assertion
legacyEqCase ops txt =
  splitTextRecursive ops txt @?= splitTextRecursiveLegacy ops txt

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitter.RecursiveCharacterSpec"
    [ testCase "Empty text returns empty chunk list" $
        splitTextRecursive defaultRecursiveCharacterSplitterOps "" @?= []
    , testCase "Legacy eq: exact chunkSize boundary" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 5, chunkOverlap = 0}
        legacyEqCase ops "abcde"
    , testCase "Legacy eq: chunkSize + 1 boundary" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 5, chunkOverlap = 0}
        legacyEqCase ops "abcdef"
    , testCase "Legacy eq: chunkSize = 1" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 1, chunkOverlap = 0}
        legacyEqCase ops "abcdef"
    , testCase "Legacy eq: separators empty list fallback" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 3, chunkOverlap = 0, separators = []}
        legacyEqCase ops "abcdefgh"
    , testCase "Legacy eq: separators only empty string fallback" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 3, chunkOverlap = 0, separators = [""]}
        legacyEqCase ops "abcdefgh"
    , testCase "Legacy eq: fallback to rest separators when first separator absent" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 6, chunkOverlap = 0, separators = ["@@", "\n", " ", ""]}
        legacyEqCase ops "aa bb cc"
    , testCase "Legacy eq: drops empties from adjacent and edge separators" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 3, chunkOverlap = 0}
        legacyEqCase ops "\n\nA\n\n\n\nB\n\n"
    , testCase "Legacy eq: overlap = 0" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 5, chunkOverlap = 0, separators = ["|", ""]}
        legacyEqCase ops "ab|cd|ef|gh"
    , testCase "Legacy eq: overlap = chunkSize" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 5, chunkOverlap = 5, separators = ["|", ""]}
        legacyEqCase ops "ab|cd|ef|gh"
    , testCase "Legacy eq: overlap > chunkSize" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 5, chunkOverlap = 9, separators = ["|", ""]}
        legacyEqCase ops "ab|cd|ef|gh|ij"
    , testCase "Legacy eq: multi-character separator with overlap" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 8, chunkOverlap = 3, separators = ["||", ""]}
        legacyEqCase ops "ab||cd||ef||gh"
    , testCase "Legacy eq: oversized piece path" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 4, chunkOverlap = 2, separators = ["|", ""]}
        legacyEqCase ops "abcdefgh|ij|kl"
    , testCase "Legacy eq: mixed separators and recursive fallback" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 10, chunkOverlap = 2, separators = ["\n\n", "\n", " ", ""]}
        legacyEqCase ops "p1 line1\n\np2 has many words\nline2"
    , testCase "Invariant: no chunk exceeds chunkSize for valid config" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 7, chunkOverlap = 2}
            chunks = splitTextRecursive ops "a aa aaa aaaa aaaaa"
        assertBool "All chunks must be <= chunkSize" (all (\c -> TL.length c <= chunkSize ops) chunks)
    , testCase "Invariant: all chunks are non-empty" $ do
        let ops = defaultRecursiveCharacterSplitterOps {chunkSize = 4, chunkOverlap = 1}
            chunks = splitTextRecursive ops "\n\nA\n\n\n\nB\n\n"
        assertBool "No empty chunks" (all (not . TL.null) chunks)
    ]

{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.TextSplitter.Token
Description : Token-based text splitting with configurable token counter
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Splits text into chunks of specified maximum token counts with optional overlap.
-}
module Langchain.TextSplitter.Token
  ( TokenSplitterOps (..)
  , defaultTokenSplitterOps
  , splitByTokens
  , countTokensApprox
  ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- | Configuration options for token-based text splitting
data TokenSplitterOps = TokenSplitterOps
  { maxTokens :: Int
  , tokenOverlap :: Int
  , tokenCounter :: Text -> Int
  }

instance Show TokenSplitterOps where
  show ops =
    "TokenSplitterOps { maxTokens = "
      ++ show (maxTokens ops)
      ++ ", tokenOverlap = "
      ++ show (tokenOverlap ops)
      ++ " }"

-- | Approximate token count (roughly 4 characters per token or word-based heuristic)
countTokensApprox :: Text -> Int
countTokensApprox t =
  let wCount = length (T.words t)
      cCount = fromIntegral (T.length t) `div` 4
   in max wCount cCount

-- | Default token splitter options (500 tokens, 50 token overlap)
defaultTokenSplitterOps :: TokenSplitterOps
defaultTokenSplitterOps =
  TokenSplitterOps
    { maxTokens = 500
    , tokenOverlap = 50
    , tokenCounter = countTokensApprox
    }

-- | Split text into chunks bounded by maxTokens
splitByTokens :: TokenSplitterOps -> Text -> [Text]
splitByTokens _ "" = []
splitByTokens ops text =
  let wordsList = T.words text
   in if null wordsList
        then []
        else go [] [] wordsList
  where
    maxT = maxTokens ops
    overlapT = tokenOverlap ops
    count = tokenCounter ops

    go :: [Text] -> [Text] -> [Text] -> [Text]
    go acc currentWords [] =
      if null currentWords
        then reverse acc
        else reverse (T.unwords (reverse currentWords) : acc)
    go acc currentWords (w : ws) =
      let candidate = T.unwords (reverse (w : currentWords))
          tokCount = count candidate
       in if tokCount <= maxT
            then go acc (w : currentWords) ws
            else
              let finishedChunk = T.unwords (reverse currentWords)
                  newAcc = finishedChunk : acc
                  -- Overlap words
                  overlapWords = takeOverlap overlapT (reverse currentWords) []
               in go newAcc (w : overlapWords) ws

    takeOverlap :: Int -> [Text] -> [Text] -> [Text]
    takeOverlap _ [] acc = acc
    takeOverlap target (pw : pws) acc =
      let candidate = T.unwords (pw : acc)
       in if count candidate <= target
            then takeOverlap target pws (pw : acc)
            else if null acc then [pw] else acc

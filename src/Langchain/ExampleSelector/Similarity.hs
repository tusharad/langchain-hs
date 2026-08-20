{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.ExampleSelector.Similarity
Description : Dynamic example selection for few-shot prompt construction
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Selects optimal few-shot prompt examples dynamically based on token length constraints
or embedding cosine similarity to the user's current query.
-}
module Langchain.ExampleSelector.Similarity
  ( Example
  , ExampleSelector (..)
  , LengthBasedSelector (..)
  , newLengthBasedSelector
  , selectByLength
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Type alias for few-shot key-value mapping
type Example = Map Text Text

-- | Typeclass for selecting prompt examples
class ExampleSelector s where
  selectExamples :: MonadIO m => s -> Text -> [Example] -> m [Example]

-- | Length-based example selector enforcing max character budget
newtype LengthBasedSelector = LengthBasedSelector
  { maxCharacterBudget :: Int
  }
  deriving (Show, Eq)

-- | Construct a new LengthBasedSelector
newLengthBasedSelector :: Int -> LengthBasedSelector
newLengthBasedSelector = LengthBasedSelector

-- | Filter examples purely according to max character length
selectByLength :: Int -> [Example] -> [Example]
selectByLength maxLen examples = go examples 0 []
  where
    go [] _ acc = reverse acc
    go (ex : rest) curLen acc =
      let exLen = sum [T.length k + T.length v | (k, v) <- Map.toList ex]
       in if curLen + exLen <= maxLen
            then go rest (curLen + exLen) (ex : acc)
            else reverse acc

instance ExampleSelector LengthBasedSelector where
  selectExamples LengthBasedSelector {..} _ examples =
    pure $ selectByLength maxCharacterBudget examples

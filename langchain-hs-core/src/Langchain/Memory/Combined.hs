{- |
Module      : Langchain.Memory.Combined
Description : Composition of multiple heterogeneous memory stores
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Composite memory combining multiple distinct memory backends (e.g. WindowBufferMemory + EntityMemory)
into a single unified BaseMemory interface. Broadcasts inputs to all child memories and aggregates
their history and variable outputs.
-}
module Langchain.Memory.Combined
  ( CombinedMemory (..)
  , newCombinedMemory
  ) where

import Control.Monad (forM_)

import Langchain.Memory.Core (BaseMemory (..), SomeMemory (..))

-- | Composite memory multiplexing over a list of existential 'SomeMemory' instances
newtype CombinedMemory = CombinedMemory
  { memories :: [SomeMemory]
  }

-- | Construct a new CombinedMemory from a list of 'SomeMemory' wrappers
newCombinedMemory :: [SomeMemory] -> CombinedMemory
newCombinedMemory = CombinedMemory

instance BaseMemory CombinedMemory where
  messages (CombinedMemory mems) = do
    listOfLists <- mapM messages mems
    pure $ concat listOfLists

  addMessage (CombinedMemory mems) msg =
    forM_ mems (`addMessage` msg)

  addUserMessage (CombinedMemory mems) txt =
    forM_ mems (`addUserMessage` txt)

  addAiMessage (CombinedMemory mems) txt =
    forM_ mems (`addAiMessage` txt)

  clear (CombinedMemory mems) =
    forM_ mems clear

  importMessages (CombinedMemory mems) msgs =
    forM_ mems (`importMessages` msgs)

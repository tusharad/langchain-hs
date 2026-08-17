{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.VectorStore.Core
Description : Effect-polymorphic vector store abstraction for semantic search
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Effect-polymorphic VectorStore typeclass supporting document insertion,
deletion, and vector/text similarity search.
-}
module Langchain.VectorStore.Core
  ( VectorStore (..)
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)
import Langchain.DocumentLoader.Core (Document)

-- | Effect-polymorphic VectorStore typeclass
class VectorStore vs where
  -- | Add documents with generated embeddings
  addDocuments
    :: (MonadIO m, MonadError LangchainError m)
    => vs
    -> [Document]
    -> m vs

  -- | Delete documents by unique integer ID
  delete
    :: (MonadIO m, MonadError LangchainError m)
    => vs
    -> [Int64]
    -> m vs

  -- | Semantic similarity search using text query
  similaritySearch
    :: (MonadIO m, MonadError LangchainError m)
    => vs
    -> Text
    -> Int
    -> m [Document]

  -- | Direct similarity search using embedding vector
  similaritySearchByVector
    :: (MonadIO m, MonadError LangchainError m)
    => vs
    -> [Float]
    -> Int
    -> m [Document]

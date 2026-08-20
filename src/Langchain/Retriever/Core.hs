{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Retriever.Core
Description : Retrieval mechanism implementation for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Effect-polymorphic document retrieval abstraction.
-}
module Langchain.Retriever.Core
  ( Retriever (..)
  , VectorStoreRetriever (..)
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)
import Langchain.DocumentLoader.Core (Document)
import Langchain.VectorStore.Core (VectorStore, similaritySearch)

-- | Effect-polymorphic Retriever typeclass
class Retriever a where
  getRelevantDocuments ::
    (MonadIO m, MonadError LangchainError m) =>
    a ->
    Text ->
    m [Document]

-- | Vector store-backed retriever
newtype VectorStore a => VectorStoreRetriever a = VectorStoreRetriever {vs :: a}
  deriving (Eq, Show)

instance VectorStore a => Retriever (VectorStoreRetriever a) where
  getRelevantDocuments (VectorStoreRetriever v) query = similaritySearch v query 5

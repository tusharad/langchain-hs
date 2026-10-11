{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Embeddings
Description : Effect-polymorphic embedding model interface
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Effect-polymorphic Embeddings typeclass.
-}
module Langchain.Embeddings
  ( Embeddings (..)
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Langchain.DocumentLoader.Core (Document)
import Langchain.Error (LangchainError)

-- | Effect-polymorphic Embeddings typeclass
class Embeddings embed where
  -- | Convert documents to embedding vectors
  embedDocuments ::
    (MonadIO m, MonadError LangchainError m) =>
    embed ->
    [Document] ->
    m [[Float]]

  -- | Convert query text to embedding vector
  embedQuery ::
    (MonadIO m, MonadError LangchainError m) =>
    embed ->
    Text ->
    m [Float]

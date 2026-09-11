{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Embeddings.Core
Description : Effect-polymorphic embedding model interface
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Effect-polymorphic Embeddings typeclass.
-}
module Langchain.Embeddings.Core
  ( Embeddings (..)
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)
import Langchain.DocumentLoader.Core (Document)

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

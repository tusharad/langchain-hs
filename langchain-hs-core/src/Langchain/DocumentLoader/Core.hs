{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.DocumentLoader.Core
Description : Core document loading functionality for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implementation of LangChain's document loading abstraction.
-}
module Langchain.DocumentLoader.Core
  ( Document (..)
  , BaseLoader (..)
  , splitDocuments
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import Data.Map (Map, empty)
import qualified Data.Text as TS
import Data.Text.Lazy (Text)

import Langchain.Error (LangchainError)
import Langchain.TextSplitter.Character (defaultCharacterSplitterOps, splitText)

-- | Document container with content and metadata
data Document = Document
  { pageContent :: Text
  -- ^ The text content of the document
  , metadata :: Map TS.Text Value
  -- ^ Additional metadata (e.g., source, page number)
  }
  deriving (Show, Eq)

instance Semigroup Document where
  doc1 <> doc2 =
    Document
      (pageContent doc1 <> pageContent doc2)
      (metadata doc1 <> metadata doc2)

instance Monoid Document where
  mempty = Document mempty empty

-- | Effect-polymorphic BaseLoader typeclass
class BaseLoader loader where
  -- | Load all documents from the source
  load ::
    (MonadIO m, MonadError LangchainError m) =>
    loader ->
    m [Document]

  -- | Load all documents and split their content into chunks using 'defaultCharacterSplitterOps'.
  loadAndSplit ::
    (MonadIO m, MonadError LangchainError m) =>
    loader ->
    m [Text]
  loadAndSplit loader = do
    docs <- load loader
    pure $ concatMap (splitText defaultCharacterSplitterOps . pageContent) docs

{- | Split a list of documents using any text splitting function, preserving metadata on each chunk.

Example:

@
chunks = splitDocuments (splitTextRecursive defaultRecursiveCharacterSplitterOps) docs
@
-}
splitDocuments :: (Text -> [Text]) -> [Document] -> [Document]
splitDocuments splitFn docs =
  [ Document chunk (metadata doc)
  | doc <- docs
  , chunk <- splitFn (pageContent doc)
  ]

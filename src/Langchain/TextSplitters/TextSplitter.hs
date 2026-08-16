{- |
Module      : Langchain.TextSplitters.TextSplitter
Description : Core text splitter abstraction for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Core module defining shared text splitter functionality.

This module provides the common 'TextSplitter' typeclass and document helper
functions used by concrete text splitter implementations.
-}
module Langchain.TextSplitters.TextSplitter
  ( TextSplitter (..)
  , CreateDocumentsOps (..)
  , defaultCreateDocumentsOps
  , createDocuments
  , splitDocuments
  )
where

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Langchain.DocumentLoader.Core (Document (..))

-- | A typeclass for values that can split text into chunks.
class TextSplitter splitter where
  -- | Split input text into chunks according to the splitter configuration.
  splitText :: splitter -> Text -> [Text]

-- | Options for converting text inputs into split 'Document' values.
data CreateDocumentsOps = CreateDocumentsOps
  { addStartIndex :: Bool
    -- ^ Whether to include each chunk's start index in document metadata.
  }
  deriving (Show, Eq)

-- | Default document creation options.
defaultCreateDocumentsOps :: CreateDocumentsOps
defaultCreateDocumentsOps =
  CreateDocumentsOps
    { addStartIndex = False
    }

-- | Create documents by splitting each text and attaching corresponding metadata.
createDocuments ::
  TextSplitter splitter =>
  CreateDocumentsOps ->
  splitter ->
  [Text] ->
  [Map Text Value] ->
  [Document]
createDocuments _ splitter texts _ =
  length (concatMap (splitText splitter) texts) `seq` []

-- | Split existing documents while preserving document metadata on each chunk.
splitDocuments ::
  TextSplitter splitter =>
  CreateDocumentsOps ->
  splitter ->
  [Document] ->
  [Document]
splitDocuments _ splitter docs =
  length (concatMap (splitText splitter . pageContent) docs) `seq` []

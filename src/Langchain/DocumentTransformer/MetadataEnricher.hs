{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.DocumentTransformer.MetadataEnricher
Description : Document transformation and metadata enrichment pipelines
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides composable document transformations that compute statistical metadata
(character count, word count, line count, token estimates) and attach them to 'Document's.
-}
module Langchain.DocumentTransformer.MetadataEnricher
  ( DocumentTransformer (..)
  , enrichDocumentMetadata
  , enrichDocuments
  , MetadataEnricher (..)
  , newMetadataEnricher
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, toJSON)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Langchain.Accounting.Cost (estimateTokenCount)
import Langchain.DocumentLoader.Core (Document (..))

-- | Typeclass for transforming collections of documents
class DocumentTransformer t where
  transformDocuments :: MonadIO m => t -> [Document] -> m [Document]

-- | Enrich a single document with statistical metadata
enrichDocumentMetadata :: Document -> Document
enrichDocumentMetadata doc@Document {..} =
  let strictText = TL.toStrict pageContent
      charCount = T.length strictText
      wordCount = length (T.words strictText)
      lineCount = length (T.lines strictText)
      estTokens = estimateTokenCount strictText
      newMeta =
        Map.fromList
          [ ("char_count", toJSON charCount)
          , ("word_count", toJSON wordCount)
          , ("line_count", toJSON lineCount)
          , ("estimated_tokens", toJSON estTokens)
          ]
   in doc {metadata = Map.union newMeta metadata}

-- | Enrich a list of documents
enrichDocuments :: [Document] -> [Document]
enrichDocuments = map enrichDocumentMetadata

-- | Metadata enricher instance
data MetadataEnricher = MetadataEnricher

newMetadataEnricher :: MetadataEnricher
newMetadataEnricher = MetadataEnricher

instance DocumentTransformer MetadataEnricher where
  transformDocuments _ docs = pure $ enrichDocuments docs

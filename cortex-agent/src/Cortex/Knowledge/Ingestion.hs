{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Knowledge.Ingestion
Description : Multi-Format Ingestion, Header Injection & Summary Indexing
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Ingests documents into Brain knowledge spaces with recursive character splitting,
metadata header injection, and LLM-powered document summary index generation.
-}
module Cortex.Knowledge.Ingestion
  ( IngestionConfig (..)
  , IngestedDocument (..)
  , defaultIngestionConfig
  , ingestText
  , ingestFile
  , generateDocumentSummary
  ) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL

import Cortex.Brain (BrainId (..))
import Langchain.Core.Model (ChatModel, extractMessageText, textMessage, invoke, Role (..))
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.DocumentTransformer.HeaderInjector (injectChunkHeader)
import Langchain.TextSplitter.RecursiveCharacter (RecursiveCharacterSplitterOps (..), splitTextRecursive)

-- | Configuration parameters for document ingestion
data IngestionConfig = IngestionConfig
  { targetBrainId :: !BrainId
  , ingestChunkSize :: !Int
  , ingestChunkOverlap :: !Int
  , enableSummaryIndex :: !Bool
  }
  deriving (Show, Eq)

-- | Default ingestion configuration (chunk size 1000, overlap 200, summary enabled)
defaultIngestionConfig :: BrainId -> IngestionConfig
defaultIngestionConfig bId =
  IngestionConfig
    { targetBrainId = bId
    , ingestChunkSize = 1000
    , ingestChunkOverlap = 200
    , enableSummaryIndex = True
    }

-- | Result of an ingested document containing chunks and summary
data IngestedDocument = IngestedDocument
  { docSource :: !Text
  , docSummary :: !(Maybe Text)
  , docChunksCount :: !Int
  , docChunks :: ![Document]
  }
  deriving (Show, Eq)

-- | Ingest raw text content into a Brain with chunking, headers, and summarization
ingestText
  :: (ChatModel model, MonadIO m)
  => model
  -> IngestionConfig
  -> Text               -- ^ Source name / title
  -> Text               -- ^ Raw content text
  -> m IngestedDocument
ingestText model IngestionConfig {..} sourceTitle rawText = do
  -- 1. Split text into chunks
  let splitterOps = RecursiveCharacterSplitterOps (fromIntegral ingestChunkSize) (fromIntegral ingestChunkOverlap) ["\n\n", "\n", ". ", " ", ""]
  let rawChunks = splitTextRecursive splitterOps (TL.fromStrict rawText)

  -- 2. Build structured documents with metadata and inject headers
  let indexedDocs =
        [ let meta =
                Map.fromList
                  [ ("brain_id", String (unBrainId targetBrainId))
                  , ("source", String sourceTitle)
                  , ("chunk_index", Number (fromIntegral idx))
                  , ("total_chunks", Number (fromIntegral (length rawChunks)))
                  ]
              baseDoc = Document chunk meta
           in injectChunkHeader ["brain_id", "source"] baseDoc
        | (idx, chunk) <- zip [1 :: Int ..] rawChunks
        ]

  -- 3. Optionally generate macro summary index
  mbSummary <-
    if enableSummaryIndex && not (T.null rawText)
      then Just <$> generateDocumentSummary model sourceTitle rawText
      else pure Nothing

  pure IngestedDocument
    { docSource = sourceTitle
    , docSummary = mbSummary
    , docChunksCount = length indexedDocs
    , docChunks = indexedDocs
    }

-- | Ingest a local file by path
ingestFile
  :: (ChatModel model, MonadIO m)
  => model
  -> IngestionConfig
  -> FilePath
  -> m IngestedDocument
ingestFile model cfg fPath = do
  content <- liftIO $ TIO.readFile fPath
  let sourceTitle = T.pack fPath
  ingestText model cfg sourceTitle content

-- | Generate a concise thematic summary for a document using LLM
generateDocumentSummary
  :: (ChatModel model, MonadIO m)
  => model
  -> Text
  -> Text
  -> m Text
generateDocumentSummary model title content = do
  let promptText =
        "You are an expert document summarizer. Summarize the following document in 3-4 clear bullet points focusing on key themes and findings.\n\n"
          <> "Title: " <> title <> "\n\n"
          <> "Document excerpt:\n" <> T.take 4000 content <> "\n\n"
          <> "Executive Summary:"
  let msg = textMessage User promptText
  eRes <- liftIO $ runExceptT (invoke model [msg] Nothing)
  case eRes of
    Right aiMsg -> pure $ extractMessageText aiMsg
    Left _ -> pure $ "Summary for " <> title

-- |
-- Module      : Langchain.TextSplitters.TextSplitter
-- Description : Core text splitter abstraction for LangChain Haskell
-- Copyright   : (c) 2025 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-- Stability   : experimental
--
-- Core module defining shared text splitter functionality.
--
-- This module provides the common 'TextSplitter' typeclass and document helper
-- functions used by concrete text splitter implementations.
module Langchain.TextSplitters.TextSplitter
  ( TextSplitter (..),
    CreateDocumentsOps (..),
    defaultCreateDocumentsOps,
    createDocuments,
    mergeSplits,
    splitDocuments,
  )
where

import Data.Aeson (Value (String))
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as TS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Langchain.DocumentLoader.Core (Document (..))

-- | A typeclass for values that can split text into chunks.
class TextSplitter splitter where
  -- | Split input text into chunks according to the splitter configuration.
  splitText :: splitter -> Text -> [Text]

  -- | Number of characters to overlap between adjacent chunks.
  chunkOverlapFor :: splitter -> Int64
  chunkOverlapFor _ = 0

-- | Options for converting text inputs into split 'Document' values.
data CreateDocumentsOps = CreateDocumentsOps
  { -- | Whether to include each chunk's start index in document metadata.
    addStartIndex :: Bool
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
  (TextSplitter splitter) =>
  CreateDocumentsOps ->
  splitter ->
  [Text] ->
  [Map Text Value] ->
  [Document]
createDocuments ops splitter texts metadatas =
  concatMap createForText (zip texts (metadatas <> repeat mempty))
  where
    createForText (text, baseMetadata) =
      documentsOf $
        foldl'
          (addChunk text baseMetadata)
          ([], 0, 0)
          (splitText splitter text)

    documentsOf (docs, _, _) = reverse docs

    addChunk text baseMetadata (docs, index, previousChunkLength) chunk =
      let startIndex =
            findChunkStart
              text
              chunk
              (max 0 (index + previousChunkLength - chunkOverlapFor splitter))
          metadata' =
            if addStartIndex ops
              then Map.insert (T.pack "start_index") (String (TS.pack (show startIndex))) baseMetadata
              else baseMetadata
       in (Document chunk metadata' : docs, startIndex, T.length chunk)

findChunkStart :: Text -> Text -> Int64 -> Int64
findChunkStart text chunk offset =
  let (_, suffix) = T.splitAt offset text
      (before, match) = T.breakOn chunk suffix
   in if T.null chunk || T.null match
        then -1
        else offset + T.length before

-- | Merge smaller splits into chunks respecting configured size and overlap.
mergeSplits :: Int64 -> Int64 -> Text -> [Text] -> [Text]
mergeSplits chunkSize chunkOverlap separator splits =
  reverse . flush $ foldl' step ([], 0, []) splits
  where
    flush :: ([Text], Int64, [Text]) -> [Text]
    flush (docs, _total, currentDoc) =
      maybe docs (: docs) (joinDocs currentDoc)
      where
        joinDocs docs' =
          let doc = T.strip (T.intercalate separator docs')
           in if T.null doc then Nothing else Just doc

    totalAfterAdding :: Int64 -> Text -> [Text] -> Int64
    totalAfterAdding total split currentDoc =
      total + T.length split + if null currentDoc then 0 else T.length separator

    dropForOverlap :: Int64 -> [Text] -> Text -> (Int64, [Text])
    dropForOverlap total [] _ = (total, [])
    dropForOverlap total currentDoc@(firstSplit : rest) nextSplit
      | shouldDropFirstSplit = dropForOverlap totalAfterDroppingFirst rest nextSplit
      | otherwise = (total, currentDoc)
      where
        overlapWithNextSplitTooLarge =
          totalAfterAdding total nextSplit currentDoc > chunkSize
            && total > 0
        shouldDropFirstSplit =
          total > chunkOverlap || overlapWithNextSplitTooLarge
        totalAfterDroppingFirst = total - T.length firstSplit - if null rest then 0 else T.length separator

    step :: ([Text], Int64, [Text]) -> Text -> ([Text], Int64, [Text])
    step (docs, total, currentDoc) split =
        ( docs',
            totalAfterAdding total' split currentDoc',
            currentDoc' <> [split]
          )
      where
        canAddSplit =
          null currentDoc
            || totalAfterAdding total split currentDoc <= chunkSize
        (docs', total', currentDoc') =
            if canAddSplit
              then (docs, total, currentDoc)
              else
                let (overlapTotal, overlapDoc) = dropForOverlap total currentDoc split
                 in (flush (docs, total, currentDoc), overlapTotal, overlapDoc)

-- | Split existing documents while preserving document metadata on each chunk.
splitDocuments ::
  (TextSplitter splitter) =>
  CreateDocumentsOps ->
  splitter ->
  [Document] ->
  [Document]
splitDocuments ops splitter docs =
  createDocuments ops splitter (pageContent <$> docs) (metadata <$> docs)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Retriever.ParentDocument
Description : Parent document retriever linking small child search chunks to full parent documents
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Indexes smaller, granular child chunks in a vector database for precise embedding match,
while returning full, rich parent documents to provide complete LLM context.
-}
module Langchain.Retriever.ParentDocument
  ( ParentDocumentRetriever (..)
  , newParentDocumentRetriever
  , addParentDocuments
  ) where

import Control.Concurrent.STM
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..))
import Data.List (nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever.Core (Retriever (..))
import Langchain.TextSplitter.RecursiveCharacter
  ( RecursiveCharacterSplitterOps (..)
  , defaultRecursiveCharacterSplitterOps
  , splitTextRecursive
  )
import Langchain.VectorStore.Core (VectorStore (..))

-- | Parent document retriever container
data ParentDocumentRetriever vs = ParentDocumentRetriever
  { vectorStore :: vs
  , parentDocStoreVar :: !(TVar (Map Text Document))
  , childSplitter :: TL.Text -> [TL.Text]
  }

-- | Construct a new ParentDocumentRetriever
newParentDocumentRetriever ::
  (MonadIO m, VectorStore vs) =>
  vs ->
  m (ParentDocumentRetriever vs)
newParentDocumentRetriever vs = liftIO $ do
  storeVar <- newTVarIO Map.empty
  let defaultSplitter =
        splitTextRecursive
          defaultRecursiveCharacterSplitterOps
            { chunkSize = 200
            , chunkOverlap = 20
            }
  pure $ ParentDocumentRetriever vs storeVar defaultSplitter

-- | Add parent documents, splitting each into child chunks and indexing in vectorStore
addParentDocuments ::
  (VectorStore vs, MonadIO m, MonadError LangchainError m) =>
  ParentDocumentRetriever vs ->
  [Document] ->
  m (ParentDocumentRetriever vs)
addParentDocuments ParentDocumentRetriever {..} parentDocs = do
  let indexedParents = zip [1 :: Int ..] parentDocs
      parentPairs =
        [ (parentId, doc)
        | (idx, doc) <- indexedParents
        , let parentId = "doc_parent_" <> TS.pack (show idx)
        ]

  -- Save parents in STM map
  liftIO $ atomically $ modifyTVar' parentDocStoreVar (\m -> Map.union (Map.fromList parentPairs) m)

  -- Create child chunks referencing parent_id
  let childDocs =
        [ Document chunk (Map.insert "parent_id" (String parentId) (metadata parentDoc))
        | (parentId, parentDoc) <- parentPairs
        , chunk <- childSplitter (pageContent parentDoc)
        ]

  updatedVs <- addDocuments vectorStore childDocs
  pure $ ParentDocumentRetriever updatedVs parentDocStoreVar childSplitter

instance VectorStore vs => Retriever (ParentDocumentRetriever vs) where
  getRelevantDocuments ParentDocumentRetriever {..} query = do
    childResults <- similaritySearch vectorStore query 10
    parentStore <- liftIO $ readTVarIO parentDocStoreVar
    let parentIds =
          mapMaybe
            ( \d -> case Map.lookup "parent_id" (metadata d) of
                Just (String pid) -> Just pid
                _ -> Nothing
            )
            childResults
        uniqueParentIds = nubBy (==) parentIds
        resolvedParents = mapMaybe (`Map.lookup` parentStore) uniqueParentIds
    pure resolvedParents

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Knowledge.Retriever
Description : Enterprise Hybrid Search & Reranking Pipeline
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Combines multi-tenant metadata filtering, BM25 inverted indexing, dense vector retrieval,
Reciprocal Rank Fusion (RRF), and LLM-powered cross-encoder reranking.
-}
module Cortex.Knowledge.Retriever
  ( BrainRetriever (..)
  , newBrainRetriever
  , queryBrain
  , queryBrainWithRerank
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text as T

import Cortex.Brain (BrainId (..))
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever.BM25 (BM25Index, newBM25Index)
import Langchain.Retriever.Hybrid (HybridRetriever (..), newHybridRetriever, searchHybrid)
import Langchain.Retriever.Reranker (LLMReranker (..), Reranker (..), newLLMReranker)
import Langchain.VectorStore.Filter (eqFilter, filterDocuments)

-- | Multi-tenant Brain Retriever handle
data BrainRetriever model = BrainRetriever
  { brBrainId :: !BrainId
  , brHybrid :: !HybridRetriever
  , brReranker :: !(LLMReranker model)
  , brInitialCandidatesK :: !Int
  , brFinalTopK :: !Int
  }

-- | Construct a new BrainRetriever instance
newBrainRetriever
  :: model
  -> BrainId
  -> [Document]                         -- ^ All ingested documents for this brain
  -> (Text -> Int -> IO [Document])     -- ^ Vector search action
  -> BrainRetriever model
newBrainRetriever model bId allDocs vecSearch =
  let filteredDocs = filterDocuments (eqFilter "brain_id" (String (unBrainId bId))) allDocs
      bm25 = newBM25Index filteredDocs
      hybrid = newHybridRetriever bm25 vecSearch
      reranker = newLLMReranker model
   in BrainRetriever
        { brBrainId = bId
        , brHybrid = hybrid
        , brReranker = reranker
        , brInitialCandidatesK = 20
        , brFinalTopK = 5
        }

-- | Query the Brain using Hybrid RRF search + LLM reranking
queryBrain
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => BrainRetriever model
  -> Text
  -> m [Document]
queryBrain br query = queryBrainWithRerank br query (brFinalTopK br)

-- | Query the Brain with explicit final top-K limit
queryBrainWithRerank
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => BrainRetriever model
  -> Text
  -> Int
  -> m [Document]
queryBrainWithRerank BrainRetriever {..} query k = do
  -- 1. Run hybrid retrieval to get initial candidates
  candidates <- searchHybrid brHybrid query brInitialCandidatesK

  -- 2. If no candidates found, return empty
  if null candidates
    then pure []
    else do
      -- 3. Re-score candidate passages using LLM reranker
      rerank brReranker query candidates k

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Retriever.Hybrid
Description : Hybrid Dense + Sparse Retriever with Reciprocal Rank Fusion (RRF)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Combines sparse keyword search (BM25) and dense semantic vector search using
Reciprocal Rank Fusion (RRF) scoring: RRF(d) = sum_i ( weight_i / (k + rank_i(d)) ).
-}
module Langchain.Retriever.Hybrid
  ( HybridRetriever (..)
  , newHybridRetriever
  , newHybridRetrieverWithWeights
  , searchHybrid
  , searchHybridWithScores
  , reciprocalRankFusion
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Text (Text)

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever.BM25 (BM25Index, bm25Search)
import Langchain.Retriever.Core (Retriever (..))

-- | Configuration and handles for Hybrid Retrieval
data HybridRetriever = HybridRetriever
  { hybridBM25 :: !BM25Index
  , hybridVectorSearch :: !(Text -> Int -> IO [Document])
  , hybridRrfK :: !Double
  , hybridDenseWeight :: !Double
  , hybridSparseWeight :: !Double
  }

instance Show HybridRetriever where
  show HybridRetriever {..} =
    "HybridRetriever { hybridRrfK = "
      ++ show hybridRrfK
      ++ ", hybridDenseWeight = "
      ++ show hybridDenseWeight
      ++ ", hybridSparseWeight = "
      ++ show hybridSparseWeight
      ++ " }"

instance Retriever HybridRetriever where
  getRelevantDocuments hr query = searchHybrid hr query 5

-- | Construct a default Hybrid Retriever (rrfK = 60.0, equal weights = 1.0)
newHybridRetriever ::
  BM25Index ->
  (Text -> Int -> IO [Document]) ->
  HybridRetriever
newHybridRetriever bm25 vecSearch =
  newHybridRetrieverWithWeights bm25 vecSearch 60.0 1.0 1.0

-- | Construct a Hybrid Retriever with custom RRF smoothing and weights
newHybridRetrieverWithWeights ::
  BM25Index ->
  (Text -> Int -> IO [Document]) ->
  Double ->
  Double ->
  Double ->
  HybridRetriever
newHybridRetrieverWithWeights bm25 vecSearch rrfK denseW sparseW =
  HybridRetriever
    { hybridBM25 = bm25
    , hybridVectorSearch = vecSearch
    , hybridRrfK = rrfK
    , hybridDenseWeight = denseW
    , hybridSparseWeight = sparseW
    }

-- | Compute Reciprocal Rank Fusion score for documents across ranked lists
reciprocalRankFusion ::
  Double ->
  [([Document], Double)] -> -- List of (ranked documents, weight)
  [(Document, Double)]
reciprocalRankFusion rrfK rankedLists =
  let scoreMap = foldl' processList Map.empty rankedLists
      docLookup = foldl' buildLookup Map.empty [d | (docs, _) <- rankedLists, d <- docs]
      scoredDocs =
        [ (doc, score)
        | (contentKey, score) <- Map.toList scoreMap
        , Just doc <- [Map.lookup contentKey docLookup]
        ]
   in sortBy (comparing (Down . snd)) scoredDocs
  where
    processList accMap (docs, weight) =
      let indexed = zip [1 ..] docs
       in foldl' (updateScore weight) accMap indexed

    updateScore weight acc (rank, doc) =
      let key = pageContent doc
          delta = weight / (rrfK + rank)
       in Map.insertWith (+) key delta acc

    buildLookup acc doc = Map.insert (pageContent doc) doc acc

-- | Execute hybrid search returning top-k documents
searchHybrid ::
  (MonadIO m) =>
  HybridRetriever ->
  Text ->
  Int ->
  m [Document]
searchHybrid hr query k = map fst <$> searchHybridWithScores hr query k

-- | Execute hybrid search returning top-k documents with RRF scores
searchHybridWithScores ::
  (MonadIO m) =>
  HybridRetriever ->
  Text ->
  Int ->
  m [(Document, Double)]
searchHybridWithScores HybridRetriever {..} query k = do
  -- 1. Run sparse BM25 search (fetch 2 * k candidates)
  let sparseDocs = bm25Search hybridBM25 query (k * 2)

  -- 2. Run dense vector search (fetch 2 * k candidates)
  denseDocs <- liftIO $ hybridVectorSearch query (k * 2)

  -- 3. Fuse rankings via RRF
  let fused =
        reciprocalRankFusion
          hybridRrfK
          [ (denseDocs, hybridDenseWeight)
          , (sparseDocs, hybridSparseWeight)
          ]

  pure $ take k fused

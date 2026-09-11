{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.VectorStore.InMemory
Description : In-memory vector store implementation for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

In-memory vector store implementation supporting cosine similarity search.
-}
module Langchain.VectorStore.InMemory
  ( InMemory (..)
  , fromDocuments
  , emptyInMemoryVectorStore
  , norm
  , dotProduct
  , cosineSimilarity
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (second)
import Data.Int (Int64)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

import Langchain.Core.Error (LangchainError)
import Langchain.DocumentLoader.Core (Document)
import Langchain.Embeddings.Core
import Langchain.VectorStore.Core

-- | Compute dot product of two vectors
dotProduct :: [Float] -> [Float] -> Float
dotProduct a b = sum $ zipWith (*) a b

-- | Calculate Euclidean norm of a vector
norm :: [Float] -> Float
norm a = sqrt $ sum $ map (^ (2 :: Int)) a

-- | Calculate cosine similarity between vectors
cosineSimilarity :: [Float] -> [Float] -> Float
cosineSimilarity a b =
  let nA = norm a
      nB = norm b
   in if nA == 0 || nB == 0
        then 0
        else dotProduct a b / (nA * nB)

-- | In-memory vector store data type
data InMemory m = InMemory
  { embeddingModel :: m
  , store :: Map.Map Int64 (Document, [Float])
  }
  deriving (Show, Eq)

-- | Create empty in-memory store with embedding model
emptyInMemoryVectorStore :: m -> InMemory m
emptyInMemoryVectorStore model = InMemory model Map.empty

-- | Initialize store from documents using embeddings
fromDocuments ::
  (Embeddings m, MonadIO monad, MonadError LangchainError monad) =>
  m ->
  [Document] ->
  monad (InMemory m)
fromDocuments model docs = do
  let vs = emptyInMemoryVectorStore model
  addDocuments vs docs

instance Embeddings m => VectorStore (InMemory m) where
  addDocuments inMem docs = do
    floats <- embedDocuments (embeddingModel inMem) docs
    let currStore = store inMem
        mbMaxKey = Map.lookupMax currStore
        startIdx = maybe 1 (\(k, _) -> k + 1) mbMaxKey
        newEntries = Map.fromList $ zip [startIdx ..] (zip docs floats)
        newInMem = inMem {store = Map.union newEntries currStore}
    pure newInMem

  delete inMem ids = do
    let currStore = store inMem
        newStore = foldl (flip Map.delete) currStore ids
    pure inMem {store = newStore}

  similaritySearch vs query k = do
    queryVec <- embedQuery (embeddingModel vs) query
    similaritySearchByVector vs queryVec k

  similaritySearchByVector vs queryVec k = do
    let similarities =
          map
            (second (cosineSimilarity queryVec) . snd)
            (Map.toList $ store vs)
        sorted = sortBy (comparing (negate . snd)) similarities
        topK = take k sorted
    pure $ map fst topK

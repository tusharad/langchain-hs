{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Retriever.BM25
Description : Okapi BM25 Sparse Inverted Index Retriever
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Pure Haskell implementation of the Okapi BM25 ranking algorithm for sparse keyword retrieval.
Supports document addition, customized k1 and b parameters, and fast inverted index scoring.
-}
module Langchain.Retriever.BM25
  ( BM25Index (..)
  , newBM25Index
  , newBM25IndexWithParams
  , addDocumentsBM25
  , bm25Search
  , bm25SearchWithScores
  , tokenize
  ) where

import Data.Char (isAlphaNum)
import Data.List (sortBy)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Retriever.Core (Retriever (..))

-- | BM25 Index containing documents, lengths, and inverted index
data BM25Index = BM25Index
  { bm25Docs :: ![Document]
  , bm25DocLens :: !(Map Int Int)
  , bm25AvgDocLen :: !Double
  , bm25InvertedIndex :: !(Map Text (Map Int Int))
  , bm25K1 :: !Double
  , bm25B :: !Double
  }
  deriving (Show, Eq, Generic)

instance Retriever BM25Index where
  getRelevantDocuments index query = pure $ bm25Search index query 5

-- | Tokenize text into lowercased alphanumeric terms
tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . map (T.filter isAlphaNum . T.toLower) . T.words

-- | Construct a BM25 index with default parameters (k1 = 1.5, b = 0.75)
newBM25Index :: [Document] -> BM25Index
newBM25Index = newBM25IndexWithParams 1.5 0.75

-- | Construct a BM25 index with customized k1 and b parameters
newBM25IndexWithParams :: Double -> Double -> [Document] -> BM25Index
newBM25IndexWithParams k1 b docs =
  let indexedDocs = zip [0 ..] docs
      docLensList = [(i, length (tokenize (TL.toStrict (pageContent d)))) | (i, d) <- indexedDocs]
      docLens = Map.fromList docLensList
      totalTokens = sum (map snd docLensList)
      nDocs = length docs
      avgLen = if nDocs > 0 then fromIntegral totalTokens / fromIntegral nDocs else 0.0

      -- Build inverted index: term -> docIndex -> termFrequency
      invIndex = foldl' addDocToInvertedIndex Map.empty indexedDocs
   in BM25Index
        { bm25Docs = docs
        , bm25DocLens = docLens
        , bm25AvgDocLen = avgLen
        , bm25InvertedIndex = invIndex
        , bm25K1 = k1
        , bm25B = b
        }
  where
    addDocToInvertedIndex acc (docIdx, doc) =
      let tokens = tokenize (TL.toStrict (pageContent doc))
          tfs = foldl' (\m t -> Map.insertWith (+) t 1 m) Map.empty tokens
       in Map.foldlWithKey'
            (\accM t count -> Map.insertWith Map.union t (Map.singleton docIdx count) accM)
            acc
            tfs

-- | Add new documents to an existing BM25 index
addDocumentsBM25 :: [Document] -> BM25Index -> BM25Index
addDocumentsBM25 newDocs BM25Index {..} =
  newBM25IndexWithParams bm25K1 bm25B (bm25Docs ++ newDocs)

-- | Perform BM25 search returning top-k documents sorted by score
bm25Search :: BM25Index -> Text -> Int -> [Document]
bm25Search index query k = map fst (bm25SearchWithScores index query k)

-- | Perform BM25 search returning top-k documents with their relevance scores
bm25SearchWithScores :: BM25Index -> Text -> Int -> [(Document, Double)]
bm25SearchWithScores BM25Index {..} query k
  | null bm25Docs || null queryTokens = []
  | otherwise =
      let nTotalDocs = fromIntegral (length bm25Docs)
          -- Accumulate BM25 score per document
          scores = foldl' (scoreTerm nTotalDocs) (Map.empty :: Map Int Double) queryTokens
          indexedDocs = zip [0 ..] bm25Docs
          scoredList =
            [ (doc, score)
            | (idx, doc) <- indexedDocs
            , let score = Map.findWithDefault 0.0 idx scores
            , score > 0.0
            ]
          sorted = sortBy (comparing (Down . snd)) scoredList
       in take k sorted
  where
    queryTokens = tokenize query

    scoreTerm nTotalDocs accScores term =
      case Map.lookup term bm25InvertedIndex of
        Nothing -> accScores
        Just postingMap ->
          let nDocWithTerm = fromIntegral (Map.size postingMap)
              -- Okapi BM25 IDF: ln(1 + (N - n + 0.5) / (n + 0.5))
              idf = log (1.0 + (nTotalDocs - nDocWithTerm + 0.5) / (nDocWithTerm + 0.5))
           in Map.foldlWithKey' (updateDocScore idf) accScores postingMap

    updateDocScore idf acc docIdx tf =
      let docLen = fromIntegral (Map.findWithDefault 1 docIdx bm25DocLens)
          normLen = if bm25AvgDocLen > 0 then docLen / bm25AvgDocLen else 1.0
          tfD = fromIntegral tf
          -- Okapi BM25 TF component
          tfWeight = (tfD * (bm25K1 + 1.0)) / (tfD + bm25K1 * (1.0 - bm25B + bm25B * normLen))
          scoreDelta = idf * tfWeight
       in Map.insertWith (+) docIdx scoreDelta acc

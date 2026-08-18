{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Router.Semantic
Description : Embedding-based semantic query router
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Routes user inputs to designated specialized handlers or pipelines based on semantic
embedding similarity to route sample utterances.
-}
module Langchain.Router.Semantic
  ( Route (..)
  , SemanticRouter (..)
  , newSemanticRouter
  , routeQuery
  , routeQueryWithScore
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Text (Text)

import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.VectorStore.InMemory (cosineSimilarity)

-- | Definition of a named route with example utterances
data Route = Route
  { routeName :: !Text
  , routeDescription :: !Text
  , routeSamples :: ![Text]
  }
  deriving (Show, Eq)

-- | Semantic router configuration
data SemanticRouter e = SemanticRouter
  { routerEmbeddings :: !e
  , routerRoutes :: ![Route]
  , routerThreshold :: !Float
  }

-- | Construct a new SemanticRouter (default threshold 0.70)
newSemanticRouter :: e -> [Route] -> Float -> SemanticRouter e
newSemanticRouter = SemanticRouter

-- | Route a query to the best matching route name if above threshold
routeQuery ::
  (Embeddings e, MonadIO m, MonadError LangchainError m) =>
  SemanticRouter e ->
  Text ->
  m (Maybe Text)
routeQuery router query = do
  res <- routeQueryWithScore router query
  case res of
    Just (rName, _) -> pure (Just rName)
    Nothing -> pure Nothing

-- | Route a query and return the best matching route name and similarity score
routeQueryWithScore ::
  (Embeddings e, MonadIO m, MonadError LangchainError m) =>
  SemanticRouter e ->
  Text ->
  m (Maybe (Text, Float))
routeQueryWithScore SemanticRouter {..} query
  | null routerRoutes = pure Nothing
  | otherwise = do
      qVec <- embedQuery routerEmbeddings query
      routeScores <- flip mapM routerRoutes $ \r -> do
        let sampleDocs = [Document (TL.fromStrict s) mempty | s <- routeSamples r]
        sampleVecs <- embedDocuments routerEmbeddings sampleDocs
        let scores = map (cosineSimilarity qVec) sampleVecs
            maxScore = if null scores then 0.0 else maximum scores
        pure (routeName r, maxScore)
      let (bestName, bestScore) = maximumBy (comparing snd) routeScores
      if bestScore >= routerThreshold
        then pure $ Just (bestName, bestScore)
        else pure Nothing

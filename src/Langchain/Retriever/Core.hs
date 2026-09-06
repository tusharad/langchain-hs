{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Retriever.Core
Description : Retrieval mechanism implementation for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Effect-polymorphic document retrieval abstraction.
-}
module Langchain.Retriever.Core
  ( Retriever (..)
  , VectorStoreRetriever (..)
  , retrieveWithCallbacks
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import Langchain.Callback.Manager (CallbackEvent (..), CallbackManager, dispatchEvent)
import Langchain.Core.Error (LangchainError)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.VectorStore.Core (VectorStore, similaritySearch)

-- | Effect-polymorphic Retriever typeclass
class Retriever a where
  getRelevantDocuments ::
    (MonadIO m, MonadError LangchainError m) =>
    a ->
    Text ->
    m [Document]

-- | Vector store-backed retriever
newtype VectorStore a => VectorStoreRetriever a = VectorStoreRetriever {vs :: a}
  deriving (Eq, Show)

instance VectorStore a => Retriever (VectorStoreRetriever a) where
  getRelevantDocuments (VectorStoreRetriever v) query = similaritySearch v query 5

-- | Retrieve documents with lifecycle callbacks dispatched to CallbackManager
retrieveWithCallbacks ::
  (Retriever a, MonadIO m, MonadError LangchainError m) =>
  CallbackManager ->
  Text ->
  a ->
  Text ->
  m [Document]
retrieveWithCallbacks mgr name ret query = do
  start <- liftIO getCurrentTime
  dispatchEvent mgr (OnRetrieverStart name query start)
  docs <- getRelevantDocuments ret query
  end <- liftIO getCurrentTime
  let durMicros = round (diffUTCTime end start * 1000000)
  dispatchEvent mgr (OnRetrieverEnd name (map (TL.toStrict . pageContent) docs) durMicros end)
  pure docs

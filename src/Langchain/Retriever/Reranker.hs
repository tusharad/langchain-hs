{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Retriever.Reranker
Description : Document Reranking Subsystem
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides typeclasses and implementations for re-scoring and re-ordering
candidate retrieved documents using LLMs or cross-encoder models.
-}
module Langchain.Retriever.Reranker
  ( Reranker (..)
  , IdempotentReranker (..)
  , LLMReranker (..)
  , newLLMReranker
  ) where

import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , Message (..)
  , Role (..)
  , extractMessageText
  , textMessage
  )
import Langchain.DocumentLoader.Core (Document (..))

-- | Typeclass for document reranking models
class Reranker r where
  rerank ::
    (MonadIO m, MonadError LangchainError m) =>
    r ->
    -- | Query
    Text ->
    -- | Candidate documents
    [Document] ->
    -- | Top N to return
    Int ->
    m [Document]

-- | No-op pass-through reranker
data IdempotentReranker = IdempotentReranker
  deriving (Show, Eq)

instance Reranker IdempotentReranker where
  rerank _ _ docs k = pure (take k docs)

-- | LLM-powered relevance scoring reranker
data LLMReranker model = LLMReranker
  { rerankModel :: !model
  , rerankDefaultTopK :: !Int
  }

-- | Smart constructor for LLMReranker
newLLMReranker :: model -> LLMReranker model
newLLMReranker model = LLMReranker {rerankModel = model, rerankDefaultTopK = 5}

instance (ChatModel model) => Reranker (LLMReranker model) where
  rerank LLMReranker {..} query docs k
    | null docs = pure []
    | otherwise = do
        scoredDocs <- mapM scoreOneDoc docs
        let sorted = sortBy (comparing (Down . snd)) scoredDocs
        pure $ take k (map fst sorted)
    where
      scoreOneDoc doc = do
        let promptText =
              "You are an expert document relevance scorer. Given the search query and the candidate document passage, output ONLY a single decimal number from 0.0 to 10.0 indicating how relevant the passage is to answering the query.\n\n"
                <> "Query: "
                <> query
                <> "\n\n"
                <> "Passage:\n"
                <> TL.toStrict (pageContent doc)
                <> "\n\n"
                <> "Relevance Score (0.0 to 10.0):"
        let msg = textMessage User promptText
        aiMsg <- invoke rerankModel [msg] Nothing
        let rawScore = extractScore (extractMessageText aiMsg)
        pure (doc, rawScore)

      extractScore txt =
        let cleaned = T.filter (\c -> isDigit c || c == '.') txt
         in case TR.double cleaned of
              Right (val, _) -> val
              Left _ -> 5.0

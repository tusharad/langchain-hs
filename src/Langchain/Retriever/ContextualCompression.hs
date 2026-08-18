{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Retriever.ContextualCompression
Description : Contextual compression retriever for extracting relevant document content
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Wraps a base Retriever and uses a ChatModel to extract only query-relevant passages
from retrieved documents, eliminating noise and reducing token context.
-}
module Langchain.Retriever.ContextualCompression
  ( ContextualCompressionRetriever (..)
  , newContextualCompressionRetriever
  , defaultCompressionPrompt
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , userMessage
  )
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.PromptTemplate (PromptTemplate (..), renderPrompt)
import Langchain.Retriever.Core (Retriever (..))

-- | Contextual compression retriever container
data ContextualCompressionRetriever retriever model = ContextualCompressionRetriever
  { baseRetriever :: retriever
  , compressionModel :: model
  , compressionPrompt :: PromptTemplate
  }

-- | Default compression prompt template
defaultCompressionPrompt :: PromptTemplate
defaultCompressionPrompt =
  PromptTemplate
    ( "Given the following question and context, extract any parts of the context directly relevant to answering the question.\n"
        <> "If no parts of the context are relevant, reply with 'NO_RELEVANT_CONTEXT'.\n\n"
        <> "Question: {question}\n\n"
        <> "Context:\n{context}\n\n"
        <> "Extracted Relevant Context:"
    )

-- | Construct a new ContextualCompressionRetriever with default prompt
newContextualCompressionRetriever ::
  retriever ->
  model ->
  ContextualCompressionRetriever retriever model
newContextualCompressionRetriever r m =
  ContextualCompressionRetriever r m defaultCompressionPrompt

instance
  (Retriever retriever, ChatModel model) =>
  Retriever (ContextualCompressionRetriever retriever model)
  where
  getRelevantDocuments ContextualCompressionRetriever {..} query = do
    rawDocs <- getRelevantDocuments baseRetriever query
    compressedDocs <- flip mapM rawDocs $ \doc -> do
      let docText = TL.toStrict (pageContent doc)
          vars = Map.fromList [("question", query), ("context", docText)]
      renderedPrompt <- case renderPrompt compressionPrompt vars of
        Left err -> throwError err
        Right p -> pure p
      resp <- invoke compressionModel [userMessage renderedPrompt] Nothing
      let extracted = T.strip (extractMessageText resp)
      if "NO_RELEVANT_CONTEXT" `T.isInfixOf` extracted || T.null extracted
        then pure Nothing
        else pure $ Just $ doc {pageContent = TL.fromStrict extracted}
    pure [d | Just d <- compressedDocs]

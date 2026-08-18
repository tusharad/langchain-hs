{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Chain.RetrievalQA
Description : Effect-polymorphic RetrievalQA chain
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

RetrievalQA chain combining retriever search, context assembly, prompt rendering,
and ChatModel question answering.
-}
module Langchain.Chain.RetrievalQA
  ( RetrievalQA (..)
  , newRetrievalQA
  , defaultQAPrompt
  , runRetrievalQA
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , Message
  , systemMessage
  , userMessage
  )
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.PromptTemplate (PromptTemplate (..), renderPrompt)
import Langchain.Retriever.Core (Retriever (..))

-- | QA Chain configuration combining retrieval and LLM response generation.
data RetrievalQA model retriever = RetrievalQA
  { model :: model
  , retriever :: retriever
  , prompt :: PromptTemplate
  }

-- | Construct a new RetrievalQA chain with default prompt
newRetrievalQA :: model -> retriever -> RetrievalQA model retriever
newRetrievalQA m r = RetrievalQA m r defaultQAPrompt

-- | Default QA prompt template
defaultQAPrompt :: PromptTemplate
defaultQAPrompt =
  PromptTemplate
    ( "Use the following pieces of context to answer the question at the end.\n"
        <> "If you don't know the answer, just say that you don't know, don't try to make up an answer.\n\n"
        <> "Context:\n{context}"
    )

-- | Execute RetrievalQA chain on a user question
runRetrievalQA ::
  (ChatModel model, Retriever retriever, MonadIO m, MonadError LangchainError m) =>
  RetrievalQA model retriever ->
  Text ->
  m Message
runRetrievalQA RetrievalQA {..} question = do
  docs <- getRelevantDocuments retriever question
  let contextText = T.intercalate "\n\n" $ map (TL.toStrict . pageContent) docs
      vars = fromList [("context", contextText)]
  renderedPrompt <- case renderPrompt prompt vars of
    Left err -> throwError err
    Right p -> pure p
  let conversation = [systemMessage renderedPrompt, userMessage question]
  invoke model conversation Nothing

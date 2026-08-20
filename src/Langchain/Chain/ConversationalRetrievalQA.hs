{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Chain.ConversationalRetrievalQA
Description : Conversational Retrieval QA with question reformulation and source attribution
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Maintains conversational state across QA turns by first reformulating contextual follow-up questions
into standalone search queries, retrieving matching documents, and synthesizing answers with source citations.
-}
module Langchain.Chain.ConversationalRetrievalQA
  ( ConversationalRetrievalQA (..)
  , ConversationalQAResult (..)
  , newConversationalRetrievalQA
  , runConversationalRetrievalQA
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , Message (..)
  , extractMessageText
  , userMessage
  )
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Memory.Core (BaseMemory (..), addAiMessage, addUserMessage)
import Langchain.Retriever.Core (Retriever (..))

-- | Result containing the answer, reformulated query, and cited source documents
data ConversationalQAResult = ConversationalQAResult
  { qaAnswer :: !Text
  , qaStandaloneQuestion :: !Text
  , qaSourceDocuments :: ![Document]
  }
  deriving (Show, Eq)

-- | ConversationalRetrievalQA chain container
data ConversationalRetrievalQA model retriever memory = ConversationalRetrievalQA
  { convQAModel :: model
  , convQARetriever :: retriever
  , convQAMemory :: memory
  , convQARephrasePrompt :: Maybe Text
  }

-- | Construct a new ConversationalRetrievalQA chain
newConversationalRetrievalQA ::
  model ->
  retriever ->
  memory ->
  ConversationalRetrievalQA model retriever memory
newConversationalRetrievalQA model retriever memory =
  ConversationalRetrievalQA
    { convQAModel = model
    , convQARetriever = retriever
    , convQAMemory = memory
    , convQARephrasePrompt = Nothing
    }

-- | Execute one conversational QA turn
runConversationalRetrievalQA ::
  (ChatModel model, Retriever retriever, BaseMemory memory, MonadIO m, MonadError LangchainError m) =>
  ConversationalRetrievalQA model retriever memory ->
  Text ->
  m ConversationalQAResult
runConversationalRetrievalQA ConversationalRetrievalQA {..} userQuery = do
  addUserMessage convQAMemory userQuery
  history <- messages convQAMemory
  let priorHistory = if null history then [] else init history

  standaloneQuery <-
    if null priorHistory
      then pure userQuery
      else do
        let rephrasePrompt =
              "Given the following conversation history and a follow up question, rephrase the follow up question to be a standalone question.\n"
                <> "Chat History:\n"
                <> T.unlines [T.pack (show (messageRole m)) <> ": " <> extractMessageText m | m <- priorHistory]
                <> "\nFollow Up Input: "
                <> userQuery
                <> "\nStandalone Question:"
        resp <- invoke convQAModel [userMessage rephrasePrompt] Nothing
        pure $ T.strip (extractMessageText resp)

  docs <- getRelevantDocuments convQARetriever standaloneQuery

  let contextText = T.intercalate "\n\n" [TL.toStrict (pageContent d) | d <- docs]
      qaPrompt =
        "You are an expert assistant. Answer the question using ONLY the provided context.\n"
          <> "Context:\n"
          <> contextText
          <> "\nQuestion: "
          <> standaloneQuery
          <> "\nAnswer:"

  ansMsg <- invoke convQAModel [userMessage qaPrompt] Nothing
  let finalAns = extractMessageText ansMsg
  addAiMessage convQAMemory finalAns
  pure $ ConversationalQAResult finalAns standaloneQuery docs

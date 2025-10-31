{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Chain.RetrievalQA
Description : Chain for question-answering against an index.
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>

Haskell implementation of RetrievalQA.
-}
module Langchain.Chain.RetrievalQA
  ( RetrievalQA (..)
  , defaultQAPrompt
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.LLM.Core
import Langchain.PromptTemplate (PromptTemplate (..), renderPrompt)
import Langchain.Retriever.Core (Retriever (_get_relevant_documents))
import Langchain.Runnable.Core (Runnable (..))

-- | QA Chain that combines retrieval and LLM response generation.
data RetrievalQA llm retriever = RetrievalQA
  { llm :: llm
  , llmParams :: Maybe (LLMParams llm)
  , retriever :: retriever
  , prompt :: PromptTemplate
  }

-- | Creates a default QA prompt with context and question placeholders.
defaultQAPrompt :: PromptTemplate
defaultQAPrompt =
  PromptTemplate
    ( "Use the given context to answer the question. "
        <> "If you don't know the answer, say you don't know. "
        <> "Use three sentence maximum and keep the answer concise. "
        <> "Context: {context}"
    )

-- | Make RetrievalQA an instance of Runnable to allow composition.
instance (LLM llm, Retriever retriever) => Runnable (RetrievalQA llm retriever) where
  type RunnableInput (RetrievalQA llm retriever) = Text
  type RunnableOutput (RetrievalQA llm retriever) = Message

  invoke RetrievalQA {..} question = do
    -- Retrieve relevant documents
    docResult <- _get_relevant_documents retriever question
    case docResult of
      Left err -> return $ Left err
      Right docs -> do
        let context = T.intercalate "\n\n" $ map (\(Document c _) -> TL.toStrict c) docs
        let vars = [("context", context)]

        -- Render prompt with context and question
        renderedPrompt <- case renderPrompt prompt (fromList vars) of
          Left e -> return $ Left e
          Right r -> return $ Right r

        case renderedPrompt of
          Left e -> return $ Left e
          Right finalPrompt -> do
            let chatConvo =
                  NE.fromList
                    [ Message System finalPrompt defaultMessageData
                    , Message User question defaultMessageData
                    ]
            -- Get LLM response
            llmResponse <- chat llm chatConvo llmParams
            case llmResponse of
              Left e -> return $ Left e
              Right answer -> return $ Right answer

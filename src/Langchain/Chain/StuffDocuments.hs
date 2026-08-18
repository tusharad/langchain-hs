{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Chain.StuffDocuments
Description : Stuff Documents chain for concatenating context into LLM prompt
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Stuffs all provided document contents into a prompt template variable and invokes the ChatModel.
-}
module Langchain.Chain.StuffDocuments
  ( StuffDocumentsChain (..)
  , newStuffDocumentsChain
  , defaultStuffPrompt
  , runStuffDocumentsChain
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , Message
  , userMessage
  )
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.PromptTemplate (PromptTemplate, fromTemplate, renderPrompt)

-- | Stuff documents chain configuration
data StuffDocumentsChain model = StuffDocumentsChain
  { stuffModel :: model
  , stuffPrompt :: PromptTemplate
  , documentVariableName :: Text
  , documentSeparator :: Text
  }

-- | Default stuff prompt
defaultStuffPrompt :: PromptTemplate
defaultStuffPrompt =
  fromTemplate
    ( "Use the following context to answer the question:\n\n"
        <> "Context:\n{context}\n\n"
        <> "Question: {question}\n\n"
        <> "Answer:"
    )

-- | Construct a new StuffDocumentsChain
newStuffDocumentsChain :: model -> PromptTemplate -> Text -> StuffDocumentsChain model
newStuffDocumentsChain m p docVar =
  StuffDocumentsChain
    { stuffModel = m
    , stuffPrompt = p
    , documentVariableName = docVar
    , documentSeparator = "\n\n"
    }

-- | Execute StuffDocumentsChain with documents and template variables
runStuffDocumentsChain
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => StuffDocumentsChain model
  -> [Document]
  -> Map Text Text
  -> m Message
runStuffDocumentsChain StuffDocumentsChain {..} docs vars = do
  let combinedDocs = T.intercalate documentSeparator $ map (TL.toStrict . pageContent) docs
      allVars = Map.insert documentVariableName combinedDocs vars
  renderedPrompt <- case renderPrompt stuffPrompt allVars of
    Left err -> throwError err
    Right p -> pure p
  invoke stuffModel [userMessage renderedPrompt] Nothing

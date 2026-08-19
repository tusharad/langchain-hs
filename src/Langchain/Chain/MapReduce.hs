{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Chain.MapReduce
Description : Map-Reduce document summarization and synthesis chain
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Applies a map LLM prompt individually over each document, then combines and synthesizes results
using a reduce LLM prompt.
-}
module Langchain.Chain.MapReduce
  ( MapReduceChain (..)
  , newMapReduceChain
  , defaultMapPrompt
  , defaultReducePrompt
  , runMapReduceChain
  ) where

import Control.Monad (forM)
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
  , extractMessageText
  , userMessage
  )
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.PromptTemplate.Prompt (PromptTemplate, fromTemplate, renderPrompt)

-- | Map-Reduce chain configuration
data MapReduceChain model = MapReduceChain
  { mapReduceModel :: model
  , mapPromptTemplate :: PromptTemplate
  , reducePromptTemplate :: PromptTemplate
  , mapDocVar :: Text
  , reduceDocVar :: Text
  }

-- | Default map prompt for individual document summarization
defaultMapPrompt :: PromptTemplate
defaultMapPrompt =
  fromTemplate "Summarize the key information in the following document concisely:\n\n{document}\n\nSummary:"

-- | Default reduce prompt for synthesizing all document summaries
defaultReducePrompt :: PromptTemplate
defaultReducePrompt =
  fromTemplate "Combine and synthesize the following summaries into a comprehensive final response:\n\n{summaries}\n\nFinal Synthesis:"

-- | Construct a new MapReduceChain
newMapReduceChain :: model -> MapReduceChain model
newMapReduceChain m =
  MapReduceChain
    { mapReduceModel = m
    , mapPromptTemplate = defaultMapPrompt
    , reducePromptTemplate = defaultReducePrompt
    , mapDocVar = "document"
    , reduceDocVar = "summaries"
    }

-- | Execute MapReduceChain across documents
runMapReduceChain
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => MapReduceChain model
  -> [Document]
  -> Map Text Text
  -> m Message
runMapReduceChain MapReduceChain {..} docs baseVars = do
  -- Phase 1: Map over each document
  summaries <- forM docs $ \doc -> do
    let docTxt = TL.toStrict (pageContent doc)
        vars = Map.insert mapDocVar docTxt baseVars
    rendered <- case renderPrompt mapPromptTemplate vars of
      Left err -> throwError err
      Right p -> pure p
    resp <- invoke mapReduceModel [userMessage rendered] Nothing
    pure $ extractMessageText resp

  -- Phase 2: Reduce summaries into final synthesis
  let combinedSummaries = T.intercalate "\n\n---\n\n" summaries
      reduceVars = Map.insert reduceDocVar combinedSummaries baseVars
  renderedReduce <- case renderPrompt reducePromptTemplate reduceVars of
    Left err -> throwError err
    Right p -> pure p
  invoke mapReduceModel [userMessage renderedReduce] Nothing

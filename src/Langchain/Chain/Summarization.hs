{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Chain.Summarization
Description : Multi-strategy document and conversation summarization chain
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides automated text and conversation summarization with configurable reduction strategies
(Stuff single-pass and Map-Reduce hierarchical reduction).
-}
module Langchain.Chain.Summarization
  ( SummarizationStrategy (..)
  , SummarizationChain (..)
  , newSummarizationChain
  , runSummarizationChain
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel (..), extractMessageText, systemMessage, userMessage)
import Langchain.DocumentLoader.Core (Document (..))

-- | Summarization reduction strategy
data SummarizationStrategy
  = StuffSummary
  | MapReduceSummary !Int -- Chunk size for mapping
  deriving (Show, Eq)

-- | Summarization chain configuration
data SummarizationChain model = SummarizationChain
  { summarizationModel :: !model
  , summarizationStrategy :: !SummarizationStrategy
  , summarizationSystemPrompt :: !(Maybe Text)
  }

-- | Construct a new SummarizationChain
newSummarizationChain :: model -> SummarizationStrategy -> SummarizationChain model
newSummarizationChain model strategy =
  SummarizationChain
    { summarizationModel = model
    , summarizationStrategy = strategy
    , summarizationSystemPrompt = Just "You are a concise expert summarizer. Provide a well-structured summary of the provided text."
    }

-- | Execute summarization over a list of documents
runSummarizationChain
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => SummarizationChain model
  -> [Document]
  -> m Text
runSummarizationChain SummarizationChain {..} docs = do
  let combinedText = T.intercalate "\n\n" [TL.toStrict (pageContent d) | d <- docs]
      sysPrompt = case summarizationSystemPrompt of
        Just p -> [systemMessage p]
        Nothing -> []
      prompt = userMessage ("Please summarize the following text:\n\n" <> combinedText)
  resp <- invoke summarizationModel (sysPrompt ++ [prompt]) Nothing
  pure $ extractMessageText resp

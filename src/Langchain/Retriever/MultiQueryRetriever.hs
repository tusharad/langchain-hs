{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Retriever.MultiQueryRetriever
Description : Multi-query retrieval implementation for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Advanced retriever implementation that generates multiple queries from a single
input using a ChatModel to improve document recall.
-}
module Langchain.Retriever.MultiQueryRetriever
  ( MultiQueryRetriever (..)
  , QueryGenerationPrompt (..)
  , MultiQueryRetrieverConfig (..)
  , newMultiQueryRetriever
  , defaultQueryGenerationPrompt
  , newMultiQueryRetrieverWithConfig
  , defaultMultiQueryRetrieverConfig
  , generateQueries
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.List (nub)
import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, vectorStoreError)
import Langchain.Core.Model (ChatModel (..), extractMessageText, userMessage)
import Langchain.DocumentLoader.Core (Document)
import Langchain.OutputParser.Core (NumberSeparatedList (..), OutputParser (..))
import Langchain.PromptTemplate (PromptTemplate, fromTemplate, renderPrompt)
import Langchain.Retriever.Core (Retriever (..))

-- | Query generation prompt template
newtype QueryGenerationPrompt = QueryGenerationPrompt PromptTemplate
  deriving (Show, Eq)

-- | Default query generation prompt
defaultQueryGenerationPrompt :: QueryGenerationPrompt
defaultQueryGenerationPrompt =
  QueryGenerationPrompt $
    fromTemplate
      ( T.unlines
          [ "You are an AI language model assistant that helps users by generating multiple search queries based on their initial query."
          , "These queries should help retrieve relevant documents or information from a vector database."
          , ""
          , "Original query: {query}"
          , ""
          , "Please generate {num_queries} different versions of this query that will help the user find the most relevant information."
          , "The queries should be different but related to the original query."
          , "Return these queries in the following format: 1. query 1 \n 2. query 2 \n 3. query 3"
          , "Only return queries and nothing else"
          ]
      )

-- | Configuration for multi-query retrieval
data MultiQueryRetrieverConfig = MultiQueryRetrieverConfig
  { numQueries :: Int
  -- ^ Number of queries to generate
  , queryGenerationPrompt :: QueryGenerationPrompt
  -- ^ Prompt template for query generation
  , includeMergeDocs :: Bool
  -- ^ Whether to include merged documents
  , includeOriginalQuery :: Bool
  -- ^ Whether to include results from original query
  }
  deriving (Show, Eq)

-- | Default configuration
defaultMultiQueryRetrieverConfig :: MultiQueryRetrieverConfig
defaultMultiQueryRetrieverConfig =
  MultiQueryRetrieverConfig
    { numQueries = 3
    , queryGenerationPrompt = defaultQueryGenerationPrompt
    , includeMergeDocs = True
    , includeOriginalQuery = True
    }

-- | Multi-query retriever struct
data MultiQueryRetriever a model = MultiQueryRetriever
  { retriever :: a
  , model :: model
  , config :: MultiQueryRetrieverConfig
  }

-- | Create retriever with default settings
newMultiQueryRetriever :: a -> model -> MultiQueryRetriever a model
newMultiQueryRetriever r m =
  MultiQueryRetriever
    { retriever = r
    , model = m
    , config = defaultMultiQueryRetrieverConfig
    }

-- | Create retriever with custom configuration
newMultiQueryRetrieverWithConfig
  :: a
  -> model
  -> MultiQueryRetrieverConfig
  -> MultiQueryRetriever a model
newMultiQueryRetrieverWithConfig r m c =
  MultiQueryRetriever
    { retriever = r
    , model = m
    , config = c
    }

-- | Generate multiple query variants using ChatModel
generateQueries
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> QueryGenerationPrompt
  -> Text
  -> Int
  -> Bool
  -> m [Text]
generateQueries mdl (QueryGenerationPrompt promptTemplate) query n includeOriginal = do
  let vars = HM.fromList [("query", query), ("num_queries", T.pack $ show n)]
  case renderPrompt promptTemplate vars of
    Left err -> throwError err
    Right prompt -> do
      msg <- invoke mdl [userMessage prompt] Nothing
      let rawText = extractMessageText msg
      case parse rawText :: Either LangchainError NumberSeparatedList of
        Left err -> throwError err
        Right (NumberSeparatedList queries) -> do
          let uniqueQueries = nub $ filter (not . T.null) queries
          pure $
            if includeOriginal
              then query : uniqueQueries
              else uniqueQueries

-- | Combine documents from multiple queries
combineDocuments :: [[Document]] -> [Document]
combineDocuments = nub . concat

instance (Retriever a, ChatModel model) => Retriever (MultiQueryRetriever a model) where
  getRelevantDocuments r query = do
    let baseRetriever = retriever r
        mdl = model r
        cfg = config r

    queries <-
      generateQueries
        mdl
        (queryGenerationPrompt cfg)
        query
        (numQueries cfg)
        (includeOriginalQuery cfg)

    docLists <- mapM (getRelevantDocuments baseRetriever) queries
    let combined = combineDocuments docLists
    if null combined
      then throwError $ vectorStoreError "No valid results returned from any query variant" (Just "MultiQueryRetriever") Nothing
      else pure combined

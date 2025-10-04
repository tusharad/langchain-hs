{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Retriever.Core
Description : Retrieval mechanism implementation for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Haskell implementation of LangChain's retrieval abstraction, providing:

- Document retrieval based on semantic similarity
- Integration with vector stores
- Runnable interface for workflow composition

Example usage:

@
-- Hypothetical vector store instance
vectorStore :: MyVectorStore
vectorStore = ...

-- Create retriever
retriever :: VectorStoreRetriever MyVectorStore
retriever = VectorStoreRetriever vectorStore

-- Retrieve relevant documents
docs <- invoke retriever "Haskell programming"
-- Right [Document {pageContent = "...", ...}, ...]
@
-}
module Langchain.Retriever.Core
  ( Retriever (..)
  , VectorStoreRetriever (..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Langchain.DocumentLoader.Core (Document)
import Langchain.Error (LangchainResult)
import Langchain.Runnable.Core
import Langchain.VectorStore.Core

{- | Typeclass for document retrieval systems
Implementations should return documents relevant to a given query.

Example instance for a custom retriever:

@
data CustomRetriever = CustomRetriever

instance Retriever CustomRetriever where
  _get_relevant_documents _ query = do
    -- Custom retrieval logic
    return $ Right [Document ("Result for: " <> query) mempty]
@
-}
class Retriever a where
  {- | Retrieve documents relevant to the query

  Example:

  >>> _get_relevant_documents (VectorStoreRetriever myStore) "AI"
  Right [Document "AI definition...", ...]
  -}
  _get_relevant_documents :: a -> Text -> IO (LangchainResult [Document])

  _get_relevant_documentsM :: MonadIO m => a -> Text -> m (LangchainResult [Document])
  _get_relevant_documentsM retriever query = liftIO $ _get_relevant_documents retriever query

{- | Vector store-backed retriever implementation
Wraps any 'VectorStore' instance to provide similarity-based retrieval.

Example usage:

@
-- Using a hypothetical FAISS vector store
faissStore :: FAISSStore
faissStore = ...

-- Create vector store retriever
vsRetriever = VectorStoreRetriever faissStore

-- Get similar documents
docs <- _get_relevant_documents vsRetriever "machine learning"
-- Returns top 5 relevant documents by default
@
-}
newtype VectorStore a => VectorStoreRetriever a = VectorStoreRetriever {vs :: a}
  deriving (Eq, Show)

{- | Runnable interface for vector store retrievers
Allows integration with LangChain workflows and expressions.

Example:

>>> invoke (VectorStoreRetriever store) "Quantum computing"
Right [Document "Quantum theory...", ...]
-}
instance VectorStore a => Retriever (VectorStoreRetriever a) where
  _get_relevant_documents (VectorStoreRetriever v) query = similaritySearch v query 5

{- | Runnable interface for vector store retrievers
Allows integration with LangChain workflows and expressions.

Example:

>>> invoke (VectorStoreRetriever store) "Quantum computing"
Right [Document "Quantum theory...", ...]
-}
instance VectorStore a => Runnable (VectorStoreRetriever a) where
  type RunnableInput (VectorStoreRetriever a) = Text
  type RunnableOutput (VectorStoreRetriever a) = [Document]

  invoke = _get_relevant_documents

{- $examples
Test case patterns:
1. Basic retrieval
   >>> let retriever = VectorStoreRetriever mockStore
   >>> _get_relevant_documents retriever "Test"
   Right [Document "Test content" ...]

2. Runnable integration
   >>> run retriever "Hello"
   Right [Document "Greeting response" ...]

3. Error handling
   >>> _get_relevant_documents (VectorStoreRetriever invalidStore) "Query"
   Left "Vector store error"
-}

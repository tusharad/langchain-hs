{- |
Module      : Langchain.VectorStore.InMemory
Description : In-memory vector store implementation for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

In-memory vector store implementation following LangChain's patterns, supporting:

- Document storage with embeddings
- Cosine similarity search
- Integration with embedding models

Example usage:

@
-- Create store with Ollama embeddings
ollamaEmb = OllamaEmbeddings "nomic-embed" Nothing Nothing
inMem = emptyInMemoryVectorStore ollamaEmb

-- Add documents
docs = [Document "Hello World" mempty, Document "Haskell is functional" mempty]
updatedStore <- addDocuments inMem docs

-- Perform similarity search
results <- similaritySearch updatedStore "functional programming" 1
-- Right [Document "Haskell is functional"...]
@
-}
module Langchain.VectorStore.InMemory
  ( InMemory (..)
  , fromDocuments
  , emptyInMemoryVectorStore
  , norm
  , dotProduct
  , cosineSimilarity
  ) where

import Data.Bifunctor
import Data.Int (Int64)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Langchain.DocumentLoader.Core (Document)
import Langchain.Embeddings.Core
import Langchain.Error (LangchainError)
import Langchain.VectorStore.Core

{- | Compute dot product of two vectors
Example:

>>> dotProduct [1,2,3] [4,5,6]
32.0
-}
dotProduct :: [Float] -> [Float] -> Float
dotProduct a b = sum $ zipWith (*) a b

{- | Calculate Euclidean norm of a vector
Example:

>>> norm [3,4]
5.0
-}
norm :: [Float] -> Float
norm a = sqrt $ sum $ map (^ (2 :: Int)) a

{- | Calculate cosine similarity between vectors
Example:

>>> cosineSimilarity [1,2] [2,4]
1.0
-}
cosineSimilarity :: [Float] -> [Float] -> Float
cosineSimilarity a b = dotProduct a b / (norm a * norm b)

{- | Create empty in-memory store with embedding model
Example:

>>> emptyInMemoryVectorStore ollamaEmb
InMemory {_embeddingModel = ..., _store = empty}
-}
emptyInMemoryVectorStore :: Embeddings m => m -> InMemory m
emptyInMemoryVectorStore model = InMemory model Map.empty

{- | Initialize store from documents using embeddings
Example:

>>> fromDocuments ollamaEmb [Document "Test" mempty]
Right (InMemory {_store = ...})
-}
fromDocuments :: Embeddings m => m -> [Document] -> IO (Either LangchainError (InMemory m))
fromDocuments model docs = do
  let vs = emptyInMemoryVectorStore model
  addDocuments vs docs

{- | In-memory vector store implementation
Stores documents with:

- Embedding model reference
- Map of document IDs to (Document, embedding) pairs
-}
data Embeddings m => InMemory m = InMemory
  { embeddingModel :: m
  , store :: Map.Map Int64 (Document, [Float])
  }
  deriving (Show, Eq)

instance Embeddings m => VectorStore (InMemory m) where
  -- \| Add documents with generated embeddings
  --  Example:
  --
  --  >>> addDocuments inMem [doc1, doc2]
  --  Right (InMemory {_store = ...})
  --
  addDocuments inMem docs = do
    eRes <- embedDocuments (embeddingModel inMem) docs
    case eRes of
      Left err -> pure $ Left err
      Right floats -> do
        let currStore = store inMem
            mbMaxKey = Map.lookupMax currStore
            newStore =
              Map.fromList $
                zip
                  [(maybe 1 (\x -> fst x + 1) mbMaxKey) ..]
                  (zip docs floats)
            newInMem = inMem {store = Map.union newStore currStore}
        pure $ Right newInMem

  -- \| Delete documents by ID
  --  Example:
  --
  --  >>> delete inMem [1, 2]
  --  Right (InMemory {_store = ...})
  --
  delete inMem ids = do
    let currStore = store inMem
        newStore = foldl (flip Map.delete) currStore ids
        newInMem = inMem {store = newStore}
    pure $ Right newInMem

  -- \| Text-based similarity search
  --  Example:
  --
  --  >>> similaritySearch inMem "Haskell" 2
  --  Right [Document "Haskell is...", Document "Functional programming..."]
  --
  similaritySearch vs query k = do
    eQueryEmbedding <- embedQuery (embeddingModel vs) query
    case eQueryEmbedding of
      Left err -> return $ Left err
      Right queryVec -> similaritySearchByVector vs queryVec k

  -- \| Vector-based similarity search
  --  Uses cosine similarity for ranking
  --
  --  Example:
  --
  --  >>> similaritySearchByVector inMem [0.1, 0.3, ...] 3
  --  Right [mostRelevantDoc, ...]
  --
  similaritySearchByVector vs queryVec k = do
    let similarities =
          map
            (second (cosineSimilarity queryVec) . snd)
            (Map.toList $ store vs)
        sorted = sortBy (comparing (negate . snd)) similarities
        -- Sort in descending order
        topK = take k sorted
    return $ Right $ map fst topK

{-
ghci> let x = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
ghci> let inMem = emptyInMemoryVectorStore x
ghci> eRes <- addDocuments inMem [Document "Hello World" empty, Document "Nice to meet you" empty]
ghci> let newInMem = fromRight inMem eRes
ghci> similaritySearch newInMem "World" 1
Right [Document {pageContent = "Hello World", metadata = fromList []}]
ghci> similaritySearch newInMem "Meet you" 1
Right [Document {pageContent = "Nice to meet you", metadata = fromList []}]
-}

{- $examples
Test case patterns:
1. Document addition
   >>> addDocuments inMem [Document "Test" mempty]
   Right (InMemory {_store = ...})

2. Similarity search
   >>> similaritySearch inMem "World" 1
   Right [Document "Hello World"...]

3. Vector-based search
   >>> similaritySearchByVector inMem [0.5, 0.5] 1
   Right [mostSimilarDoc]
-}

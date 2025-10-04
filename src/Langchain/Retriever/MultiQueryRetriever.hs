{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Retriever.MultiQueryRetriever
Description : Multi-query retrieval implementation for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Advanced retriever implementation that generates multiple queries from a single
input to improve document retrieval. Integrates with LLMs for query expansion
and vector stores for document retrieval

Example usage:

@
-- Create components
ollamaLLM = Ollama "llama3" []
vs = VectorStoreRetriever (createVectorStore ...)

-- Create retriever with default config
mqRetriever = newMultiQueryRetriever vs ollamaLLM

-- Retrieve documents
docs <- _get_relevant_documents mqRetriever "Haskell features"
-- Returns combined results from multiple generated queries
@
-}
module Langchain.Retriever.MultiQueryRetriever
  ( MultiQueryRetriever (..)
  , QueryGenerationPrompt (..)
  , newMultiQueryRetriever
  , defaultQueryGenerationPrompt
  , newMultiQueryRetrieverWithConfig
  , defaultMultiQueryRetrieverConfig
  , generateQueries
  ) where

import Langchain.DocumentLoader.Core (Document)
import Langchain.LLM.Core (LLM (..))
import Langchain.OutputParser.Core (NumberSeparatedList (..), OutputParser (..))
import Langchain.PromptTemplate (PromptTemplate (..), renderPrompt)
import Langchain.Retriever.Core (Retriever (..))
import qualified Langchain.Runnable.Core as Run

import Data.Either (rights)
import Data.List (nub)
import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Error (LangchainError, llmError)

{- | Query generation prompt template
Controls how the LLM generates multiple query variants from the original query.

Example prompt structure:

@
"You are an AI assistant... Original query: {query}... Generate {num_queries} versions..."
@
-}
newtype QueryGenerationPrompt = QueryGenerationPrompt PromptTemplate
  deriving (Show, Eq)

{- | Default query generation prompt
Generates 3 query variants in numbered list format. Includes instructions for
query diversity and formatting.
-}
defaultQueryGenerationPrompt :: QueryGenerationPrompt
defaultQueryGenerationPrompt =
  QueryGenerationPrompt $
    PromptTemplate
      { templateString =
          T.unlines
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
      }

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

{- | Default configuration
- 3 generated queries
- Includes original query results
- Uses default query generation prompt
-}
defaultMultiQueryRetrieverConfig :: MultiQueryRetrieverConfig
defaultMultiQueryRetrieverConfig =
  MultiQueryRetrieverConfig
    { numQueries = 3
    , queryGenerationPrompt = defaultQueryGenerationPrompt
    , includeMergeDocs = True
    , includeOriginalQuery = True
    }

{- | Multi-query retriever implementation
Generates multiple queries using an LLM, retrieves documents for each query,
and combines results. Improves recall by exploring different query formulations.

Example instance:

@
mqRetriever = MultiQueryRetriever
  { retriever = vectorStoreRetriever
  , llm = ollamaLLM
  , config = defaultMultiQueryRetrieverConfig
  }
@
-}
data (Retriever a, LLM m) => MultiQueryRetriever a m = MultiQueryRetriever
  { retriever :: a
  -- ^ The base retriever
  , llm :: m
  -- ^ The language model for generating queries
  , config :: MultiQueryRetrieverConfig
  -- ^ Configuration
  }

{- | Create retriever with default settings
Example:

>>> newMultiQueryRetriever vsRetriever ollamaLLM
MultiQueryRetriever {numQueries = 3, ...}
-}
newMultiQueryRetriever :: (Retriever a, LLM m) => a -> m -> MultiQueryRetriever a m
newMultiQueryRetriever r l =
  MultiQueryRetriever
    { retriever = r
    , llm = l
    , config = defaultMultiQueryRetrieverConfig
    }

{- | Create retriever with custom configuration
Example:

>>> let customCfg = defaultMultiQueryRetrieverConfig { numQueries = 5 }
>>> newMultiQueryRetrieverWithConfig vsRetriever ollamaLLM customCfg
MultiQueryRetriever {numQueries = 5, ...}
-}
newMultiQueryRetrieverWithConfig ::
  (Retriever a, LLM m) =>
  a ->
  m ->
  MultiQueryRetrieverConfig ->
  MultiQueryRetriever a m
newMultiQueryRetrieverWithConfig r l c =
  MultiQueryRetriever
    { retriever = r
    , llm = l
    , config = c
    }

{- | Generate multiple query variants using LLM
Example:

>>> generateQueries ollamaLLM prompt "Haskell" 3 True
Right ["Haskell", "Haskell features", "Haskell applications"]
-}
generateQueries ::
  LLM m => m -> QueryGenerationPrompt -> Text -> Int -> Bool -> IO (Either LangchainError [Text])
generateQueries model (QueryGenerationPrompt promptTemplate) query n includeOriginal = do
  let vars = HM.fromList [("query", query), ("num_queries", T.pack $ show n)]
  case renderPrompt promptTemplate vars of
    Left err -> return $ Left err
    Right prompt -> do
      result <- generate model prompt Nothing
      case result of
        Left err -> return $ Left err
        Right response -> do
          case parse response :: Either LangchainError NumberSeparatedList of
            Left err -> return $ Left err
            Right (NumberSeparatedList queries) -> do
              let uniqueQueries = nub $ filter (not . T.null) queries
              return $
                Right $
                  if includeOriginal
                    then query : uniqueQueries
                    else uniqueQueries

{- | Combine documents from multiple queries
Removes duplicates while maintaining order (simplified approach).
-}
combineDocuments :: [[Document]] -> [Document]
combineDocuments docLists =
  -- This is a simplified approach. In a production system, you'd want a more
  -- sophisticated way to identify and rank duplicate documents
  nub $ concat docLists

{- | Retriever instance implementation
1. Generates multiple queries using LLM
2. Retrieves documents for each query
3. Combines and deduplicates results

Example retrieval:

>>> _get_relevant_documents mqRetriever "Haskell"
Right [Document "Haskell is...", Document "Functional programming...", ...]
-}
instance (Retriever a, LLM m) => Retriever (MultiQueryRetriever a m) where
  _get_relevant_documents r query = do
    let baseRetriever = retriever r
        model = llm r
        cfg = config r

    -- Generate multiple queries
    queriesResult <-
      generateQueries
        model
        (queryGenerationPrompt cfg)
        query
        (numQueries cfg)
        (includeOriginalQuery cfg)

    case queriesResult of
      Left err -> return $ Left err
      Right queries -> do
        -- Get documents for each query
        results <- mapM (_get_relevant_documents baseRetriever) queries

        -- Filter successful results
        let validResults = rights results

        if null validResults
          then return $ Left (llmError "No valid results from any query" Nothing Nothing)
          else return $ Right $ combineDocuments validResults

{-
 ghci> :set -XOverloadedStrings
 ghci> let ollamaEmbed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
 ghci> let vs = emptyInMemoryVectorStore ollamaEmbed
 ghci> import Data.Map (empty)
 ghci> import Data.Either
 ghci> newVs <- addDocuments vs [Document "Tushar is 25 years old." empty]
 ghci> let newVs_ = fromRight vs newVs
 ghci> let vRet = VectorStoreRetriever newVs_
 ghci> let ollamLLM = Ollama "llama3.2" []
 ghci> let mqRet = newMultiQueryRetriever vRet ollamLLM
 ghci> documents <- _get_relevant_documents mqRet "How old is Tushar?"
 ghci> documents
    Right [Document {pageContent = "Tushar is 25 years old.", metadata = fromList []}]
 -}

{- | Runnable interface implementation
Allows integration with LangChain workflows:

>>> invoke mqRetriever "AI applications"
Right [Document "Machine learning...", ...]
-}
instance (Retriever a, LLM m) => Run.Runnable (MultiQueryRetriever a m) where
  type RunnableInput (MultiQueryRetriever a m) = Text
  type RunnableOutput (MultiQueryRetriever a m) = [Document]

  invoke = _get_relevant_documents

{- $examples
Test case patterns:
1. Query generation
   >>> generateQueries ollamaLLM prompt "Test" 2 False
   Right ["Test case", "Test example"]

2. Full retrieval flow
   >>> _get_relevant_documents mqRetriever "Haskell"
   Right [Document "Functional...", Document "Type system..."]

3. Configuration variants
   >>> let cfg = defaultMultiQueryRetrieverConfig { numQueries = 5 }
   >>> newMultiQueryRetrieverWithConfig vsRetriever ollamaLLM cfg
   MultiQueryRetriever {numQueries = 5, ...}
-}

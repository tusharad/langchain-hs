{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.RAG.HybridRetriever
Description : Hybrid retrieval combining vector similarity and symbol graph traversal
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Hybrid retrieval engine that combines dense vector similarity search with
structural symbol dependency graph traversal. Supports multi-query expansion
for comprehensive context retrieval across code dependencies.
-}
module Aegis.RAG.HybridRetriever
  ( -- * Retriever
    HybridRetriever (..)
  , newHybridRetriever

    -- * Retrieval
  , hybridRetrieve
  , expandQueries
  , retrieveBySymbolGraph

    -- * Configuration
  , HybridRetrieverConfig (..)
  , defaultHybridRetrieverConfig

    -- * Result Types
  , RetrievalResult (..)
  , RetrievalSource (..)
  , rankResults
  ) where

import Control.Concurrent.STM
import Data.List (nubBy, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Aegis.Core.Types.AST

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the hybrid retriever
data HybridRetrieverConfig = HybridRetrieverConfig
  { hrMaxResults :: Int
  -- ^ Maximum number of results to return
  , hrVectorWeight :: Double
  -- ^ Weight for vector similarity results (0.0 to 1.0)
  , hrGraphWeight :: Double
  -- ^ Weight for graph traversal results (0.0 to 1.0)
  , hrMaxGraphDepth :: Int
  -- ^ Maximum depth for graph traversal
  , hrQueryExpansionCount :: Int
  -- ^ Number of expanded query variants to generate
  , hrMinRelevanceScore :: Double
  -- ^ Minimum relevance score threshold
  }
  deriving (Eq, Show)

-- | Default config: balanced vector/graph weights, depth 2, 10 results
defaultHybridRetrieverConfig :: HybridRetrieverConfig
defaultHybridRetrieverConfig = HybridRetrieverConfig
  { hrMaxResults = 10
  , hrVectorWeight = 0.6
  , hrGraphWeight = 0.4
  , hrMaxGraphDepth = 2
  , hrQueryExpansionCount = 3
  , hrMinRelevanceScore = 0.1
  }

-- ---------------------------------------------------------------------------
-- Retrieval Result Types
-- ---------------------------------------------------------------------------

-- | Source of a retrieval result
data RetrievalSource
  = VectorSearch
  -- ^ Result from vector similarity search
  | GraphTraversal
  -- ^ Result from symbol graph traversal
  | QueryExpansion
  -- ^ Result from expanded query variant
  deriving (Eq, Ord, Show)

-- | A single retrieval result with scoring metadata
data RetrievalResult = RetrievalResult
  { rrChunk :: CodeChunk
  -- ^ The retrieved code chunk
  , rrScore :: Double
  -- ^ Relevance score (0.0 to 1.0)
  , rrSource :: RetrievalSource
  -- ^ How this result was found
  , rrQuery :: Text
  -- ^ The query that produced this result
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Hybrid Retriever
-- ---------------------------------------------------------------------------

-- | In-memory code chunk index for retrieval
data HybridRetriever = HybridRetriever
  { hrConfig :: HybridRetrieverConfig
  -- ^ Retriever configuration
  , hrChunks :: TVar [CodeChunk]
  -- ^ All indexed code chunks
  , hrSymbolGraph :: TVar SymbolGraph
  -- ^ Symbol dependency graph
  , hrChunkIndex :: TVar (Map Text [CodeChunk])
  -- ^ Inverted index: symbol -> chunks that define/use it
  , hrFileIndex :: TVar (Map FilePath [CodeChunk])
  -- ^ File path -> chunks mapping
  }

-- | Create a new empty hybrid retriever
newHybridRetriever :: HybridRetrieverConfig -> IO HybridRetriever
newHybridRetriever config = do
  chunks <- newTVarIO []
  sg <- newTVarIO emptySymbolGraph
  chunkIdx <- newTVarIO Map.empty
  fileIdx <- newTVarIO Map.empty
  pure HybridRetriever
    { hrConfig = config
    , hrChunks = chunks
    , hrSymbolGraph = sg
    , hrChunkIndex = chunkIdx
    , hrFileIndex = fileIdx
    }

-- | Index code chunks into the retriever
indexChunks :: HybridRetriever -> [CodeChunk] -> SymbolGraph -> IO ()
indexChunks retriever chunks sg = atomically $ do
  writeTVar (hrChunks retriever) chunks
  writeTVar (hrSymbolGraph retriever) sg
  -- Build inverted symbol index
  let symbolIdx = foldl (\acc chunk ->
        let symbols = chunkSymbolsDefined chunk ++ chunkSymbolsUsed chunk
        in foldl (\m s -> Map.insertWith (++) s [chunk] m) acc symbols
        ) Map.empty chunks
  writeTVar (hrChunkIndex retriever) symbolIdx
  -- Build file index
  let fileIdx = foldl (\acc chunk ->
        Map.insertWith (++) (chunkFilePath chunk) [chunk] acc
        ) Map.empty chunks
  writeTVar (hrFileIndex retriever) fileIdx

-- ---------------------------------------------------------------------------
-- Hybrid Retrieval
-- ---------------------------------------------------------------------------

-- | Perform hybrid retrieval combining vector search and graph traversal
hybridRetrieve :: HybridRetriever -> Text -> IO [RetrievalResult]
hybridRetrieve retriever query = do
  let config = hrConfig retriever
  -- Step 1: Vector-like similarity search (text matching as proxy)
  vectorResults <- vectorSearch retriever query
  -- Step 2: Symbol graph traversal
  graphResults <- retrieveBySymbolGraph retriever query
  -- Step 3: Query expansion for additional coverage
  expansionResults <- do
    let expanded = expandQueries query (hrQueryExpansionCount config)
    concat <$> mapM (vectorSearch retriever) expanded
  -- Step 4: Merge and rank all results
  let allResults = vectorResults ++ graphResults ++ expansionResults
      ranked = rankResults config allResults
  pure $ take (hrMaxResults config) ranked

-- | Simple text-matching vector search proxy
-- In production, this would use actual vector embeddings; here we use
-- keyword overlap scoring as a functional demonstration.
vectorSearch :: HybridRetriever -> Text -> IO [RetrievalResult]
vectorSearch retriever query = do
  chunks <- readTVarIO (hrChunks retriever)
  let queryWords = Set.fromList $ T.words (T.toLower query)
      scored = [(chunk, scoreChunk queryWords chunk) | chunk <- chunks]
      filtered = [(c, s) | (c, s) <- scored, s > 0]
      sorted = sortBy (comparing (Down . snd)) filtered
  pure $ map (\(c, s) -> RetrievalResult c s VectorSearch query) (take 20 sorted)

-- | Score a chunk against query words using keyword overlap
scoreChunk :: Set Text -> CodeChunk -> Double
scoreChunk queryWords chunk =
  let chunkWords = Set.fromList $ T.words $ T.toLower $ T.unlines
        [ chunkContent chunk
        , maybe "" id (chunkScopeName chunk)
        , chunkContext chunk
        ]
      overlap = Set.intersection queryWords chunkWords
      overlapSize = fromIntegral (Set.size overlap) :: Double
      querySize = fromIntegral (Set.size queryWords) :: Double
  in if querySize == 0 then 0 else overlapSize / querySize

-- | Retrieve chunks related to symbols mentioned in the query via graph traversal
retrieveBySymbolGraph :: HybridRetriever -> Text -> IO [RetrievalResult]
retrieveBySymbolGraph retriever query = do
  sg <- readTVarIO (hrSymbolGraph retriever)
  chunkIdx <- readTVarIO (hrChunkIndex retriever)
  let config = hrConfig retriever
      queryWords = T.words query
      -- Find symbols mentioned in the query
      knownSymbols = allSymbols sg
      matchedSymbols = [w | w <- queryWords, w `Set.member` knownSymbols]
      -- Traverse graph to find related symbols
      relatedSymbols = concatMap (\s -> traverseGraph sg s (hrMaxGraphDepth config)) matchedSymbols
      -- Look up chunks for related symbols
      relatedChunks = concatMap (\s -> Map.findWithDefault [] s chunkIdx) relatedSymbols
      -- Score based on graph distance
      scored = nubBy (\a b -> chunkIdentifier (rrChunk a) == chunkIdentifier (rrChunk b)) $
        zipWith (\c idx ->
          RetrievalResult c (1.0 / fromIntegral (idx + 1)) GraphTraversal query
        ) relatedChunks [0..]
  pure scored

-- | Traverse the symbol graph to find related symbols up to a depth
traverseGraph :: SymbolGraph -> Text -> Int -> [Text]
traverseGraph sg startSymbol maxDepth = go [startSymbol] Set.empty 0
  where
    go [] _ _ = []
    go _ _ depth | depth >= maxDepth = []
    go frontier visited depth =
      let newSymbols = concatMap (\s ->
            Set.toList (symbolDependencies s sg) ++
            Set.toList (symbolDependents s sg)
            ) frontier
          unvisited = filter (`Set.notMember` visited) newSymbols
          visited' = Set.union visited (Set.fromList frontier)
      in frontier ++ go unvisited visited' (depth + 1)

-- ---------------------------------------------------------------------------
-- Query Expansion
-- ---------------------------------------------------------------------------

-- | Expand a query into multiple search variants
-- Uses simple heuristic rewriting (in production, this would use an LLM)
expandQueries :: Text -> Int -> [Text]
expandQueries query count =
  take count $ filter (/= query)
    [ query <> " definition"
    , query <> " usage"
    , query <> " type signature"
    , "function " <> query
    , "data " <> query
    , query <> " implementation"
    , query <> " module"
    , "import " <> query
    , query <> " error handling"
    , query <> " test"
    ]

-- ---------------------------------------------------------------------------
-- Result Ranking
-- ---------------------------------------------------------------------------

-- | Rank and deduplicate retrieval results
rankResults :: HybridRetrieverConfig -> [RetrievalResult] -> [RetrievalResult]
rankResults config results =
  let -- Apply source-specific weights
      weighted = map (\r -> r { rrScore = applyWeight config r }) results
      -- Deduplicate by chunk identifier, keeping highest score
      deduped = Map.elems $ foldl (\acc r ->
        let key = chunkIdentifier (rrChunk r)
        in Map.insertWith (\new old -> if rrScore new > rrScore old then new else old) key r acc
        ) Map.empty weighted
      -- Filter by minimum score
      filtered = filter (\r -> rrScore r >= hrMinRelevanceScore config) deduped
      -- Sort by score descending
      sorted = sortBy (comparing (Down . rrScore)) filtered
  in sorted

-- | Apply source-specific weight to a result score
applyWeight :: HybridRetrieverConfig -> RetrievalResult -> Double
applyWeight config r = case rrSource r of
  VectorSearch   -> rrScore r * hrVectorWeight config
  GraphTraversal -> rrScore r * hrGraphWeight config
  QueryExpansion -> rrScore r * hrVectorWeight config * 0.8

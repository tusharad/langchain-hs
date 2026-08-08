{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.RAG.Indexer
Description : Orchestrates the full codebase indexing pipeline
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Orchestrates the complete RAG indexing pipeline: loads codebase via 'CodeLoader',
chunks via 'ASTChunker', builds symbol dependency graph, and populates the
'HybridRetriever' for subsequent searches by agents.
-}
module Aegis.RAG.Indexer
  ( -- * Indexing
    IndexResult (..)
  , indexCodebase
  , indexFiles
  , rebuildIndex

    -- * Symbol Graph Construction
  , buildSymbolGraphFromNodes
  , mergeSymbolGraphs
  ) where

import Control.Concurrent.STM (atomically, writeTVar, readTVarIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime)

import Aegis.Core.Types.AST
import Aegis.Core.Types.Pipeline (PipelineEvent (..), EventSeverity (..), PipelinePhase (..))
import Aegis.RAG.ASTChunker (ChunkerConfig, defaultChunkerConfig)
import Aegis.RAG.CodeLoader
import Aegis.RAG.HybridRetriever

-- ---------------------------------------------------------------------------
-- Index Result
-- ---------------------------------------------------------------------------

-- | Result of a codebase indexing operation
data IndexResult = IndexResult
  { irTotalFiles :: Int
  -- ^ Number of files processed
  , irTotalChunks :: Int
  -- ^ Number of code chunks generated
  , irTotalSymbols :: Int
  -- ^ Number of symbols indexed
  , irTotalLines :: Int
  -- ^ Total lines of code processed
  , irLanguageBreakdown :: Map Language Int
  -- ^ File count per language
  , irErrors :: [Text]
  -- ^ Non-fatal errors encountered during indexing
  , irDurationSeconds :: Double
  -- ^ Time taken for indexing
  , irEvents :: [PipelineEvent]
  -- ^ Events generated during indexing
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Indexing Pipeline
-- ---------------------------------------------------------------------------

-- | Index an entire codebase into the hybrid retriever
indexCodebase :: HybridRetriever -> CodeLoaderConfig -> FilePath -> IO IndexResult
indexCodebase retriever loaderConfig repoPath = do
  startTime <- getCurrentTime

  -- Step 1: Load all source files
  loadResult <- loadCodebase loaderConfig repoPath
  case loadResult of
    Left err -> do
      endTime <- getCurrentTime
      let duration = realToFrac (diffUTCTime endTime startTime)
      pure IndexResult
        { irTotalFiles = 0
        , irTotalChunks = 0
        , irTotalSymbols = 0
        , irTotalLines = 0
        , irLanguageBreakdown = Map.empty
        , irErrors = [err]
        , irDurationSeconds = duration
        , irEvents = [mkEvent "Indexer" ("Codebase loading failed: " <> err) EventError]
        }
    Right loadedFiles -> indexFiles retriever loadedFiles startTime

-- | Index pre-loaded files into the hybrid retriever
indexFiles :: HybridRetriever -> [LoadedFile] -> UTCTime -> IO IndexResult
indexFiles retriever loadedFiles startTime = do
  -- Step 2: Collect all chunks
  let allChunks = concatMap lfChunks loadedFiles
      allNodes = map lfNode loadedFiles

  -- Step 3: Build symbol graph from code nodes
  let symbolGraph = buildSymbolGraphFromNodes allNodes

  -- Step 4: Populate the retriever
  atomically $ do
    writeTVar (hrChunks retriever) allChunks
    writeTVar (hrSymbolGraph retriever) symbolGraph
    -- Build chunk index
    let chunkIdx = foldl (\acc chunk ->
          let symbols = chunkSymbolsDefined chunk ++ chunkSymbolsUsed chunk
          in foldl (\m s -> Map.insertWith (++) s [chunk] m) acc symbols
          ) Map.empty allChunks
    writeTVar (hrChunkIndex retriever) chunkIdx
    -- Build file index
    let fileIdx = foldl (\acc chunk ->
          Map.insertWith (++) (chunkFilePath chunk) [chunk] acc
          ) Map.empty allChunks
    writeTVar (hrFileIndex retriever) fileIdx

  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime)
      languageBreakdown = foldl (\acc lf ->
        Map.insertWith (+) (lfLanguage lf) 1 acc) Map.empty loadedFiles
      totalSymbols = Set.size (allSymbols symbolGraph)

  pure IndexResult
    { irTotalFiles = length loadedFiles
    , irTotalChunks = length allChunks
    , irTotalSymbols = totalSymbols
    , irTotalLines = sum (map lfLineCount loadedFiles)
    , irLanguageBreakdown = languageBreakdown
    , irErrors = []
    , irDurationSeconds = duration
    , irEvents =
        [ mkEvent "Indexer" ("Indexed " <> T.pack (show (length loadedFiles)) <> " files, "
          <> T.pack (show (length allChunks)) <> " chunks, "
          <> T.pack (show totalSymbols) <> " symbols in "
          <> T.pack (show duration) <> "s") EventInfo
        ]
    }

-- | Rebuild the index (clear and re-index)
rebuildIndex :: HybridRetriever -> CodeLoaderConfig -> FilePath -> IO IndexResult
rebuildIndex retriever loaderConfig repoPath = do
  -- Clear existing index
  atomically $ do
    writeTVar (hrChunks retriever) []
    writeTVar (hrSymbolGraph retriever) emptySymbolGraph
    writeTVar (hrChunkIndex retriever) Map.empty
    writeTVar (hrFileIndex retriever) Map.empty
  -- Re-index
  indexCodebase retriever loaderConfig repoPath

-- ---------------------------------------------------------------------------
-- Symbol Graph Construction
-- ---------------------------------------------------------------------------

-- | Build a unified symbol graph from a list of code nodes
buildSymbolGraphFromNodes :: [CodeNode] -> SymbolGraph
buildSymbolGraphFromNodes nodes =
  let -- Step 1: Register all defined symbols
      graphWithNodes = foldl registerNodeSymbols emptySymbolGraph nodes
      -- Step 2: Add edges based on imports and symbol usage
      graphWithEdges = foldl addNodeEdges graphWithNodes nodes
  in graphWithEdges

-- | Register all symbols from a code node into the graph
registerNodeSymbols :: SymbolGraph -> CodeNode -> SymbolGraph
registerNodeSymbols sg node =
  let fp = nodeFilePath node
      symbols = Set.toList (nodeSymbolsDefined node)
  in foldl (\g sym -> addSymbolNode sym fp g) sg symbols

-- | Add dependency edges for a code node
addNodeEdges :: SymbolGraph -> CodeNode -> SymbolGraph
addNodeEdges sg node =
  let defined = nodeSymbolsDefined node
      used = nodeSymbolsUsed node
      allKnown = allSymbols sg
      -- For each used symbol, if it's defined elsewhere, add an edge
      externalUsed = Set.intersection used allKnown `Set.difference` defined
  in Set.foldl (\g usedSym ->
       -- Each defined symbol in this file depends on the external symbol
       Set.foldl (\g' defSym -> addSymbolEdge defSym usedSym g') g defined
     ) sg externalUsed

-- | Merge two symbol graphs
mergeSymbolGraphs :: SymbolGraph -> SymbolGraph -> SymbolGraph
mergeSymbolGraphs sg1 sg2 = SymbolGraph
  { sgNodes = Map.unionWith Set.union (sgNodes sg1) (sgNodes sg2)
  , sgReverseNodes = Map.unionWith Set.union (sgReverseNodes sg1) (sgReverseNodes sg2)
  , sgSymbolToFile = Map.union (sgSymbolToFile sg1) (sgSymbolToFile sg2)
  , sgFileToSymbols = Map.unionWith Set.union (sgFileToSymbols sg1) (sgFileToSymbols sg2)
  }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkEvent :: Text -> Text -> EventSeverity -> PipelineEvent
mkEvent agent msg sev = PipelineEvent
  { eventTimestamp = Nothing
  , eventSeverity = sev
  , eventPhase = PhaseIndexing
  , eventAgent = agent
  , eventMessage = msg
  , eventMetadata = Map.empty
  }

diffUTCTime :: UTCTime -> UTCTime -> Double
diffUTCTime end start =
  let diff = realToFrac (diffTime end start) :: Double
  in diff
  where
    diffTime a b = toRational (a `diffUTC` b)
    diffUTC a b = a `seq` b `seq` 0  -- Placeholder; real impl uses Data.Time

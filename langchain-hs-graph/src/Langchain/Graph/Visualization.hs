{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.Visualization
Description : Graphviz DOT format exporter for StateGraph
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Exports StateGraph instances to standard Graphviz DOT format for architectural visualization.
-}
module Langchain.Graph.Visualization
  ( toDot
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Graph.StateGraph

-- | Export a StateGraph structure to Graphviz DOT format
toDot :: StateGraph s m -> Text
toDot StateGraph {..} =
  T.unlines $
    [ "digraph StateGraph {"
    , "  rankdir=LR;"
    , "  node [shape=box, style=\"rounded,filled\", fillcolor=\"#f0f4f8\", fontname=\"Helvetica\"];"
    , "  edge [fontname=\"Helvetica\", fontsize=10];"
    , ""
    , "  // Special Nodes"
    , "  \"" <> startNodeId <> "\" [shape=circle, fillcolor=\"#d1fae5\", label=\"Start\"];"
    , "  \"" <> endNodeId <> "\" [shape=doublecircle, fillcolor=\"#fee2e2\", label=\"End\"];"
    , ""
    , "  // Regular Nodes"
    ]
      ++ [ "  \"" <> nId <> "\" [label=\"" <> nId <> "\"];"
         | nId <- Map.keys graphNodes
         , nId /= startNodeId && nId /= endNodeId
         ]
      ++ [ ""
         , "  // Edges"
         ]
      ++ [ case edge of
             StaticEdge target -> "  \"" <> fromNode <> "\" -> \"" <> target <> "\";"
             ConditionalEdge _ -> "  \"" <> fromNode <> "\" -> \"(conditional)\" [style=dashed];"
         | (fromNode, edge) <- Map.toList graphEdges
         ]
      ++ ["}"]

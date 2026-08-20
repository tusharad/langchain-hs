{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Core.Types.AST
Description : Language-agnostic AST representation for code analysis
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Language-agnostic abstract syntax tree representation used for structural code analysis,
scope-preserving chunking, and symbol dependency graph construction. These types power
the RAG pipeline's AST-aware code understanding capabilities.
-}
module Aegis.Core.Types.AST
  ( -- * Language Classification
    Language (..)
  , detectLanguage
  , languageExtensions

    -- * Code Scope
  , ScopeKind (..)
  , CodeScope (..)
  , scopeQualifiedName
  , scopeLineCount

    -- * Code Nodes
  , CodeNode (..)
  , emptyCodeNode

    -- * Symbol Graph
  , SymbolGraph (..)
  , emptySymbolGraph
  , addSymbolNode
  , addSymbolEdge
  , lookupSymbol
  , symbolDependencies
  , symbolDependents
  , allSymbols
  , connectedComponents

    -- * Code Chunk (for RAG)
  , CodeChunk (..)
  , chunkIdentifier

    -- * File Summary
  , FileSummary (..)
  , emptyFileSummary
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath (takeExtension)

-- ---------------------------------------------------------------------------
-- Language Classification
-- ---------------------------------------------------------------------------

-- | Supported programming languages for analysis
data Language
  = Haskell
  | Python
  | Rust
  | JavaScript
  | TypeScript
  | Go
  | Java
  | CSharp
  | Cpp
  | C
  | Ruby
  | Shell
  | Markdown
  | YAML
  | JSON
  | TOML
  | UnknownLanguage Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Detect programming language from file extension
detectLanguage :: FilePath -> Language
detectLanguage path = case takeExtension path of
  ".hs"    -> Haskell
  ".lhs"   -> Haskell
  ".py"    -> Python
  ".pyi"   -> Python
  ".rs"    -> Rust
  ".js"    -> JavaScript
  ".jsx"   -> JavaScript
  ".ts"    -> TypeScript
  ".tsx"   -> TypeScript
  ".go"    -> Go
  ".java"  -> Java
  ".cs"    -> CSharp
  ".cpp"   -> Cpp
  ".cc"    -> Cpp
  ".c"     -> C
  ".h"     -> C
  ".hpp"   -> Cpp
  ".rb"    -> Ruby
  ".sh"    -> Shell
  ".bash"  -> Shell
  ".md"    -> Markdown
  ".yaml"  -> YAML
  ".yml"   -> YAML
  ".json"  -> JSON
  ".toml"  -> TOML
  ext      -> UnknownLanguage (T.pack ext)

-- | Get file extensions for a language
languageExtensions :: Language -> [Text]
languageExtensions Haskell    = [".hs", ".lhs"]
languageExtensions Python     = [".py", ".pyi"]
languageExtensions Rust       = [".rs"]
languageExtensions JavaScript = [".js", ".jsx"]
languageExtensions TypeScript = [".ts", ".tsx"]
languageExtensions Go         = [".go"]
languageExtensions Java       = [".java"]
languageExtensions CSharp     = [".cs"]
languageExtensions Cpp        = [".cpp", ".cc", ".hpp"]
languageExtensions C          = [".c", ".h"]
languageExtensions Ruby       = [".rb"]
languageExtensions Shell      = [".sh", ".bash"]
languageExtensions Markdown   = [".md"]
languageExtensions YAML       = [".yaml", ".yml"]
languageExtensions JSON       = [".json"]
languageExtensions TOML       = [".toml"]
languageExtensions (UnknownLanguage ext) = [ext]

-- ---------------------------------------------------------------------------
-- Code Scope
-- ---------------------------------------------------------------------------

-- | Classification of syntactic scopes within a source file
data ScopeKind
  = ModuleScope
  -- ^ Top-level module/file scope
  | FunctionScope
  -- ^ Function or procedure definition
  | ClassScope
  -- ^ Class/typeclass/trait definition
  | MethodScope
  -- ^ Method within a class
  | DataDeclScope
  -- ^ Data type / struct / enum declaration
  | InterfaceScope
  -- ^ Interface / protocol definition
  | InstanceScope
  -- ^ Instance / implementation block
  | BlockScope
  -- ^ Generic block scope (let, where, do)
  | ImportScope
  -- ^ Import/include block
  | TestScope
  -- ^ Test function or test suite
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, ToJSON, FromJSON)

-- | A scoped region within a source file
data CodeScope = CodeScope
  { scopeKind :: ScopeKind
  -- ^ Kind of syntactic scope
  , scopeName :: Text
  -- ^ Name of the scope (function name, class name, etc.)
  , scopeParentModule :: Maybe Text
  -- ^ Parent module name
  , scopeStartLine :: Int
  -- ^ Start line (1-indexed)
  , scopeEndLine :: Int
  -- ^ End line (1-indexed)
  , scopeSignature :: Maybe Text
  -- ^ Optional type signature or declaration
  , scopeExported :: Bool
  -- ^ Whether this scope is exported/public
  , scopeChildren :: [CodeScope]
  -- ^ Nested child scopes
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Get the qualified name of a scope (module.name)
scopeQualifiedName :: CodeScope -> Text
scopeQualifiedName scope = case scopeParentModule scope of
  Nothing -> scopeName scope
  Just modName -> modName <> "." <> scopeName scope

-- | Get the number of lines in a scope
scopeLineCount :: CodeScope -> Int
scopeLineCount scope = scopeEndLine scope - scopeStartLine scope + 1

-- ---------------------------------------------------------------------------
-- Code Nodes
-- ---------------------------------------------------------------------------

-- | A node in the parsed code structure, representing a complete parseable unit
data CodeNode = CodeNode
  { nodeFilePath :: FilePath
  -- ^ Source file path
  , nodeLanguage :: Language
  -- ^ Detected programming language
  , nodeModuleName :: Maybe Text
  -- ^ Module name (if applicable)
  , nodeImports :: [Text]
  -- ^ Import statements
  , nodeExports :: [Text]
  -- ^ Exported symbols
  , nodeScopes :: [CodeScope]
  -- ^ Top-level scopes within this file
  , nodeSymbolsDefined :: Set Text
  -- ^ All symbols defined in this file
  , nodeSymbolsUsed :: Set Text
  -- ^ All symbols referenced/used in this file
  , nodeLineCount :: Int
  -- ^ Total number of lines in the file
  , nodeRawContent :: Text
  -- ^ Raw source content
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create an empty code node for a file
emptyCodeNode :: FilePath -> Language -> CodeNode
emptyCodeNode fp lang =
  CodeNode
    { nodeFilePath = fp
    , nodeLanguage = lang
    , nodeModuleName = Nothing
    , nodeImports = []
    , nodeExports = []
    , nodeScopes = []
    , nodeSymbolsDefined = Set.empty
    , nodeSymbolsUsed = Set.empty
    , nodeLineCount = 0
    , nodeRawContent = ""
    }

-- ---------------------------------------------------------------------------
-- Symbol Graph
-- ---------------------------------------------------------------------------

-- | Directed graph representing symbol dependencies across the codebase
--
-- Nodes are qualified symbol names, edges represent dependencies
-- (\"A depends on B\" is represented as an edge from A to B).
data SymbolGraph = SymbolGraph
  { sgNodes :: Map Text (Set Text)
  -- ^ Forward adjacency: symbol -> set of symbols it depends on
  , sgReverseNodes :: Map Text (Set Text)
  -- ^ Reverse adjacency: symbol -> set of symbols that depend on it
  , sgSymbolToFile :: Map Text FilePath
  -- ^ Mapping from symbol to the file where it's defined
  , sgFileToSymbols :: Map FilePath (Set Text)
  -- ^ Mapping from file to symbols defined in it
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create an empty symbol graph
emptySymbolGraph :: SymbolGraph
emptySymbolGraph =
  SymbolGraph
    { sgNodes = Map.empty
    , sgReverseNodes = Map.empty
    , sgSymbolToFile = Map.empty
    , sgFileToSymbols = Map.empty
    }

-- | Add a symbol node to the graph, associated with a file
addSymbolNode :: Text -> FilePath -> SymbolGraph -> SymbolGraph
addSymbolNode sym fp sg =
  sg
    { sgNodes = Map.insertWith Set.union sym Set.empty (sgNodes sg)
    , sgReverseNodes = Map.insertWith Set.union sym Set.empty (sgReverseNodes sg)
    , sgSymbolToFile = Map.insert sym fp (sgSymbolToFile sg)
    , sgFileToSymbols = Map.insertWith Set.union fp (Set.singleton sym) (sgFileToSymbols sg)
    }

-- | Add a directed dependency edge: @from@ depends on @to@
addSymbolEdge :: Text -> Text -> SymbolGraph -> SymbolGraph
addSymbolEdge from to sg =
  sg
    { sgNodes = Map.insertWith Set.union from (Set.singleton to) (sgNodes sg)
    , sgReverseNodes = Map.insertWith Set.union to (Set.singleton from) (sgReverseNodes sg)
    }

-- | Look up which file defines a symbol
lookupSymbol :: Text -> SymbolGraph -> Maybe FilePath
lookupSymbol sym sg = Map.lookup sym (sgSymbolToFile sg)

-- | Get all symbols that a given symbol directly depends on
symbolDependencies :: Text -> SymbolGraph -> Set Text
symbolDependencies sym sg = Map.findWithDefault Set.empty sym (sgNodes sg)

-- | Get all symbols that directly depend on a given symbol
symbolDependents :: Text -> SymbolGraph -> Set Text
symbolDependents sym sg = Map.findWithDefault Set.empty sym (sgReverseNodes sg)

-- | Get all known symbols in the graph
allSymbols :: SymbolGraph -> Set Text
allSymbols sg = Map.keysSet (sgNodes sg)

-- | Compute connected components of the symbol graph (simplified BFS)
connectedComponents :: SymbolGraph -> [[Text]]
connectedComponents sg = go (Set.toList (allSymbols sg)) Set.empty
  where
    go [] _ = []
    go (s : rest) visited
      | s `Set.member` visited = go rest visited
      | otherwise =
          let (comp, visited') = bfs [s] Set.empty visited
           in comp : go rest visited'

    bfs [] found visited = (Set.toList found, visited)
    bfs (x : queue) found visited
      | x `Set.member` visited = bfs queue found visited
      | otherwise =
          let neighbors =
                Set.toList (symbolDependencies x sg)
                  ++ Set.toList (symbolDependents x sg)
           in bfs (queue ++ neighbors) (Set.insert x found) (Set.insert x visited)

-- ---------------------------------------------------------------------------
-- Code Chunk (for RAG)
-- ---------------------------------------------------------------------------

-- | A chunk of code suitable for embedding and retrieval,
-- preserving semantic boundaries from AST analysis.
data CodeChunk = CodeChunk
  { chunkFilePath :: FilePath
  -- ^ Source file this chunk came from
  , chunkLanguage :: Language
  -- ^ Programming language
  , chunkModuleName :: Maybe Text
  -- ^ Module name (if applicable)
  , chunkScopeName :: Maybe Text
  -- ^ Scope name (function, class, etc.)
  , chunkScopeKind :: Maybe ScopeKind
  -- ^ Kind of scope this chunk represents
  , chunkStartLine :: Int
  -- ^ Start line in original file
  , chunkEndLine :: Int
  -- ^ End line in original file
  , chunkContent :: Text
  -- ^ The actual code content
  , chunkSignature :: Maybe Text
  -- ^ Type signature or declaration (if available)
  , chunkSymbolsDefined :: [Text]
  -- ^ Symbols defined in this chunk
  , chunkSymbolsUsed :: [Text]
  -- ^ Symbols referenced in this chunk
  , chunkContext :: Text
  -- ^ Contextual summary for embedding enrichment
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Generate a unique identifier for a code chunk
chunkIdentifier :: CodeChunk -> Text
chunkIdentifier chunk =
  T.pack (chunkFilePath chunk)
    <> ":"
    <> T.pack (show (chunkStartLine chunk))
    <> "-"
    <> T.pack (show (chunkEndLine chunk))
    <> maybe "" (\n -> ":" <> n) (chunkScopeName chunk)

-- ---------------------------------------------------------------------------
-- File Summary
-- ---------------------------------------------------------------------------

-- | Aggregate summary of a parsed source file
data FileSummary = FileSummary
  { fsFilePath :: FilePath
  -- ^ File path
  , fsLanguage :: Language
  -- ^ Detected language
  , fsModuleName :: Maybe Text
  -- ^ Module name
  , fsLineCount :: Int
  -- ^ Total line count
  , fsScopeCount :: Int
  -- ^ Number of top-level scopes
  , fsSymbolCount :: Int
  -- ^ Number of defined symbols
  , fsImportCount :: Int
  -- ^ Number of imports
  , fsExportCount :: Int
  -- ^ Number of exports
  , fsComplexityEstimate :: Int
  -- ^ Rough complexity estimate (sum of scope nesting depths)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create an empty file summary
emptyFileSummary :: FilePath -> Language -> FileSummary
emptyFileSummary fp lang =
  FileSummary
    { fsFilePath = fp
    , fsLanguage = lang
    , fsModuleName = Nothing
    , fsLineCount = 0
    , fsScopeCount = 0
    , fsSymbolCount = 0
    , fsImportCount = 0
    , fsExportCount = 0
    , fsComplexityEstimate = 0
    }

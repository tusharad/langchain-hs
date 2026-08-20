{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.RAG.ASTChunker
Description : AST-aware code chunking for semantically intact RAG indexing
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

AST-aware text splitter that segments source code into semantically intact scopes
(function bodies, data declarations, class definitions) rather than arbitrary
line-based chunks. Preserves code structure for higher-quality embeddings.
-}
module Aegis.RAG.ASTChunker
  ( -- * Chunking
    chunkHaskellSource
  , chunkGenericSource
  , chunkSourceFile

    -- * Configuration
  , ChunkerConfig (..)
  , defaultChunkerConfig

    -- * Scope Detection
  , detectHaskellScopes
  , detectGenericScopes
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Aegis.Core.Types.AST

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the AST chunker
data ChunkerConfig = ChunkerConfig
  { ccMaxChunkLines :: Int
  -- ^ Maximum number of lines per chunk
  , ccMinChunkLines :: Int
  -- ^ Minimum number of lines per chunk (avoid tiny chunks)
  , ccOverlapLines :: Int
  -- ^ Number of lines to overlap between adjacent chunks
  , ccIncludeImports :: Bool
  -- ^ Whether to include import blocks as separate chunks
  , ccIncludeSignatures :: Bool
  -- ^ Whether to include type signatures with their implementations
  , ccContextPrefix :: Bool
  -- ^ Whether to add module/file context as a prefix to each chunk
  }
  deriving (Eq, Show)

-- | Default chunker: max 80 lines, min 5, 3 line overlap
defaultChunkerConfig :: ChunkerConfig
defaultChunkerConfig = ChunkerConfig
  { ccMaxChunkLines = 80
  , ccMinChunkLines = 5
  , ccOverlapLines = 3
  , ccIncludeImports = True
  , ccIncludeSignatures = True
  , ccContextPrefix = True
  }

-- ---------------------------------------------------------------------------
-- Main Chunking Entry Point
-- ---------------------------------------------------------------------------

-- | Chunk a source file based on its detected language
chunkSourceFile :: ChunkerConfig -> FilePath -> Text -> [CodeChunk]
chunkSourceFile config fp content =
  let lang = detectLanguage fp
  in case lang of
       Haskell -> chunkHaskellSource config fp content
       _       -> chunkGenericSource config fp lang content

-- ---------------------------------------------------------------------------
-- Haskell-Specific Chunking
-- ---------------------------------------------------------------------------

-- | Chunk Haskell source code preserving semantic boundaries
chunkHaskellSource :: ChunkerConfig -> FilePath -> Text -> [CodeChunk]
chunkHaskellSource config fp content =
  let ls = T.lines content
      moduleName = extractHaskellModuleName ls
      scopes = detectHaskellScopes ls
      importChunk = if ccIncludeImports config
                    then mkImportChunk fp moduleName ls
                    else []
      scopeChunks = concatMap (scopeToChunks config fp Haskell moduleName content) scopes
      -- Handle any remaining lines not covered by scopes
      coveredLines = concatMap (\s -> [scopeStartLine s .. scopeEndLine s]) scopes
      uncoveredChunks = mkUncoveredChunks config fp Haskell moduleName ls coveredLines
  in importChunk ++ scopeChunks ++ uncoveredChunks

-- | Detect Haskell top-level scopes (functions, data types, classes, instances)
detectHaskellScopes :: [Text] -> [CodeScope]
detectHaskellScopes ls = go (zip [1..] ls) Nothing []
  where
    go [] Nothing acc = reverse acc
    go [] (Just scope) acc = reverse (scope : acc)
    go ((n, line) : rest) mbCurrentScope acc
      -- Skip blank lines and comments at top level
      | T.null (T.strip line) = go rest mbCurrentScope acc
      | "--" `T.isPrefixOf` T.strip line = go rest mbCurrentScope acc
      | "{-" `T.isPrefixOf` T.strip line = go rest mbCurrentScope acc
      -- Detect top-level declarations (not indented)
      | isTopLevelDecl line =
          let finalized = case mbCurrentScope of
                Nothing -> acc
                Just s  -> s { scopeEndLine = n - 1 } : acc
              newScope = classifyTopLevelDecl n line
          in go rest (Just newScope) finalized
      -- Continuation of current scope (indented line)
      | not (T.null line) && (T.head line == ' ' || T.head line == '\t') =
          case mbCurrentScope of
            Just s  -> go rest (Just s { scopeEndLine = n }) acc
            Nothing -> go rest Nothing acc
      -- Another top-level thing without an explicit keyword
      | otherwise =
          case mbCurrentScope of
            Nothing -> go rest Nothing acc
            Just s  -> go rest (Just s { scopeEndLine = n }) acc

    isTopLevelDecl line =
      let stripped = T.strip line
      in not (T.null line)
         && T.head line /= ' '
         && T.head line /= '\t'
         && not ("--" `T.isPrefixOf` stripped)
         && not ("{-" `T.isPrefixOf` stripped)
         && not ("module " `T.isPrefixOf` stripped)
         && not ("import " `T.isPrefixOf` stripped)

    classifyTopLevelDecl :: Int -> Text -> CodeScope
    classifyTopLevelDecl n line =
      let stripped = T.strip line
          (kind, name) = classifyLine stripped
      in CodeScope
           { scopeKind = kind
           , scopeName = name
           , scopeParentModule = Nothing
           , scopeStartLine = n
           , scopeEndLine = n
           , scopeSignature = if "::" `T.isInfixOf` line then Just line else Nothing
           , scopeExported = True
           , scopeChildren = []
           }

    classifyLine :: Text -> (ScopeKind, Text)
    classifyLine line
      | "data " `T.isPrefixOf` line =
          (DataDeclScope, T.takeWhile (\c -> c /= ' ' && c /= '=') (T.drop 5 line))
      | "newtype " `T.isPrefixOf` line =
          (DataDeclScope, T.takeWhile (\c -> c /= ' ' && c /= '=') (T.drop 8 line))
      | "type " `T.isPrefixOf` line =
          (DataDeclScope, T.takeWhile (\c -> c /= ' ' && c /= '=') (T.drop 5 line))
      | "class " `T.isPrefixOf` line =
          (ClassScope, extractClassName line)
      | "instance " `T.isPrefixOf` line =
          (InstanceScope, T.takeWhile (/= ' ') (T.drop 9 line))
      | "::" `T.isInfixOf` line =
          (FunctionScope, T.strip (T.takeWhile (/= ':') line))
      | otherwise =
          (FunctionScope, T.takeWhile (\c -> c /= ' ' && c /= '=') line)

    extractClassName line =
      let ws = T.words (T.drop 6 line)
      in case filter (\w -> T.head w `elem` ['A'..'Z']) ws of
           (c : _) -> T.takeWhile (\ch -> ch /= ' ' && ch /= '(') c
           _ -> "Unknown"

-- ---------------------------------------------------------------------------
-- Generic Source Chunking
-- ---------------------------------------------------------------------------

-- | Chunk generic source code using indentation-based heuristics
chunkGenericSource :: ChunkerConfig -> FilePath -> Language -> Text -> [CodeChunk]
chunkGenericSource config fp lang content =
  let ls = T.lines content
      scopes = detectGenericScopes ls
      scopeChunks = concatMap (scopeToChunks config fp lang Nothing content) scopes
  in if null scopeChunks
     then fallbackLineChunks config fp lang content
     else scopeChunks

-- | Detect scopes in generic source code using indentation
detectGenericScopes :: [Text] -> [CodeScope]
detectGenericScopes ls = go (zip [1..] ls) []
  where
    go [] acc = reverse acc
    go ((n, line) : rest) acc
      | T.null (T.strip line) = go rest acc
      | isTopLevel line =
          let (body, remaining) = span (\(_, l) -> T.null (T.strip l) || isIndented l) rest
              endLine = if null body then n else fst (last body)
          in go remaining (mkGenericScope n endLine line : acc)
      | otherwise = go rest acc

    isTopLevel line = not (T.null line) && T.head line /= ' ' && T.head line /= '\t'
    isIndented line = not (T.null line) && (T.head line == ' ' || T.head line == '\t')

    mkGenericScope start end line = CodeScope
      { scopeKind = FunctionScope
      , scopeName = T.takeWhile (\c -> c /= '(' && c /= '{' && c /= ':' && c /= ' ') (T.strip line)
      , scopeParentModule = Nothing
      , scopeStartLine = start
      , scopeEndLine = end
      , scopeSignature = Nothing
      , scopeExported = True
      , scopeChildren = []
      }

-- ---------------------------------------------------------------------------
-- Chunk Construction Helpers
-- ---------------------------------------------------------------------------

-- | Convert a scope to one or more code chunks (splitting large scopes)
scopeToChunks :: ChunkerConfig -> FilePath -> Language -> Maybe Text -> Text -> CodeScope -> [CodeChunk]
scopeToChunks config fp lang moduleName content scope =
  let ls = T.lines content
      scopeLines = take (scopeEndLine scope - scopeStartLine scope + 1)
                        (drop (scopeStartLine scope - 1) ls)
      lineCount = length scopeLines
  in if lineCount <= ccMaxChunkLines config
     then [mkChunk fp lang moduleName scope (scopeStartLine scope) (scopeEndLine scope) (T.unlines scopeLines)]
     else splitLargeScope config fp lang moduleName scope scopeLines

-- | Split a scope that exceeds max chunk size
splitLargeScope :: ChunkerConfig -> FilePath -> Language -> Maybe Text -> CodeScope -> [Text] -> [CodeChunk]
splitLargeScope config fp lang moduleName scope scopeLines =
  let maxLines = ccMaxChunkLines config
      overlap = ccOverlapLines config
      startLine = scopeStartLine scope
      chunks = splitWithOverlap maxLines overlap scopeLines
  in zipWith (\(offset, lns) idx ->
       mkChunk fp lang moduleName
         (scope { scopeName = scopeName scope <> "_part" <> T.pack (show idx) })
         (startLine + offset)
         (startLine + offset + length lns - 1)
         (T.unlines lns)
     ) chunks [1 :: Int ..]

-- | Split a list of lines into overlapping windows
splitWithOverlap :: Int -> Int -> [Text] -> [(Int, [Text])]
splitWithOverlap _ _ [] = []
splitWithOverlap maxSize overlap ls =
  let chunk = take maxSize ls
      step = max 1 (maxSize - overlap)
      remaining = drop step ls
      restOffset = step
  in (0, chunk) : map (\(off, c) -> (off + restOffset, c)) (splitWithOverlap maxSize overlap remaining)

-- | Create a CodeChunk from scope information
mkChunk :: FilePath -> Language -> Maybe Text -> CodeScope -> Int -> Int -> Text -> CodeChunk
mkChunk fp lang moduleName scope start end content = CodeChunk
  { chunkFilePath = fp
  , chunkLanguage = lang
  , chunkModuleName = moduleName
  , chunkScopeName = Just (scopeName scope)
  , chunkScopeKind = Just (scopeKind scope)
  , chunkStartLine = start
  , chunkEndLine = end
  , chunkContent = content
  , chunkSignature = scopeSignature scope
  , chunkSymbolsDefined = [scopeName scope]
  , chunkSymbolsUsed = []
  , chunkContext = T.unwords
      [ maybe "" (\m -> "Module " <> m <> ".") moduleName
      , T.pack (show (scopeKind scope))
      , scopeName scope
      , "at"
      , T.pack fp <> ":" <> T.pack (show start) <> "-" <> T.pack (show end)
      ]
  }

-- | Create an import section chunk
mkImportChunk :: FilePath -> Maybe Text -> [Text] -> [CodeChunk]
mkImportChunk fp moduleName ls =
  let importLines = [(n, l) | (n, l) <- zip [1..] ls, "import " `T.isPrefixOf` T.strip l]
  in if null importLines then [] else
     let startLine = fst (head importLines)
         endLine = fst (last importLines)
         content = T.unlines (map snd importLines)
     in [CodeChunk
          { chunkFilePath = fp
          , chunkLanguage = Haskell
          , chunkModuleName = moduleName
          , chunkScopeName = Just "imports"
          , chunkScopeKind = Just ImportScope
          , chunkStartLine = startLine
          , chunkEndLine = endLine
          , chunkContent = content
          , chunkSignature = Nothing
          , chunkSymbolsDefined = []
          , chunkSymbolsUsed = [T.strip (T.takeWhile (/= '(') (T.drop 7 (T.strip l))) | (_, l) <- importLines]
          , chunkContext = "Import declarations for " <> maybe "unknown module" id moduleName
          }]

-- | Create chunks for lines not covered by any detected scope
mkUncoveredChunks :: ChunkerConfig -> FilePath -> Language -> Maybe Text -> [Text] -> [Int] -> [CodeChunk]
mkUncoveredChunks config fp lang moduleName ls coveredLines =
  let allLines = [1 .. length ls]
      uncovered = filter (`notElem` coveredLines) allLines
      groups = groupConsecutive uncovered
      goodGroups = filter (\g -> length g >= ccMinChunkLines config) groups
  in map (\g ->
       let start = head g
           end = last g
           content = T.unlines $ take (end - start + 1) (drop (start - 1) ls)
       in CodeChunk
            { chunkFilePath = fp
            , chunkLanguage = lang
            , chunkModuleName = moduleName
            , chunkScopeName = Nothing
            , chunkScopeKind = Nothing
            , chunkStartLine = start
            , chunkEndLine = end
            , chunkContent = content
            , chunkSignature = Nothing
            , chunkSymbolsDefined = []
            , chunkSymbolsUsed = []
            , chunkContext = "Uncovered code block at " <> T.pack fp
            }
     ) goodGroups

-- | Fallback: chunk by fixed-size line windows when no scopes are detected
fallbackLineChunks :: ChunkerConfig -> FilePath -> Language -> Text -> [CodeChunk]
fallbackLineChunks config fp lang content =
  let ls = T.lines content
      maxLines = ccMaxChunkLines config
      overlap = ccOverlapLines config
      windows = splitWithOverlap maxLines overlap ls
  in map (\(offset, lns) ->
       CodeChunk
         { chunkFilePath = fp
         , chunkLanguage = lang
         , chunkModuleName = Nothing
         , chunkScopeName = Nothing
         , chunkScopeKind = Nothing
         , chunkStartLine = offset + 1
         , chunkEndLine = offset + length lns
         , chunkContent = T.unlines lns
         , chunkSignature = Nothing
         , chunkSymbolsDefined = []
         , chunkSymbolsUsed = []
         , chunkContext = "Code block at " <> T.pack fp <> ":" <> T.pack (show (offset + 1))
         }
     ) windows

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract the Haskell module name from source lines
extractHaskellModuleName :: [Text] -> Maybe Text
extractHaskellModuleName ls =
  case filter ("module " `T.isPrefixOf`) (map T.strip ls) of
    (l : _) -> Just $ T.takeWhile (\c -> c /= ' ' && c /= '(') (T.drop 7 l)
    _ -> Nothing

-- | Group consecutive integers into runs
groupConsecutive :: [Int] -> [[Int]]
groupConsecutive [] = []
groupConsecutive (x:xs) = go [x] xs
  where
    go acc [] = [reverse acc]
    go acc (y:ys)
      | y == head acc + length acc = go (acc ++ [y]) ys
      | otherwise = reverse acc : go [y] ys

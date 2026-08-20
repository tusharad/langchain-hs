{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.RAG.CodeLoader
Description : Code-aware document loader for codebase indexing
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Code-aware document loader that recursively walks project directories, filters by
target extensions, and enriches loaded documents with metadata (module name,
language, file path). Integrates with 'ASTChunker' for scope-preserving splitting.
-}
module Aegis.RAG.CodeLoader
  ( -- * Loading
    loadCodebase
  , loadSingleFile
  , LoadedFile (..)

    -- * Configuration
  , CodeLoaderConfig (..)
  , defaultCodeLoaderConfig

    -- * Filtering
  , shouldIncludeFile
  , collectSourceFiles
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (filterM, forM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, getFileSize)
import System.FilePath (takeExtension, takeFileName, (</>))

import Aegis.Core.Types.AST
import Aegis.Core.Types.Config (RepositoryConfig (..))
import Aegis.RAG.ASTChunker

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A loaded source file with metadata and chunks
data LoadedFile = LoadedFile
  { lfFilePath :: FilePath
  -- ^ Relative path from repo root
  , lfLanguage :: Language
  -- ^ Detected language
  , lfModuleName :: Maybe Text
  -- ^ Extracted module name (if applicable)
  , lfRawContent :: Text
  -- ^ Full raw source content
  , lfLineCount :: Int
  -- ^ Total number of lines
  , lfChunks :: [CodeChunk]
  -- ^ AST-aware chunks
  , lfNode :: CodeNode
  -- ^ Parsed code node with structural information
  }
  deriving (Eq, Show)

-- | Configuration for the code loader
data CodeLoaderConfig = CodeLoaderConfig
  { clTargetExtensions :: [String]
  -- ^ File extensions to include
  , clExcludePaths :: [String]
  -- ^ Directory/file name patterns to exclude
  , clMaxFileSizeBytes :: Integer
  -- ^ Maximum file size to process
  , clRecursive :: Bool
  -- ^ Whether to recurse into subdirectories
  , clExcludeHidden :: Bool
  -- ^ Whether to exclude hidden files/directories
  , clChunkerConfig :: ChunkerConfig
  -- ^ AST chunker configuration
  }
  deriving (Eq, Show)

-- | Default configuration for Haskell projects
defaultCodeLoaderConfig :: CodeLoaderConfig
defaultCodeLoaderConfig = CodeLoaderConfig
  { clTargetExtensions = [".hs"]
  , clExcludePaths = [".git", ".stack-work", "dist-newstyle", "node_modules", ".cabal", ".hie"]
  , clMaxFileSizeBytes = 1048576
  , clRecursive = True
  , clExcludeHidden = True
  , clChunkerConfig = defaultChunkerConfig
  }

-- | Create loader config from repository config
fromRepoConfig :: RepositoryConfig -> CodeLoaderConfig
fromRepoConfig rc = CodeLoaderConfig
  { clTargetExtensions = map T.unpack (repoTargetExtensions rc)
  , clExcludePaths = map T.unpack (repoExcludePaths rc)
  , clMaxFileSizeBytes = fromIntegral (repoMaxFileSizeBytes rc)
  , clRecursive = repoRecursive rc
  , clExcludeHidden = True
  , clChunkerConfig = defaultChunkerConfig
  }

-- ---------------------------------------------------------------------------
-- Loading
-- ---------------------------------------------------------------------------

-- | Load an entire codebase from a directory
loadCodebase :: CodeLoaderConfig -> FilePath -> IO (Either Text [LoadedFile])
loadCodebase config repoPath = do
  exists <- doesDirectoryExist repoPath
  if not exists
    then pure $ Left $ "Repository path does not exist: " <> T.pack repoPath
    else do
      files <- collectSourceFiles config repoPath
      results <- forM files $ \fp -> do
        let relPath = drop (length repoPath + 1) fp
        loadSingleFile config relPath fp
      let (errs, loaded) = partitionEithers results
      if null loaded && not (null errs)
        then pure $ Left $ "Failed to load any files. First error: " <> head errs
        else pure $ Right loaded

-- | Load and parse a single source file
loadSingleFile :: CodeLoaderConfig -> FilePath -> FilePath -> IO (Either Text LoadedFile)
loadSingleFile config relPath fullPath = do
  eContent <- try $ TIO.readFile fullPath :: IO (Either SomeException Text)
  case eContent of
    Left err -> pure $ Left $ "Cannot read " <> T.pack relPath <> ": " <> T.pack (show err)
    Right content -> do
      let lang = detectLanguage relPath
          ls = T.lines content
          moduleName = extractModuleName lang ls
          chunks = chunkSourceFile (clChunkerConfig config) relPath content
          node = buildCodeNode relPath lang moduleName content
      pure $ Right LoadedFile
        { lfFilePath = relPath
        , lfLanguage = lang
        , lfModuleName = moduleName
        , lfRawContent = content
        , lfLineCount = length ls
        , lfChunks = chunks
        , lfNode = node
        }

-- ---------------------------------------------------------------------------
-- File Collection
-- ---------------------------------------------------------------------------

-- | Recursively collect all matching source files from a directory
collectSourceFiles :: CodeLoaderConfig -> FilePath -> IO [FilePath]
collectSourceFiles config dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  results <- forM fullPaths $ \fp -> do
    let baseName = takeFileName fp
    if isExcluded config baseName
      then pure []
      else do
        isDir <- doesDirectoryExist fp
        if isDir && clRecursive config
          then collectSourceFiles config fp
          else if not isDir
            then do
              include <- shouldIncludeFile config fp
              pure [fp | include]
            else pure []
  pure $ concat results

-- | Check whether a file should be included based on extension and size
shouldIncludeFile :: CodeLoaderConfig -> FilePath -> IO Bool
shouldIncludeFile config fp = do
  let ext = takeExtension fp
      extMatch = null (clTargetExtensions config) || ext `elem` clTargetExtensions config
      baseName = takeFileName fp
      notHidden = not (clExcludeHidden config) || not ("." `isPrefixOfString` baseName)
  if not (extMatch && notHidden)
    then pure False
    else do
      exists <- doesFileExist fp
      if not exists
        then pure False
        else do
          size <- getFileSize fp
          pure $ size <= clMaxFileSizeBytes config

-- | Check if a path component should be excluded
isExcluded :: CodeLoaderConfig -> String -> Bool
isExcluded config name =
  name `elem` clExcludePaths config
  || (clExcludeHidden config && "." `isPrefixOfString` name)

-- ---------------------------------------------------------------------------
-- Code Node Construction
-- ---------------------------------------------------------------------------

-- | Build a CodeNode from file content
buildCodeNode :: FilePath -> Language -> Maybe Text -> Text -> CodeNode
buildCodeNode fp lang moduleName content =
  let ls = T.lines content
      imports = extractImports lang ls
      exports = extractExports lang ls
      defined = extractDefinedSymbols lang ls
      used = extractUsedSymbols lang ls defined
      scopes = case lang of
        Haskell -> detectHaskellScopes ls
        _       -> detectGenericScopes ls
  in CodeNode
       { nodeFilePath = fp
       , nodeLanguage = lang
       , nodeModuleName = moduleName
       , nodeImports = imports
       , nodeExports = exports
       , nodeScopes = scopes
       , nodeSymbolsDefined = defined
       , nodeSymbolsUsed = used
       , nodeLineCount = length ls
       , nodeRawContent = content
       }

-- ---------------------------------------------------------------------------
-- Language-Specific Extraction
-- ---------------------------------------------------------------------------

-- | Extract module name based on language
extractModuleName :: Language -> [Text] -> Maybe Text
extractModuleName Haskell ls =
  case filter ("module " `T.isPrefixOf`) (map T.strip ls) of
    (l : _) -> Just $ T.takeWhile (\c -> c /= ' ' && c /= '(') (T.drop 7 l)
    _ -> Nothing
extractModuleName Python ls =
  Nothing -- Python doesn't have explicit module declarations in the same way
extractModuleName _ _ = Nothing

-- | Extract import statements
extractImports :: Language -> [Text] -> [Text]
extractImports Haskell ls =
  [ T.strip l | l <- ls
  , "import " `T.isPrefixOf` T.strip l
  ]
extractImports Python ls =
  [ T.strip l | l <- ls
  , "import " `T.isPrefixOf` T.strip l || "from " `T.isPrefixOf` T.strip l
  ]
extractImports _ ls =
  [ T.strip l | l <- ls
  , any (`T.isPrefixOf` T.strip l) ["import ", "require", "#include", "use "]
  ]

-- | Extract exported symbols
extractExports :: Language -> [Text] -> [Text]
extractExports Haskell ls =
  case dropWhile (not . ("module " `T.isPrefixOf`) . T.strip) ls of
    [] -> []
    (_ : rest) ->
      let exportBlock = takeWhile (\l -> not ("where" `T.isSuffixOf` T.strip l) && not (T.null (T.strip l))) rest
          items = concatMap (T.splitOn ",") exportBlock
      in map T.strip $ filter (not . T.null . T.strip) items
extractExports _ _ = []

-- | Extract top-level defined symbols
extractDefinedSymbols :: Language -> [Text] -> Set Text
extractDefinedSymbols Haskell ls =
  let topLevel = [(n, l) | (n, l) <- zip [(1::Int)..] ls
                          , not (T.null l)
                          , T.head l /= ' '
                          , T.head l /= '\t'
                          , not ("--" `T.isPrefixOf` T.strip l)
                          , not ("{-" `T.isPrefixOf` T.strip l)
                          , not ("module " `T.isPrefixOf` T.strip l)
                          , not ("import " `T.isPrefixOf` T.strip l)
                          ]
      names = [T.takeWhile (\c -> c /= ' ' && c /= ':' && c /= '=') (T.strip l) | (_, l) <- topLevel]
  in Set.fromList $ filter (not . T.null) names
extractDefinedSymbols _ ls =
  let topLevel = [l | l <- ls, not (T.null l), T.head l /= ' ', T.head l /= '\t']
      names = [T.takeWhile (\c -> c /= '(' && c /= '{' && c /= ':' && c /= ' ') (T.strip l) | l <- topLevel]
  in Set.fromList $ filter (not . T.null) names

-- | Extract used symbols (simple heuristic)
extractUsedSymbols :: Language -> [Text] -> Set Text -> Set Text
extractUsedSymbols _ ls defined =
  let allWords = Set.fromList $ concatMap T.words ls
  in Set.difference allWords defined

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isPrefixOfString :: String -> String -> Bool
isPrefixOfString [] _ = True
isPrefixOfString _ [] = False
isPrefixOfString (x:xs) (y:ys) = x == y && isPrefixOfString xs ys

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr go ([], [])
  where
    go (Left a) (ls, rs) = (a : ls, rs)
    go (Right b) (ls, rs) = (ls, b : rs)

import Data.Set (Set)
import qualified Data.Set as Set

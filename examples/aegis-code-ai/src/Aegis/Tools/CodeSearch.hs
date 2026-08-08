{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Tools.CodeSearch
Description : Code search and file reading tools for the analysis pipeline
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Tools for searching code patterns, looking up symbol definitions, and reading
file contents with line range support. These tools form the foundation for
context retrieval used by all agents.
-}
module Aegis.Tools.CodeSearch
  ( -- * Tools
    grepTool
  , symbolLookupTool
  , fileReadTool
  , fileListTool
  , fileWriteTool

    -- * Search Helpers
  , SearchResult (..)
  , grepInDirectory
  , findSymbolDefinitions
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (filterM, forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (isPrefixOf, isSuffixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Langchain.Core.Error (LangchainError, toolError)
import Langchain.Core.Tool (Tool (..), createTool)

-- ---------------------------------------------------------------------------
-- Search Result Types
-- ---------------------------------------------------------------------------

-- | A single search result with file, line, and content
data SearchResult = SearchResult
  { srFile :: FilePath
  , srLine :: Int
  , srContent :: Text
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Grep Tool
-- ---------------------------------------------------------------------------

-- | Tool that searches for patterns across the codebase
grepTool :: MonadIO m => FilePath -> Tool m
grepTool repoPath = createTool
  "grep_search"
  "Search for a text pattern across the codebase. Returns matching lines with file paths \
  \and line numbers. Arguments: {\"pattern\": \"string\", \"file_extension\": \"string (optional, e.g. '.hs')\", \
  \\"max_results\": \"int (default 50)\", \"case_sensitive\": \"bool (default true)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["pattern"] :: [Text])
    , "properties" .= object
        [ "pattern" .= object ["type" .= ("string" :: Text)]
        , "file_extension" .= object ["type" .= ("string" :: Text)]
        , "max_results" .= object ["type" .= ("integer" :: Text)]
        , "case_sensitive" .= object ["type" .= ("boolean" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let pattern_ = extractT "pattern" args
        ext = extractT "file_extension" args
        maxRes = extractI "max_results" args 50
        caseSen = extractBD "case_sensitive" args True
    if T.null pattern_
      then pure $ Left $ toolError "pattern is required" (Just "grep_search") Nothing
      else do
        results <- grepInDirectory repoPath pattern_ (if T.null ext then Nothing else Just (T.unpack ext)) caseSen maxRes
        let formatted = formatResults results
        pure $ Right $ "Found " <> T.pack (show (length results)) <> " match(es):\n\n" <> formatted
  )

-- | Search for a pattern in all files within a directory
grepInDirectory :: FilePath -> Text -> Maybe String -> Bool -> Int -> IO [SearchResult]
grepInDirectory dir pattern_ mbExt caseSensitive maxResults = do
  files <- findFilesRecursive dir mbExt
  results <- concat <$> mapM (searchFile pattern_ caseSensitive) files
  pure $ take maxResults results

-- | Recursively find files with optional extension filter
findFilesRecursive :: FilePath -> Maybe String -> IO [FilePath]
findFilesRecursive dir mbExt = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
        isExcluded fp = any (`isPrefixOf` fp) [".git", ".stack-work", "dist-newstyle", "node_modules"]
    concat <$> forM fullPaths (\fp -> do
      let base = last (words (map (\c -> if c == '/' then ' ' else c) fp))
      if isExcluded base
        then pure []
        else do
          isDir <- doesDirectoryExist fp
          if isDir
            then findFilesRecursive fp mbExt
            else case mbExt of
              Nothing -> pure [fp]
              Just ext -> pure [fp | takeExtension fp == ext])

-- | Search a single file for a pattern
searchFile :: Text -> Bool -> FilePath -> IO [SearchResult]
searchFile pattern_ caseSensitive fp = do
  eContent <- try $ TIO.readFile fp :: IO (Either SomeException Text)
  case eContent of
    Left _ -> pure []
    Right content ->
      let ls = zip [1 ..] (T.lines content)
          matchFn = if caseSensitive
                    then T.isInfixOf pattern_
                    else T.isInfixOf (T.toLower pattern_) . T.toLower
          matches = [(n, l) | (n, l) <- ls, matchFn l]
      in pure [SearchResult fp n l | (n, l) <- matches]

-- | Format search results for display
formatResults :: [SearchResult] -> Text
formatResults = T.unlines . map formatOne
  where
    formatOne sr =
      T.pack (srFile sr) <> ":" <> T.pack (show (srLine sr)) <> ": " <> T.strip (srContent sr)

-- ---------------------------------------------------------------------------
-- Symbol Lookup Tool
-- ---------------------------------------------------------------------------

-- | Tool that finds definition and usage sites for a symbol
symbolLookupTool :: MonadIO m => FilePath -> Tool m
symbolLookupTool repoPath = createTool
  "symbol_lookup"
  "Find the definition and usage sites of a symbol (function, type, class) across \
  \the codebase. Arguments: {\"symbol\": \"string\", \"file_extension\": \"string (default '.hs')\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["symbol"] :: [Text])
    , "properties" .= object
        [ "symbol" .= object ["type" .= ("string" :: Text)]
        , "file_extension" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let symbol = extractT "symbol" args
        ext = let e = extractT "file_extension" args in if T.null e then ".hs" else T.unpack e
    if T.null symbol
      then pure $ Left $ toolError "symbol is required" (Just "symbol_lookup") Nothing
      else do
        (definitions, usages) <- findSymbolDefinitions repoPath symbol ext
        let report = T.unlines $
              ["=== Symbol Lookup: " <> symbol, ""]
              ++ ["--- Definitions ---"]
              ++ if null definitions
                 then ["  (no definitions found)"]
                 else map formatOne definitions
              ++ ["", "--- Usages ---"]
              ++ if null usages
                 then ["  (no usages found)"]
                 else map formatOne (take 30 usages)
              ++ if length usages > 30
                 then ["  ... and " <> T.pack (show (length usages - 30)) <> " more"]
                 else []
        pure $ Right report
  )
  where
    formatOne sr =
      "  " <> T.pack (srFile sr) <> ":" <> T.pack (show (srLine sr)) <> ": " <> T.strip (srContent sr)

-- | Find symbol definitions and usages across the codebase
findSymbolDefinitions :: FilePath -> Text -> String -> IO ([SearchResult], [SearchResult])
findSymbolDefinitions dir symbol ext = do
  files <- findFilesRecursive dir (Just ext)
  allResults <- concat <$> mapM (searchFile symbol True) files
  let definitions = filter isDefinition allResults
      usages = filter (not . isDefinition) allResults
  pure (definitions, usages)
  where
    isDefinition sr =
      let line = T.strip (srContent sr)
      in -- Haskell definitions: starts with the symbol at column 0
         (symbol <> " " ) `T.isPrefixOf` line
         || (symbol <> " ::") `T.isPrefixOf` line
         || ("data " <> symbol) `T.isInfixOf` line
         || ("type " <> symbol) `T.isInfixOf` line
         || ("class " <> symbol) `T.isInfixOf` line
         || ("newtype " <> symbol) `T.isInfixOf` line

-- ---------------------------------------------------------------------------
-- File Read Tool
-- ---------------------------------------------------------------------------

-- | Tool that reads file contents with optional line range
fileReadTool :: MonadIO m => FilePath -> Tool m
fileReadTool repoPath = createTool
  "read_file"
  "Read the contents of a file, optionally within a specific line range. \
  \Arguments: {\"file\": \"string\", \"start_line\": \"int (optional)\", \"end_line\": \"int (optional)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["file"] :: [Text])
    , "properties" .= object
        [ "file" .= object ["type" .= ("string" :: Text)]
        , "start_line" .= object ["type" .= ("integer" :: Text)]
        , "end_line" .= object ["type" .= ("integer" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let file = extractT "file" args
        startLine = extractI "start_line" args 1
        endLine = extractI "end_line" args (maxBound :: Int)
    if T.null file
      then pure $ Left $ toolError "file is required" (Just "read_file") Nothing
      else do
        let fullPath = repoPath </> T.unpack file
        eContent <- try $ TIO.readFile fullPath :: IO (Either SomeException Text)
        case eContent of
          Left err -> pure $ Left $ toolError
            ("Cannot read file '" <> file <> "': " <> T.pack (show err)) (Just "read_file") Nothing
          Right content -> do
            let ls = T.lines content
                totalLines = length ls
                selectedLines = take (endLine - startLine + 1) (drop (startLine - 1) ls)
                numbered = zipWith
                  (\n l -> T.pack (show n) <> ": " <> l)
                  [startLine ..]
                  selectedLines
            pure $ Right $ T.unlines $
              ["File: " <> file <> " (" <> T.pack (show totalLines) <> " lines total)", ""]
              ++ numbered
  )

-- ---------------------------------------------------------------------------
-- File List Tool
-- ---------------------------------------------------------------------------

-- | Tool that lists files in a directory with optional filtering
fileListTool :: MonadIO m => FilePath -> Tool m
fileListTool repoPath = createTool
  "list_files"
  "List files in a directory with optional extension filtering. \
  \Arguments: {\"directory\": \"string (default '.')\", \
  \\"extension\": \"string (optional, e.g. '.hs')\", \
  \\"recursive\": \"bool (default true)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "directory" .= object ["type" .= ("string" :: Text)]
        , "extension" .= object ["type" .= ("string" :: Text)]
        , "recursive" .= object ["type" .= ("boolean" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let dir = let d = extractT "directory" args in if T.null d then "." else T.unpack d
        ext = let e = extractT "extension" args in if T.null e then Nothing else Just (T.unpack e)
        recursive = extractBD "recursive" args True
        fullDir = repoPath </> dir
    files <- if recursive
             then findFilesRecursive fullDir ext
             else do
               entries <- listDirectory fullDir
               let fullPaths = map (fullDir </>) entries
               filterM doesFileExist $ case ext of
                 Nothing -> fullPaths
                 Just e -> filter (\fp -> takeExtension fp == e) fullPaths
    let formatted = T.unlines $ map (T.pack . drop (length repoPath + 1)) files
    pure $ Right $ T.pack (show (length files)) <> " file(s):\n" <> formatted
  )

-- ---------------------------------------------------------------------------
-- File Write Tool
-- ---------------------------------------------------------------------------

-- | Tool that writes content to a file
fileWriteTool :: MonadIO m => FilePath -> Tool m
fileWriteTool repoPath = createTool
  "write_file"
  "Write content to a file, creating it if it doesn't exist. \
  \Arguments: {\"file\": \"string\", \"content\": \"string\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["file", "content"] :: [Text])
    , "properties" .= object
        [ "file" .= object ["type" .= ("string" :: Text)]
        , "content" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let file = extractT "file" args
        content = extractT "content" args
    if T.null file
      then pure $ Left $ toolError "file is required" (Just "write_file") Nothing
      else do
        let fullPath = repoPath </> T.unpack file
        eRes <- try $ TIO.writeFile fullPath content :: IO (Either SomeException ())
        case eRes of
          Left err -> pure $ Left $ toolError
            ("Cannot write file: " <> T.pack (show err)) (Just "write_file") Nothing
          Right () -> pure $ Right $ "Wrote " <> T.pack (show (T.length content)) <> " chars to " <> file
  )

-- ---------------------------------------------------------------------------
-- JSON Helpers
-- ---------------------------------------------------------------------------

extractT :: Text -> Value -> Text
extractT key (Object obj) = case KM.lookup (fromString (T.unpack key)) obj of
  Just (String t) -> t
  _ -> ""
extractT _ _ = ""

extractI :: Text -> Value -> Int -> Int
extractI key (Object obj) def = case KM.lookup (fromString (T.unpack key)) obj of
  Just (Number n) -> round n
  _ -> def
extractI _ _ def = def

extractBD :: Text -> Value -> Bool -> Bool
extractBD key (Object obj) def = case KM.lookup (fromString (T.unpack key)) obj of
  Just (Bool b) -> b
  _ -> def
extractBD _ _ def = def

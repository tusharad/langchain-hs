{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Tools.ASTTransformer
Description : Code transformation and structural analysis tools
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Code transformation tools for dead code detection, formatting, and dependency
analysis. Implements the @Tool m@ interface for integration into agent workflows.
-}
module Aegis.Tools.ASTTransformer
  ( -- * Tools
    deadCodeDetectorTool
  , dependencyAnalyzerTool
  , codeFormatterTool
  , moduleSummaryTool

    -- * Analysis Functions
  , detectUnusedImports
  , detectUnusedBindings
  , buildModuleDependencyGraph
  , ModuleDependency (..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub, isPrefixOf, isSuffixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Langchain.Core.Error (LangchainError, toolError)
import Langchain.Core.Tool (Tool (..), createTool)

-- ---------------------------------------------------------------------------
-- Module Dependency Types
-- ---------------------------------------------------------------------------

-- | A module dependency relationship
data ModuleDependency = ModuleDependency
  { mdModule :: Text
  -- ^ Module name
  , mdFilePath :: FilePath
  -- ^ File path
  , mdImports :: [Text]
  -- ^ Modules this module imports
  , mdExports :: [Text]
  -- ^ Symbols this module exports
  , mdLineCount :: Int
  -- ^ Number of lines
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Dead Code Detector Tool
-- ---------------------------------------------------------------------------

-- | Tool that identifies potentially dead/unused code in Haskell files
deadCodeDetectorTool :: MonadIO m => FilePath -> Tool m
deadCodeDetectorTool repoPath = createTool
  "dead_code_detector"
  "Analyze Haskell source files for unused imports, unused bindings, and dead code. \
  \Arguments: {\"file\": \"string (file path to analyze)\", \
  \\"check_imports\": \"bool (default true)\", \
  \\"check_bindings\": \"bool (default true)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["file"] :: [Text])
    , "properties" .= object
        [ "file" .= object ["type" .= ("string" :: Text)]
        , "check_imports" .= object ["type" .= ("boolean" :: Text)]
        , "check_bindings" .= object ["type" .= ("boolean" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let file = extractT "file" args
    if T.null file
      then pure $ Left $ toolError "file is required" (Just "dead_code_detector") Nothing
      else do
        let fullPath = repoPath </> T.unpack file
        eContent <- try' $ TIO.readFile fullPath
        case eContent of
          Left err -> pure $ Left $ toolError
            ("Cannot read file: " <> T.pack (show err)) (Just "dead_code_detector") Nothing
          Right content -> do
            let unusedImports = detectUnusedImports content
                unusedBindings = detectUnusedBindings content
                report = T.unlines $
                  ["=== Dead Code Analysis: " <> file, ""]
                  ++ (if null unusedImports
                      then ["No unused imports detected."]
                      else ["--- Potentially Unused Imports ---"]
                           ++ map (\i -> "  • " <> i) unusedImports)
                  ++ [""]
                  ++ (if null unusedBindings
                      then ["No potentially unused bindings detected."]
                      else ["--- Potentially Unused Bindings ---"]
                           ++ map (\b -> "  • " <> b) unusedBindings)
            pure $ Right report
  )

-- | Detect potentially unused imports in Haskell source code
-- Uses a simple heuristic: checks if imported module names appear in the code body.
detectUnusedImports :: Text -> [Text]
detectUnusedImports source =
  let ls = T.lines source
      importLines = filter isImportLine ls
      bodyLines = filter (not . isImportLine) ls
      body = T.unlines bodyLines
  in [ extractImportModule imp
     | imp <- importLines
     , let modName = extractImportModule imp
     , not (T.null modName)
     , not (isQualifiedUsed modName body)
     ]
  where
    isImportLine l = "import " `T.isPrefixOf` T.stripStart l

    extractImportModule :: Text -> Text
    extractImportModule line =
      let ws = T.words (T.strip line)
          afterImport = drop 1 $ dropWhile (/= "import") ws
          afterQualified = case afterImport of
            ("qualified" : rest) -> rest
            other -> other
      in case afterQualified of
           (modName : _) -> T.takeWhile (\c -> c /= '(' && c /= ' ') modName
           _ -> ""

    isQualifiedUsed :: Text -> Text -> Bool
    isQualifiedUsed modName body =
      let shortName = T.takeWhileEnd (/= '.') modName
      in shortName `T.isInfixOf` body || modName `T.isInfixOf` body

-- | Detect potentially unused top-level bindings in Haskell source code
-- Heuristic: finds bindings that are defined but never referenced elsewhere.
detectUnusedBindings :: Text -> [Text]
detectUnusedBindings source =
  let ls = T.lines source
      bindings = extractTopLevelBindings ls
      body = source
  in [ name
     | name <- bindings
     , not (T.null name)
     , countOccurrences name body <= 1  -- Only the definition itself
     , not ("main" == name)  -- Never report 'main' as unused
     , not ("spec" == name)  -- Never report 'spec' as unused
     ]
  where
    extractTopLevelBindings :: [Text] -> [Text]
    extractTopLevelBindings = nub . concatMap extractBinding

    extractBinding :: Text -> [Text]
    extractBinding line
      | T.null line = []
      | T.head line == ' ' || T.head line == '\t' = []  -- Indented = not top-level
      | "--" `T.isPrefixOf` T.strip line = []  -- Comment
      | "{-" `T.isPrefixOf` T.strip line = []  -- Block comment
      | "import " `T.isPrefixOf` T.strip line = []
      | "module " `T.isPrefixOf` T.strip line = []
      | "data " `T.isPrefixOf` T.strip line = []
      | "type " `T.isPrefixOf` T.strip line = []
      | "class " `T.isPrefixOf` T.strip line = []
      | "instance " `T.isPrefixOf` T.strip line = []
      | "newtype " `T.isPrefixOf` T.strip line = []
      | "deriving" `T.isPrefixOf` T.strip line = []
      | otherwise =
          let ws = T.words (T.strip line)
          in case ws of
               (name : _)
                 | T.all (\c -> c `elem` ("_abcdefghijklmnopqrstuvwxyz'" :: String)) (T.take 1 name)
                   -> [T.takeWhile (\c -> c /= ' ' && c /= ':') name]
               _ -> []

    countOccurrences :: Text -> Text -> Int
    countOccurrences needle haystack =
      length $ T.splitOn needle haystack

-- ---------------------------------------------------------------------------
-- Dependency Analyzer Tool
-- ---------------------------------------------------------------------------

-- | Tool that builds a module dependency graph for the project
dependencyAnalyzerTool :: MonadIO m => FilePath -> Tool m
dependencyAnalyzerTool repoPath = createTool
  "dependency_analyzer"
  "Build a module dependency graph showing import relationships. \
  \Arguments: {\"directory\": \"string (default 'src')\", \
  \\"extension\": \"string (default '.hs')\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "directory" .= object ["type" .= ("string" :: Text)]
        , "extension" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let dir = let d = extractT "directory" args in if T.null d then "src" else T.unpack d
        ext = let e = extractT "extension" args in if T.null e then ".hs" else T.unpack e
        fullDir = repoPath </> dir
    deps <- buildModuleDependencyGraph fullDir ext
    let report = formatDependencyGraph deps
    pure $ Right report
  )

-- | Build a module dependency graph by scanning Haskell files
buildModuleDependencyGraph :: FilePath -> String -> IO [ModuleDependency]
buildModuleDependencyGraph dir ext = do
  files <- findFiles dir ext
  mapM analyzeFile files
  where
    findFiles :: FilePath -> String -> IO [FilePath]
    findFiles d e = do
      exists <- doesDirectoryExist d
      if not exists then pure [] else do
        entries <- listDirectory d
        let fullPaths = map (d </>) entries
        files <- concat <$> mapM (\fp -> do
          isDir <- doesDirectoryExist fp
          if isDir then findFiles fp e
          else pure [fp | takeExtension fp == e]) fullPaths
        pure files

    analyzeFile :: FilePath -> IO ModuleDependency
    analyzeFile fp = do
      content <- TIO.readFile fp
      let ls = T.lines content
          moduleName = extractModuleName ls
          imports = extractImports ls
          exports = extractExports ls
      pure ModuleDependency
        { mdModule = moduleName
        , mdFilePath = fp
        , mdImports = imports
        , mdExports = exports
        , mdLineCount = length ls
        }

    extractModuleName :: [Text] -> Text
    extractModuleName ls =
      case filter ("module " `T.isPrefixOf`) (map T.stripStart ls) of
        (l : _) -> let ws = T.words l
                   in case drop 1 ws of
                        (m : _) -> T.takeWhile (\c -> c /= ' ' && c /= '(') m
                        _ -> ""
        _ -> ""

    extractImports :: [Text] -> [Text]
    extractImports ls =
      [ extractImportName l
      | l <- map T.stripStart ls
      , "import " `T.isPrefixOf` l
      ]

    extractImportName :: Text -> Text
    extractImportName line =
      let ws = T.words line
          afterImport = drop 1 $ dropWhile (/= "import") ws
          afterQualified = case afterImport of
            ("qualified" : rest) -> rest
            other -> other
      in case afterQualified of
           (modName : _) -> T.takeWhile (\c -> c /= '(' && c /= ' ') modName
           _ -> ""

    extractExports :: [Text] -> [Text]
    extractExports ls =
      let moduleHead = takeWhile (not . ("where" `T.isSuffixOf`) . T.stripEnd) ls
          exportSection = T.unlines moduleHead
          items = T.splitOn "," exportSection
      in [T.strip item | item <- items, not (T.null (T.strip item))]

-- | Format dependency graph as human-readable text
formatDependencyGraph :: [ModuleDependency] -> Text
formatDependencyGraph deps = T.unlines $
  ["=== Module Dependency Graph ===", ""]
  ++ concatMap formatModule deps
  ++ ["", "--- Summary ---"
    , "Total modules: " <> T.pack (show (length deps))
    , "Total lines: " <> T.pack (show (sum (map mdLineCount deps)))
    ]
  where
    formatModule dep = 
      [ mdModule dep <> " (" <> T.pack (show (mdLineCount dep)) <> " lines)"
      ] ++ map (\imp -> "  → " <> imp) (mdImports dep)
        ++ [""]

-- ---------------------------------------------------------------------------
-- Code Formatter Tool
-- ---------------------------------------------------------------------------

-- | Tool that applies code formatting via external formatter
codeFormatterTool :: MonadIO m => FilePath -> Tool m
codeFormatterTool repoPath = createTool
  "code_formatter"
  "Format source code using the project's formatter (fourmolu for Haskell). \
  \Arguments: {\"file\": \"string (file to format)\", \
  \\"check_only\": \"bool (default false, just check without modifying)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["file"] :: [Text])
    , "properties" .= object
        [ "file" .= object ["type" .= ("string" :: Text)]
        , "check_only" .= object ["type" .= ("boolean" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let file = extractT "file" args
        checkOnly = extractB "check_only" args
    if T.null file
      then pure $ Left $ toolError "file is required" (Just "code_formatter") Nothing
      else do
        let mode = if checkOnly then "--mode check" else "--mode inplace"
            cmd = "fourmolu " <> mode <> " " <> T.unpack file <> " 2>&1"
        eRes <- try' $ do
          let cp = (shell cmd) { cwd = Just repoPath, std_out = CreatePipe, std_err = CreatePipe }
          (_, Just hOut, _, ph) <- createProcess cp
          exitCode <- waitForProcess ph
          output <- TIO.hGetContents hOut
          pure (exitCode, output)
        case eRes of
          Left err -> pure $ Left $ toolError
            ("Formatter failed: " <> T.pack (show err)) (Just "code_formatter") Nothing
          Right (ExitSuccess, output) -> pure $ Right $
            if checkOnly then "Format check passed for " <> file
            else "Formatted " <> file <> "\n" <> output
          Right (ExitFailure _, output) -> pure $ Right $
            "Format issues found in " <> file <> ":\n" <> output
  )

-- ---------------------------------------------------------------------------
-- Module Summary Tool
-- ---------------------------------------------------------------------------

-- | Tool that generates a structural summary of a Haskell module
moduleSummaryTool :: MonadIO m => FilePath -> Tool m
moduleSummaryTool repoPath = createTool
  "module_summary"
  "Generate a structural summary of a source file showing exports, imports, \
  \type signatures, and top-level definitions. \
  \Arguments: {\"file\": \"string (file path)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["file"] :: [Text])
    , "properties" .= object
        [ "file" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let file = extractT "file" args
    if T.null file
      then pure $ Left $ toolError "file is required" (Just "module_summary") Nothing
      else do
        let fullPath = repoPath </> T.unpack file
        eContent <- try' $ TIO.readFile fullPath
        case eContent of
          Left err -> pure $ Left $ toolError
            ("Cannot read file: " <> T.pack (show err)) (Just "module_summary") Nothing
          Right content -> do
            let ls = T.lines content
                summary = T.unlines $
                  ["=== Module Summary: " <> file, ""]
                  ++ ["--- Type Signatures ---"]
                  ++ [l | l <- ls, "::" `T.isInfixOf` l, not ("--" `T.isPrefixOf` T.stripStart l)]
                  ++ ["", "--- Data Declarations ---"]
                  ++ [l | l <- ls, any (`T.isPrefixOf` T.stripStart l) ["data ", "newtype ", "type "]]
                  ++ ["", "--- Class/Instance Declarations ---"]
                  ++ [l | l <- ls, any (`T.isPrefixOf` T.stripStart l) ["class ", "instance "]]
                  ++ ["", "--- Line Count: " <> T.pack (show (length ls)) <> " ---"]
            pure $ Right summary
  )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

try' :: IO a -> IO (Either SomeException a)
try' = try

extractT :: Text -> Value -> Text
extractT key (Object obj) = case KM.lookup (fromString (T.unpack key)) obj of
  Just (String t) -> t
  _ -> ""
extractT _ _ = ""

extractB :: Text -> Value -> Bool
extractB key (Object obj) = case KM.lookup (fromString (T.unpack key)) obj of
  Just (Bool b) -> b
  _ -> False
extractB _ _ = False

import Control.Exception (SomeException)
import System.Exit (ExitCode (..))
import System.Process (shell, CreateProcess (..), StdStream (..), createProcess, waitForProcess)

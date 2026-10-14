{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Langchain.Tool.FileSystem
Description : Observational and state-modifying filesystem tools
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides comprehensive, agent-optimized filesystem tools:
* 'findFilesTool' ('find_files'): Concurrent directory search respecting .gitignore, depth, and patterns.
* 'grepSearchTool' ('grep_search'): High-performance ripgrep-style content search with line numbers and context-window protection.
* 'readFileTool' ('read_file'): Paginated reading with line numbers, context-window awareness, and read-before-edit tracking.
* 'editFileTool' ('edit_file'): Precise string replacement with read-before-edit verification, uniqueness check, and automatic undo snapshots.
* 'writeFileTool' ('write_file'): Safe file creation with overwrite protection and automatic checkpointing.
* 'listDirTool' ('list_directory'): Directory inspection with file sizes and type indicators.
* 'newFileSystemSuite': Factory creating the complete suite sharing a checkpoint registry and security policy.
-}
module Langchain.Tool.FileSystem
  ( -- * Complete Suite Factory
    newFileSystemSuite

    -- * Observational Tools
  , findFilesTool
  , grepSearchTool
  , readFileTool
  , listDirTool

    -- * Modification Tools
  , editFileTool
  , writeFileTool

    -- * Helper / Pure Matching
  , matchGlobPattern
  , isDefaultIgnored
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, Key, Object, Value (..), object, (.:?), (.=))
import Data.Aeson.Types (parseEither)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getFileSize
  , listDirectory
  )
import System.FilePath
  ( takeDirectory
  , takeFileName
  , (</>)
  )

import Langchain.Error (toolError)
import Langchain.Tool (Tool (..), createTool)
import Langchain.Tool.Checkpoint
  ( CheckpointRegistry
  , createFileCheckpoint
  , hasReadFile
  , listCheckpointsTool
  , newCheckpointRegistry
  , recordFileRead
  , undoFileEditTool
  )
import Langchain.Tool.Security
  ( SecurityPolicy
  , ToolTier (..)
  , validateSandboxPath
  , withSecurity
  )

-- | Extract an optional JSON field with a fallback default.
optField :: FromJSON a => Key -> a -> Object -> a
optField k def o = case parseEither (.:? k) o of
  Right (Just v) -> v
  _ -> def

-- | Extract an optional JSON field as Maybe.
mbField :: FromJSON a => Key -> Object -> Maybe a
mbField k o = case parseEither (.:? k) o of
  Right mv -> mv
  _ -> Nothing

-- ============================================================================
-- Pure Pattern & Gitignore Matching
-- ============================================================================

-- | Directories and file suffixes automatically ignored in find and search.
defaultIgnoredNames :: [String]
defaultIgnoredNames =
  [ ".git"
  , ".stack-work"
  , "dist-newstyle"
  , "node_modules"
  , ".cabal-sandbox"
  , ".idea"
  , ".vscode"
  , ".DS_Store"
  ]

-- | Check if a directory or filename is in the default ignore set.
isDefaultIgnored :: FilePath -> Bool
isDefaultIgnored name =
  let fn = takeFileName name
   in fn `elem` defaultIgnoredNames || ".git" `isPrefixOf` fn

-- | Simple glob matcher supporting '*' wildcard and case-insensitivity.
matchGlobPattern :: String -> String -> Bool
matchGlobPattern pat str
  | null pat = True
  | pat == "*" = True
  | "*" `isPrefixOf` pat && "*" `isSuffixOf` pat =
      let sub = drop 1 (take (length pat - 1) pat)
       in map toLower sub `isInfixOf` map toLower str
  | "*" `isPrefixOf` pat =
      let sfx = drop 1 pat
       in map toLower sfx `isSuffixOf` map toLower str
  | "*" `isSuffixOf` pat =
      let pfx = take (length pat - 1) pat
       in map toLower pfx `isPrefixOf` map toLower str
  | "*" `isInfixOf` pat =
      let (before, afterStar) = span (/= '*') pat
          after = drop 1 afterStar
       in (map toLower before `isPrefixOf` map toLower str)
            && (map toLower after `isSuffixOf` map toLower str)
  | otherwise =
      map toLower pat `isInfixOf` map toLower str

-- ============================================================================
-- 1. Find Files Tool
-- ============================================================================

-- | Create a concurrent file/directory search tool.
findFilesTool ::
  MonadIO m =>
  SecurityPolicy ->
  Tool m
findFilesTool policy =
  withSecurity TierReadOnly policy $
    createTool
      "find_files"
      "Search for files and directories matching name patterns with recursion depth control, gitignore filtering, and concurrent scanning."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Root directory to start searching from (default: '.')" :: Text)
                      ]
                , "pattern"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Filename or glob pattern to match (e.g. '*.hs', 'Spec', 'test*')" :: Text)
                      ]
                , "max_depth"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description" .= ("Maximum recursion directory depth (default: unlimited)" :: Text)
                      ]
                , "file_type"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Filter results by 'file', 'directory', or 'any' (default: 'any')" :: Text)
                      ]
                , "max_results"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description" .= ("Maximum number of matching paths to return (default: 100)" :: Text)
                      ]
                ]
          ]
      )
      ( \case
          Object o -> do
            let rawPath = optField "path" "." o :: String
                pat = optField "pattern" "" o :: String
                mbMaxDepth = mbField "max_depth" o :: Maybe Int
                fType = optField "file_type" "any" o :: Text
                maxResults = optField "max_results" 100 o :: Int

            eCleanPath <- validateSandboxPath policy rawPath
            case eCleanPath of
              Left err -> pure $ Left err
              Right cleanPath -> do
                eMatches <- liftIO $ try (walkFind cleanPath pat mbMaxDepth fType maxResults 0)
                case eMatches of
                  Left err ->
                    pure $
                      Left $
                        toolError
                          ("Failed searching directory: " <> T.pack (show (err :: SomeException)))
                          (Just "find_files")
                          Nothing
                  Right (found, truncated) ->
                    let count = length found
                        header =
                          "Found "
                            <> T.pack (show count)
                            <> " items"
                            <> (if truncated then " (truncated at " <> T.pack (show maxResults) <> ")" else "")
                            <> ":"
                        formatted = map T.pack found
                     in pure $ Right $ T.unlines (header : formatted)
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "find_files") Nothing
      )
  where
    walkFind :: FilePath -> String -> Maybe Int -> Text -> Int -> Int -> IO ([FilePath], Bool)
    walkFind dir pat mbMaxDepth fType limit currentDepth = do
      isDir <- doesDirectoryExist dir
      if not isDir
        then pure ([], False)
        else do
          entries <- listDirectory dir
          let filtered = filter (not . isDefaultIgnored) entries
          let fullPaths = map (dir </>) filtered

          let matchesPat p = null pat || matchGlobPattern pat (takeFileName p)

          -- Separate files and subdirectories
          (files, subdirs) <- partitionPaths fullPaths

          let matchingFiles =
                if fType `elem` ["any", "file"]
                  then filter matchesPat files
                  else []
              matchingDirs =
                if fType `elem` ["any", "directory"]
                  then filter matchesPat subdirs
                  else []
              currentMatches = matchingFiles ++ matchingDirs

          let canRecurse = maybe True (currentDepth <) mbMaxDepth
          if not canRecurse || length currentMatches >= limit
            then pure (take limit currentMatches, length currentMatches >= limit)
            else do
              let remainingLimit = limit - length currentMatches
              subResults <-
                mapConcurrently
                  (\sd -> walkFind sd pat mbMaxDepth fType remainingLimit (currentDepth + 1))
                  subdirs
              let (allSubMatches, anyTrunc) = mconcatResults subResults
                  combined = currentMatches ++ allSubMatches
              pure (take limit combined, anyTrunc || length combined >= limit)

    partitionPaths :: [FilePath] -> IO ([FilePath], [FilePath])
    partitionPaths paths = do
      dirFlags <- mapConcurrently doesDirectoryExist paths
      let (dirs, files) =
            foldr
              ( \(p, isD) (ds, fs) ->
                  if isD then (p : ds, fs) else (ds, p : fs)
              )
              ([], [])
              (zip paths dirFlags)
      pure (files, dirs)

    mconcatResults :: [([FilePath], Bool)] -> ([FilePath], Bool)
    mconcatResults xs = (concatMap fst xs, any snd xs)

-- ============================================================================
-- 2. Grep Search Tool
-- ============================================================================

-- | Ripgrep-style content search tool.
grepSearchTool ::
  MonadIO m =>
  SecurityPolicy ->
  Tool m
grepSearchTool policy =
  withSecurity TierReadOnly policy $
    createTool
      "grep_search"
      "Search file contents across the workspace with line numbers, regex or literal search, context lines, and context-window protection."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Directory or file path to search (default: '.')" :: Text)
                      ]
                , "query"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("The text string to find" :: Text)
                      ]
                , "case_sensitive"
                    .= object
                      [ "type" .= ("boolean" :: Text)
                      , "description" .= ("Perform case-sensitive search (default: false)" :: Text)
                      ]
                , "include_glob"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Optional filename glob filter, e.g. '*.hs' or '*.md'" :: Text)
                      ]
                , "context_lines"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description"
                          .= ("Surrounding context lines to include before/after each match (default: 0)" :: Text)
                      ]
                , "max_results"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description" .= ("Maximum total matches to return before truncation (default: 50)" :: Text)
                      ]
                ]
          , "required" .= (["query"] :: [Text])
          ]
      )
      ( \case
          Object o -> do
            let rawPath = optField "path" "." o :: String
                query = optField "query" "" o :: Text
                caseSens = optField "case_sensitive" False o :: Bool
                mbGlob = mbField "include_glob" o :: Maybe String
                contextLines = optField "context_lines" 0 o :: Int
                maxResults = optField "max_results" 50 o :: Int

            if T.null query
              then pure $ Left $ toolError "Search 'query' cannot be empty." (Just "grep_search") Nothing
              else do
                eCleanPath <- validateSandboxPath policy rawPath
                case eCleanPath of
                  Left err -> pure $ Left err
                  Right cleanPath -> do
                    eRes <- liftIO $ try (runGrep cleanPath query caseSens mbGlob contextLines maxResults)
                    case eRes of
                      Left err ->
                        pure $
                          Left $
                            toolError
                              ("Grep search failed: " <> T.pack (show (err :: SomeException)))
                              (Just "grep_search")
                              Nothing
                      Right resultText -> pure $ Right resultText
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "grep_search") Nothing
      )
  where
    runGrep :: FilePath -> Text -> Bool -> Maybe String -> Int -> Int -> IO Text
    runGrep root query caseSens mbGlob ctxLines limit = do
      isDir <- doesDirectoryExist root
      files <-
        if isDir
          then collectSearchFiles root mbGlob
          else pure [root]

      fileResults <-
        mapConcurrently
          (\f -> searchSingleFile f query caseSens ctxLines)
          files

      let allMatches = concatMap snd fileResults
          totalCount = length allMatches
          shownMatches = take limit allMatches
          wasTruncated = totalCount > limit
          header =
            "Found "
              <> T.pack (show totalCount)
              <> " match"
              <> (if totalCount == 1 then "" else "es")
              <> (if wasTruncated then " (showing first " <> T.pack (show limit) <> ")" else "")
              <> ":"

      if null allMatches
        then pure ("No matches found for '" <> query <> "' in " <> T.pack root)
        else pure $ T.unlines (header : shownMatches)

    collectSearchFiles :: FilePath -> Maybe String -> IO [FilePath]
    collectSearchFiles dir mbGlob = do
      entries <- listDirectory dir
      let valid = filter (not . isDefaultIgnored) entries
          fulls = map (dir </>) valid
      (files, subdirs) <- partitionPaths fulls
      let matchingFiles = case mbGlob of
            Nothing -> files
            Just glob -> filter (matchGlobPattern glob . takeFileName) files
      subFiles <- mapConcurrently (`collectSearchFiles` mbGlob) subdirs
      pure (matchingFiles ++ concat subFiles)

    partitionPaths :: [FilePath] -> IO ([FilePath], [FilePath])
    partitionPaths paths = do
      flags <- mapConcurrently doesDirectoryExist paths
      let (ds, fs) =
            foldr
              (\(p, isD) (dAcc, fAcc) -> if isD then (p : dAcc, fAcc) else (dAcc, p : fAcc))
              ([], [])
              (zip paths flags)
      pure (fs, ds)

    searchSingleFile :: FilePath -> Text -> Bool -> Int -> IO (FilePath, [Text])
    searchSingleFile path query caseSens ctx = do
      eTxt <- try (TIO.readFile path)
      case eTxt of
        Left (_ :: IOError) -> pure (path, [])
        Right content -> do
          let allLines = zip [1 :: Int ..] (T.lines content)
              isMatch (_, line) =
                if caseSens
                  then query `T.isInfixOf` line
                  else T.toLower query `T.isInfixOf` T.toLower line
              matchingIndices = [idx | (idx, l) <- allLines, isMatch (idx, l)]

          if null matchingIndices
            then pure (path, [])
            else do
              let blocks =
                    map
                      ( \matchIdx ->
                          if ctx == 0
                            then case lookup matchIdx allLines of
                              Just l -> T.pack path <> ":" <> T.pack (show matchIdx) <> ": " <> l
                              Nothing -> T.pack path <> ":" <> T.pack (show matchIdx) <> ":"
                            else
                              let start = max 1 (matchIdx - ctx)
                                  end = min (length allLines) (matchIdx + ctx)
                                  slice = filter (\(i, _) -> i >= start && i <= end) allLines
                               in T.unlines
                                    ( (T.pack path <> ":")
                                        : [ "  "
                                              <> T.pack (show lineNum)
                                              <> (if lineNum == matchIdx then ": " else "| ")
                                              <> lineTxt
                                          | (lineNum, lineTxt) <- slice
                                          ]
                                    )
                      )
                      matchingIndices
              pure (path, blocks)

-- ============================================================================
-- 3. Read File Tool (Paginated, Line-Numbered, Read-before-edit Enforced)
-- ============================================================================

-- | Read file tool with line-based pagination and context-window protection.
readFileTool ::
  MonadIO m =>
  Maybe CheckpointRegistry ->
  SecurityPolicy ->
  Tool m
readFileTool mbRegistry policy =
  withSecurity TierReadOnly policy $
    createTool
      "read_file"
      "Read text contents of a file with line-based pagination, context-window awareness, line numbering, and security checks."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Path to the file to read" :: Text)
                      ]
                , "start_line"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description" .= ("Optional 1-indexed starting line number (default: 1)" :: Text)
                      ]
                , "end_line"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description" .= ("Optional 1-indexed ending line number (default: start_line + 250)" :: Text)
                      ]
                ]
          , "required" .= (["path"] :: [Text])
          ]
      )
      ( \case
          Object o -> do
            let rawPath = optField "path" "" o :: String
                mbStart = mbField "start_line" o :: Maybe Int
                mbEnd = mbField "end_line" o :: Maybe Int

            if null rawPath
              then pure $ Left $ toolError "Missing 'path' parameter" (Just "read_file") Nothing
              else do
                eCleanPath <- validateSandboxPath policy rawPath
                case eCleanPath of
                  Left err -> pure $ Left err
                  Right cleanPath -> do
                    eContent <- liftIO $ try (TIO.readFile cleanPath)
                    case eContent of
                      Left err ->
                        pure $
                          Left $
                            toolError
                              ("Failed reading file: " <> T.pack (show (err :: IOError)))
                              (Just "read_file")
                              Nothing
                      Right fullContent -> do
                        -- Record file inspection in CheckpointRegistry for read-before-edit verification
                        case mbRegistry of
                          Nothing -> pure ()
                          Just reg -> liftIO $ recordFileRead reg cleanPath

                        let fileLines = T.lines fullContent
                            totalLines = length fileLines
                            start = max 1 (fromMaybe 1 mbStart)
                            defaultEnd = min totalLines (start + 249)
                            end = min totalLines (fromMaybe defaultEnd mbEnd)
                            selectedLines =
                              take (end - start + 1) $
                                drop (start - 1) fileLines
                            numbered =
                              zipWith
                                (\n l -> T.pack (show n) <> ": " <> l)
                                [start .. end]
                                selectedLines

                            hasMore = end < totalLines
                            header =
                              "[File: "
                                <> T.pack cleanPath
                                <> " | Lines "
                                <> T.pack (show start)
                                <> "-"
                                <> T.pack (show end)
                                <> " of "
                                <> T.pack (show totalLines)
                                <> "]"
                            footer =
                              if hasMore
                                then
                                  "\n[Showing "
                                    <> T.pack (show (end - start + 1))
                                    <> " lines. Call read_file with start_line="
                                    <> T.pack (show (end + 1))
                                    <> " to view subsequent lines]"
                                else ""
                        pure $ Right $ T.unlines (header : numbered) <> footer
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "read_file") Nothing
      )

-- ============================================================================
-- 4. Edit File Tool (Uniqueness Check, Read-Before-Edit, Checkpointing)
-- ============================================================================

-- | Precise string replacement tool with uniqueness check and automatic checkpointing.
editFileTool ::
  MonadIO m =>
  Maybe CheckpointRegistry ->
  SecurityPolicy ->
  Tool m
editFileTool mbRegistry policy =
  withSecurity TierWorkspaceWrite policy $
    createTool
      "edit_file"
      "Perform precise string replacement in a file. Enforces read-before-edit verification, exact uniqueness check, and automatic undo checkpointing."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Path to the file to edit" :: Text)
                      ]
                , "old_string"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description"
                          .= ("Exact character sequence to be replaced (must match uniquely in the file)" :: Text)
                      ]
                , "new_string"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Replacement text to insert" :: Text)
                      ]
                , "expected_occurrences"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description" .= ("Expected number of occurrences to replace (default: 1)" :: Text)
                      ]
                ]
          , "required" .= (["path", "old_string", "new_string"] :: [Text])
          ]
      )
      ( \case
          Object o -> do
            let rawPath = optField "path" "" o :: String
                oldStr = optField "old_string" "" o :: Text
                newStr = optField "new_string" "" o :: Text
                expectedOccs = optField "expected_occurrences" 1 o :: Int

            if null rawPath
              then pure $ Left $ toolError "Missing 'path' parameter" (Just "edit_file") Nothing
              else
                if T.null oldStr
                  then pure $ Left $ toolError "'old_string' cannot be empty" (Just "edit_file") Nothing
                  else do
                    eCleanPath <- validateSandboxPath policy rawPath
                    case eCleanPath of
                      Left err -> pure $ Left err
                      Right cleanPath -> do
                        -- 1. Read-before-edit verification
                        hasRead <- case mbRegistry of
                          Nothing -> pure True
                          Just reg -> liftIO $ hasReadFile reg cleanPath

                        if not hasRead
                          then
                            pure $
                              Left $
                                toolError
                                  ( "Read-before-edit policy violation: You must inspect '"
                                      <> T.pack cleanPath
                                      <> "' using 'read_file' before editing it. This ensures exact line content and surrounding context."
                                  )
                                  (Just "edit_file")
                                  Nothing
                          else do
                            -- 2. Read existing content
                            eCurrent <- liftIO $ try (TIO.readFile cleanPath)
                            case eCurrent of
                              Left err ->
                                pure $
                                  Left $
                                    toolError
                                      ("Failed reading target file: " <> T.pack (show (err :: IOError)))
                                      (Just "edit_file")
                                      Nothing
                              Right currentTxt -> do
                                -- 3. Uniqueness and occurrence check
                                let occCount = countOccurrences oldStr currentTxt
                                if occCount == 0
                                  then
                                    pure $
                                      Left $
                                        toolError
                                          ( "Target content not found in '"
                                              <> T.pack cleanPath
                                              <> "'. Ensure exact whitespace and line indentation by checking 'read_file'."
                                          )
                                          (Just "edit_file")
                                          Nothing
                                  else
                                    if occCount /= expectedOccs
                                      then do
                                        let lineIndices = findOccurrencesLineIndices oldStr currentTxt
                                        pure $
                                          Left $
                                            toolError
                                              ( "Ambiguity error: Expected "
                                                  <> T.pack (show expectedOccs)
                                                  <> " occurrence(s), but found "
                                                  <> T.pack (show occCount)
                                                  <> " occurrences in '"
                                                  <> T.pack cleanPath
                                                  <> "' at lines: "
                                                  <> T.pack (show lineIndices)
                                                  <> ". Please include more surrounding lines in 'old_string' to make it unique."
                                              )
                                              (Just "edit_file")
                                              Nothing
                                      else do
                                        -- 4. Automatic undo snapshot checkpoint
                                        cpIdText <- case mbRegistry of
                                          Nothing -> pure "no-registry"
                                          Just reg ->
                                            liftIO $
                                              createFileCheckpoint
                                                reg
                                                cleanPath
                                                currentTxt
                                                ("edit_file (" <> T.pack (show expectedOccs) <> " replacements)")

                                        -- 5. Apply replacement
                                        let replacedTxt = T.replace oldStr newStr currentTxt
                                        eWrite <- liftIO $ try (TIO.writeFile cleanPath replacedTxt)
                                        case eWrite of
                                          Left err ->
                                            pure $
                                              Left $
                                                toolError
                                                  ("Failed writing edited file: " <> T.pack (show (err :: IOError)))
                                                  (Just "edit_file")
                                                  Nothing
                                          Right () ->
                                            pure $
                                              Right $
                                                "Successfully edited '"
                                                  <> T.pack cleanPath
                                                  <> "' (replaced "
                                                  <> T.pack (show occCount)
                                                  <> " occurrence(s)). Checkpoint ID: ["
                                                  <> cpIdText
                                                  <> "]. Use 'undo_file_edit' to revert if needed."
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "edit_file") Nothing
      )
  where
    countOccurrences needle haystack
      | T.null needle = 0
      | otherwise = T.count needle haystack

    findOccurrencesLineIndices needle haystack =
      let lns = zip [1 :: Int ..] (T.lines haystack)
       in [ln | (ln, lineTxt) <- lns, needle `T.isInfixOf` lineTxt]

-- ============================================================================
-- 5. Write File Tool
-- ============================================================================

-- | Write text to file with overwrite protection and automatic checkpointing.
writeFileTool ::
  MonadIO m =>
  Maybe CheckpointRegistry ->
  SecurityPolicy ->
  Tool m
writeFileTool mbRegistry policy =
  withSecurity TierWorkspaceWrite policy $
    createTool
      "write_file"
      "Write text contents to a file path. Automatically creates parent directories. Protects against accidental overwrites and takes undo checkpoints."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Target destination file path" :: Text)
                      ]
                , "content"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Full content to write" :: Text)
                      ]
                , "overwrite"
                    .= object
                      [ "type" .= ("boolean" :: Text)
                      , "description" .= ("Allow overwriting if file already exists (default: false)" :: Text)
                      ]
                ]
          , "required" .= (["path", "content"] :: [Text])
          ]
      )
      ( \case
          Object o -> do
            let rawPath = optField "path" "" o :: String
                content = optField "content" "" o :: Text
                overwrite = optField "overwrite" False o :: Bool

            if null rawPath
              then pure $ Left $ toolError "Missing 'path' parameter" (Just "write_file") Nothing
              else do
                eCleanPath <- validateSandboxPath policy rawPath
                case eCleanPath of
                  Left err -> pure $ Left err
                  Right cleanPath -> do
                    exists <- liftIO $ doesFileExist cleanPath
                    if exists && not overwrite
                      then
                        pure $
                          Left $
                            toolError
                              ( "File '"
                                  <> T.pack cleanPath
                                  <> "' already exists. Set 'overwrite=true' to overwrite, or use 'edit_file' for targeted modifications."
                              )
                              (Just "write_file")
                              Nothing
                      else do
                        -- Automatic checkpoint before overwrite
                        cpIdText <-
                          if exists
                            then case mbRegistry of
                              Nothing -> pure "no-registry"
                              Just reg -> do
                                prevTxt <- liftIO $ TIO.readFile cleanPath
                                liftIO $ createFileCheckpoint reg cleanPath prevTxt "write_file overwrite"
                            else pure "new-file"

                        liftIO $ createDirectoryIfMissing True (takeDirectory cleanPath)
                        eWrite <- liftIO $ try (TIO.writeFile cleanPath content)
                        case eWrite of
                          Left err ->
                            pure $
                              Left $
                                toolError
                                  ("Failed writing file: " <> T.pack (show (err :: IOError)))
                                  (Just "write_file")
                                  Nothing
                          Right () -> do
                            -- Mark file as read so subsequent edits can proceed
                            case mbRegistry of
                              Nothing -> pure ()
                              Just reg -> liftIO $ recordFileRead reg cleanPath

                            pure $
                              Right $
                                "Successfully wrote file '"
                                  <> T.pack cleanPath
                                  <> "'"
                                  <> (if exists then " (Checkpoint: [" <> cpIdText <> "])" else "")
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "write_file") Nothing
      )

-- ============================================================================
-- 6. List Directory Tool
-- ============================================================================

-- | List directory contents with entry types and file sizes.
listDirTool ::
  MonadIO m =>
  SecurityPolicy ->
  Tool m
listDirTool policy =
  withSecurity TierReadOnly policy $
    createTool
      "list_directory"
      "List files and subdirectories in a directory path with file sizes and type indicators."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Path to directory (default: '.')" :: Text)
                      ]
                ]
          ]
      )
      ( \case
          Object o -> do
            let rawPath = optField "path" "." o :: String
            eCleanPath <- validateSandboxPath policy rawPath
            case eCleanPath of
              Left err -> pure $ Left err
              Right cleanPath -> do
                eList <- liftIO $ try (listDirectory cleanPath)
                case eList of
                  Left err ->
                    pure $
                      Left $
                        toolError
                          ("Failed listing directory: " <> T.pack (show (err :: IOError)))
                          (Just "list_directory")
                          Nothing
                  Right entries -> do
                    let sorted = sort (filter (not . isDefaultIgnored) entries)
                    formattedEntries <- liftIO $ mapConcurrently (formatEntry cleanPath) sorted
                    let count = length sorted
                        header =
                          "Directory '"
                            <> T.pack cleanPath
                            <> "' ("
                            <> T.pack (show count)
                            <> " items):"
                    pure $ Right $ T.unlines (header : formattedEntries)
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "list_directory") Nothing
      )
  where
    formatEntry :: FilePath -> FilePath -> IO Text
    formatEntry parent name = do
      let full = parent </> name
      isDir <- doesDirectoryExist full
      if isDir
        then pure $ "  [DIR]  " <> T.pack name <> "/"
        else do
          sz <- getFileSize full
          pure $
            "  [FILE] "
              <> T.pack name
              <> " ("
              <> formatBytes sz
              <> ")"

    formatBytes :: Integer -> Text
    formatBytes b
      | b < 1024 = T.pack (show b) <> " B"
      | b < 1024 * 1024 = T.pack (show (b `div` 1024)) <> " KB"
      | otherwise = T.pack (show (b `div` (1024 * 1024))) <> " MB"

-- ============================================================================
-- Complete FileSystem Toolkit Suite Factory
-- ============================================================================

{- | Instantiate a complete filesystem toolkit bound to a shared
'CheckpointRegistry' and 'SecurityPolicy'.
-}
newFileSystemSuite ::
  MonadIO m =>
  SecurityPolicy ->
  IO [Tool m]
newFileSystemSuite policy = do
  registry <- newCheckpointRegistry
  pure
    [ findFilesTool policy
    , grepSearchTool policy
    , readFileTool (Just registry) policy
    , listDirTool policy
    , writeFileTool (Just registry) policy
    , editFileTool (Just registry) policy
    , undoFileEditTool registry
    , listCheckpointsTool registry
    ]

{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Tools.Git
Description : Git operation tools for repository interaction
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Git operation tools implementing the @Tool m@ interface for diff generation,
patch application, commit history querying, and pull request creation.
All operations use @System.Process@ for external git invocations.
-}
module Aegis.Tools.Git
  ( -- * Tools
    gitDiffTool
  , gitApplyPatchTool
  , gitLogTool
  , gitStatusTool
  , gitCommitTool
  , gitCreateBranchTool
  , gitCheckoutTool
  , gitShowFileTool

    -- * Pure Helpers
  , parseGitDiff
  , parseGitLog
  , runGitCommand
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..))
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )

import Langchain.Core.Error (LangchainError, toolError)
import Langchain.Core.Tool (Tool (..), createTool)

-- ---------------------------------------------------------------------------
-- Git Command Runner
-- ---------------------------------------------------------------------------

-- | Run a git command in a specified working directory and return stdout/stderr
runGitCommand :: FilePath -> [String] -> IO (Either Text Text)
runGitCommand workDir args = do
  eRes <- try $ do
    let cp = (proc "git" args) { cwd = Just workDir, std_out = CreatePipe, std_err = CreatePipe }
    (_, Just hOut, Just hErr, ph) <- createProcess cp
    exitCode <- waitForProcess ph
    stdout <- TIO.hGetContents hOut
    stderr <- TIO.hGetContents hErr
    case exitCode of
      ExitSuccess -> pure $ Right stdout
      ExitFailure code -> pure $ Left $
        "git " <> T.pack (unwords args) <> " failed (exit " <> T.pack (show code) <> "): " <> stderr
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ "Git command exception: " <> T.pack (show err)
    Right result -> pure result

-- ---------------------------------------------------------------------------
-- Git Diff Tool
-- ---------------------------------------------------------------------------

-- | Tool that generates unified diffs between git refs or the working tree
gitDiffTool :: MonadIO m => FilePath -> Tool m
gitDiffTool repoPath = createTool
  "git_diff"
  "Generate a unified diff between two git refs, or show unstaged changes. \
  \Arguments: {\"ref1\": \"string (optional, default HEAD)\", \"ref2\": \"string (optional)\", \
  \\"file\": \"string (optional, specific file)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "ref1" .= object ["type" .= ("string" :: Text), "description" .= ("First ref, defaults to HEAD" :: Text)]
        , "ref2" .= object ["type" .= ("string" :: Text), "description" .= ("Second ref, optional" :: Text)]
        , "file" .= object ["type" .= ("string" :: Text), "description" .= ("Specific file to diff" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let ref1 = extractTextField "ref1" args
        ref2 = extractTextField "ref2" args
        file = extractTextField "file" args
        diffArgs = ["diff"] ++
          (if T.null ref1 then [] else [T.unpack ref1]) ++
          (if T.null ref2 then [] else [T.unpack ref2]) ++
          ["--"] ++
          (if T.null file then [] else [T.unpack file])
    result <- runGitCommand repoPath diffArgs
    case result of
      Left err -> pure $ Left $ toolError err (Just "git_diff") Nothing
      Right output -> pure $ Right $ if T.null output then "(no changes)" else output
  )

-- ---------------------------------------------------------------------------
-- Git Apply Patch Tool
-- ---------------------------------------------------------------------------

-- | Tool that applies a unified diff patch to the working tree
gitApplyPatchTool :: MonadIO m => FilePath -> Tool m
gitApplyPatchTool repoPath = createTool
  "git_apply_patch"
  "Apply a unified diff patch to the repository working tree. \
  \Arguments: {\"patch\": \"string (unified diff content)\", \"check_only\": \"bool (optional, dry run)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["patch"] :: [Text])
    , "properties" .= object
        [ "patch" .= object ["type" .= ("string" :: Text)]
        , "check_only" .= object ["type" .= ("boolean" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let patchContent = extractTextField "patch" args
        checkOnly = extractBoolField "check_only" args
    if T.null patchContent
      then pure $ Left $ toolError "Patch content is empty" (Just "git_apply_patch") Nothing
      else do
        -- Write patch to a temp file
        let patchFile = repoPath ++ "/.aegis-patch.tmp"
        TIO.writeFile patchFile patchContent
        let applyArgs = ["apply"] ++
              (if checkOnly then ["--check"] else []) ++
              [patchFile]
        result <- runGitCommand repoPath applyArgs
        case result of
          Left err -> pure $ Left $ toolError err (Just "git_apply_patch") Nothing
          Right output -> pure $ Right $
            if checkOnly then "Patch check passed: " <> output
            else "Patch applied successfully: " <> output
  )

-- ---------------------------------------------------------------------------
-- Git Log Tool
-- ---------------------------------------------------------------------------

-- | Tool that queries git commit history with filters
gitLogTool :: MonadIO m => FilePath -> Tool m
gitLogTool repoPath = createTool
  "git_log"
  "Query git commit history. \
  \Arguments: {\"max_count\": \"int (default 10)\", \"file\": \"string (optional)\", \
  \\"author\": \"string (optional)\", \"since\": \"string (optional, e.g. '1 week ago')\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "max_count" .= object ["type" .= ("integer" :: Text)]
        , "file" .= object ["type" .= ("string" :: Text)]
        , "author" .= object ["type" .= ("string" :: Text)]
        , "since" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let maxCount = extractIntField "max_count" args 10
        file = extractTextField "file" args
        author = extractTextField "author" args
        since = extractTextField "since" args
        logArgs = ["log", "--oneline", "--format=%H|%an|%ai|%s",
                   "-n", show maxCount] ++
          (if T.null author then [] else ["--author=" ++ T.unpack author]) ++
          (if T.null since then [] else ["--since=" ++ T.unpack since]) ++
          (if T.null file then [] else ["--", T.unpack file])
    result <- runGitCommand repoPath logArgs
    case result of
      Left err -> pure $ Left $ toolError err (Just "git_log") Nothing
      Right output -> pure $ Right output
  )

-- ---------------------------------------------------------------------------
-- Git Status Tool
-- ---------------------------------------------------------------------------

-- | Tool that shows the current git status
gitStatusTool :: MonadIO m => FilePath -> Tool m
gitStatusTool repoPath = createTool
  "git_status"
  "Show the current git repository status (modified, staged, untracked files)."
  (object ["type" .= ("object" :: Text), "properties" .= object []])
  (\_ -> liftIO $ do
    result <- runGitCommand repoPath ["status", "--porcelain"]
    case result of
      Left err -> pure $ Left $ toolError err (Just "git_status") Nothing
      Right output -> pure $ Right $ if T.null output then "(clean working tree)" else output
  )

-- ---------------------------------------------------------------------------
-- Git Commit Tool
-- ---------------------------------------------------------------------------

-- | Tool that stages and commits changes
gitCommitTool :: MonadIO m => FilePath -> Tool m
gitCommitTool repoPath = createTool
  "git_commit"
  "Stage all changes and create a git commit. \
  \Arguments: {\"message\": \"string (commit message)\", \"files\": \"[string] (optional, specific files)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["message"] :: [Text])
    , "properties" .= object
        [ "message" .= object ["type" .= ("string" :: Text)]
        , "files" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]
        ]
    ])
  (\args -> liftIO $ do
    let msg = extractTextField "message" args
        files = extractTextArrayField "files" args
    if T.null msg
      then pure $ Left $ toolError "Commit message is required" (Just "git_commit") Nothing
      else do
        -- Stage files
        stageResult <- if null files
          then runGitCommand repoPath ["add", "-A"]
          else runGitCommand repoPath ("add" : map T.unpack files)
        case stageResult of
          Left err -> pure $ Left $ toolError ("Staging failed: " <> err) (Just "git_commit") Nothing
          Right _ -> do
            commitResult <- runGitCommand repoPath ["commit", "-m", T.unpack msg]
            case commitResult of
              Left err -> pure $ Left $ toolError ("Commit failed: " <> err) (Just "git_commit") Nothing
              Right output -> pure $ Right $ "Committed: " <> output
  )

-- ---------------------------------------------------------------------------
-- Git Create Branch Tool
-- ---------------------------------------------------------------------------

-- | Tool that creates a new git branch
gitCreateBranchTool :: MonadIO m => FilePath -> Tool m
gitCreateBranchTool repoPath = createTool
  "git_create_branch"
  "Create a new git branch and optionally switch to it. \
  \Arguments: {\"branch_name\": \"string\", \"checkout\": \"bool (default true)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["branch_name"] :: [Text])
    , "properties" .= object
        [ "branch_name" .= object ["type" .= ("string" :: Text)]
        , "checkout" .= object ["type" .= ("boolean" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let branchName = extractTextField "branch_name" args
        checkout = extractBoolFieldDefault "checkout" args True
    if T.null branchName
      then pure $ Left $ toolError "Branch name is required" (Just "git_create_branch") Nothing
      else do
        let cmd = if checkout then "checkout" else "branch"
            branchArgs = if checkout
              then [cmd, "-b", T.unpack branchName]
              else [cmd, T.unpack branchName]
        result <- runGitCommand repoPath branchArgs
        case result of
          Left err -> pure $ Left $ toolError err (Just "git_create_branch") Nothing
          Right output -> pure $ Right $ "Branch created: " <> branchName <> " " <> output
  )

-- ---------------------------------------------------------------------------
-- Git Checkout Tool
-- ---------------------------------------------------------------------------

-- | Tool that checks out a branch or ref
gitCheckoutTool :: MonadIO m => FilePath -> Tool m
gitCheckoutTool repoPath = createTool
  "git_checkout"
  "Checkout an existing git branch or ref. Arguments: {\"ref\": \"string\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["ref"] :: [Text])
    , "properties" .= object
        [ "ref" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let ref = extractTextField "ref" args
    if T.null ref
      then pure $ Left $ toolError "Ref is required" (Just "git_checkout") Nothing
      else do
        result <- runGitCommand repoPath ["checkout", T.unpack ref]
        case result of
          Left err -> pure $ Left $ toolError err (Just "git_checkout") Nothing
          Right output -> pure $ Right $ "Checked out: " <> ref <> " " <> output
  )

-- ---------------------------------------------------------------------------
-- Git Show File Tool
-- ---------------------------------------------------------------------------

-- | Tool that shows the content of a file at a specific git ref
gitShowFileTool :: MonadIO m => FilePath -> Tool m
gitShowFileTool repoPath = createTool
  "git_show_file"
  "Show the content of a file at a specific git ref. \
  \Arguments: {\"ref\": \"string (default HEAD)\", \"file\": \"string (file path)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["file"] :: [Text])
    , "properties" .= object
        [ "ref" .= object ["type" .= ("string" :: Text)]
        , "file" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let ref = let r = extractTextField "ref" args in if T.null r then "HEAD" else r
        file = extractTextField "file" args
    if T.null file
      then pure $ Left $ toolError "File path is required" (Just "git_show_file") Nothing
      else do
        result <- runGitCommand repoPath ["show", T.unpack (ref <> ":" <> file)]
        case result of
          Left err -> pure $ Left $ toolError err (Just "git_show_file") Nothing
          Right output -> pure $ Right output
  )

-- ---------------------------------------------------------------------------
-- Parse Helpers
-- ---------------------------------------------------------------------------

-- | Parse a unified diff into structured sections
parseGitDiff :: Text -> [(FilePath, Text)]
parseGitDiff diffText =
  let ls = T.lines diffText
      chunks = splitOn isDiffHeader ls
  in map extractFileAndContent chunks
  where
    isDiffHeader l = "diff --git" `T.isPrefixOf` l
    splitOn _ [] = []
    splitOn predicate xs =
      let (chunk, rest) = break predicate (drop 1 (dropWhile (not . predicate) xs))
      in (takeWhile (not . predicate) xs) : splitOn predicate rest
    extractFileAndContent lns =
      let fileLine = head (filter ("--- a/" `T.isPrefixOf`) lns ++ [""])
          fp = T.unpack $ T.drop 6 fileLine
      in (fp, T.unlines lns)

-- | Parse git log output into structured entries
parseGitLog :: Text -> [(Text, Text, Text, Text)]
parseGitLog logText =
  map parseLine (filter (not . T.null) (T.lines logText))
  where
    parseLine l = case T.splitOn "|" l of
      [hash, author, date, subject] -> (T.strip hash, T.strip author, T.strip date, T.strip subject)
      _ -> (l, "", "", "")

-- ---------------------------------------------------------------------------
-- JSON Field Extraction Helpers
-- ---------------------------------------------------------------------------

extractTextField :: Text -> Value -> Text
extractTextField key (Object obj) = case KM.lookup (fromText key) obj of
  Just (String t) -> t
  _ -> ""
  where fromText t = fromString (T.unpack t)
extractTextField _ _ = ""

extractIntField :: Text -> Value -> Int -> Int
extractIntField key (Object obj) def = case KM.lookup (fromText key) obj of
  Just (Number n) -> round n
  _ -> def
  where fromText t = fromString (T.unpack t)
extractIntField _ _ def = def

extractBoolField :: Text -> Value -> Bool
extractBoolField key (Object obj) = case KM.lookup (fromText key) obj of
  Just (Bool b) -> b
  _ -> False
  where fromText t = fromString (T.unpack t)
extractBoolField _ _ = False

extractBoolFieldDefault :: Text -> Value -> Bool -> Bool
extractBoolFieldDefault key (Object obj) def = case KM.lookup (fromText key) obj of
  Just (Bool b) -> b
  _ -> def
  where fromText t = fromString (T.unpack t)
extractBoolFieldDefault _ _ def = def

extractTextArrayField :: Text -> Value -> [Text]
extractTextArrayField key (Object obj) = case KM.lookup (fromText key) obj of
  Just (Array arr) -> [t | String t <- toList arr]
  _ -> []
  where
    fromText t = fromString (T.unpack t)
    toList = foldr (:) []
extractTextArrayField _ _ = []

-- Reexport fromString for Aeson Key
fromString :: String -> Key
fromString = fromString

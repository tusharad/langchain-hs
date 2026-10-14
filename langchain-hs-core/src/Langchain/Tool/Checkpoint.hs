{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Tool.Checkpoint
Description : Undo checkpoints, file snapshots, and audit log tracking for tool calls
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides transactional undo snapshots before file modifications, audit logging of tool calls,
and read-before-edit verification.
-}
module Langchain.Tool.Checkpoint
  ( -- * Types
    FileCheckpoint (..)
  , ToolAuditEntry (..)
  , CheckpointRegistry (..)

    -- * Registry Operations
  , newCheckpointRegistry
  , recordFileRead
  , hasReadFile
  , createFileCheckpoint
  , rollbackCheckpoint
  , recordAuditEntry
  , getAuditHistory
  , listCheckpoints

    -- * Tools
  , undoFileEditTool
  , listCheckpointsTool
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, Value (..), object, (.:?), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (normalise)

import Langchain.Error (LangchainError, toolError)
import Langchain.Tool (Tool (..), createTool)
import Langchain.Tool.Security (ToolTier (..))

-- | Represents an automatic undo snapshot taken before a file was modified.
data FileCheckpoint = FileCheckpoint
  { cpId :: !Text
  -- ^ Unique checkpoint identifier (e.g. @"cp-1"@)
  , cpTimestamp :: !UTCTime
  -- ^ Time when the snapshot was recorded
  , cpFilePath :: !FilePath
  -- ^ Canonical path of the modified file
  , cpPreviousContent :: !Text
  -- ^ File content before the modification
  , cpDescription :: !Text
  -- ^ Operation description (e.g. @"edit_file: replace 'foo' with 'bar'"@)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Audit trail record for a single tool call.
data ToolAuditEntry = ToolAuditEntry
  { auditStep :: !Int
  -- ^ Step index of the tool call
  , auditTimestamp :: !UTCTime
  -- ^ Timestamp of invocation
  , auditToolName :: !Text
  -- ^ Name of tool executed
  , auditArguments :: !Value
  -- ^ Arguments supplied to the tool
  , auditResult :: !(Either Text Text)
  -- ^ Execution result (Left error or Right output)
  , auditTier :: !ToolTier
  -- ^ Permission tier of the tool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Stateful in-memory STM registry tracking file checkpoints, audit history, and inspected files.
data CheckpointRegistry = CheckpointRegistry
  { regFileCheckpoints :: !(TVar [FileCheckpoint])
  , regAuditLog :: !(TVar [ToolAuditEntry])
  , regReadFiles :: !(TVar (Set.Set FilePath))
  , regNextId :: !(TVar Int)
  }

-- | Initialize a new in-memory checkpoint and audit registry.
newCheckpointRegistry :: IO CheckpointRegistry
newCheckpointRegistry = do
  cpsVar <- newTVarIO []
  audVar <- newTVarIO []
  readVar <- newTVarIO Set.empty
  idVar <- newTVarIO 1
  pure $ CheckpointRegistry cpsVar audVar readVar idVar

-- | Normalize a path for read-tracking lookups.
normalizeForTracking :: FilePath -> IO FilePath
normalizeForTracking fp = do
  exists <- doesFileExist fp
  if exists
    then canonicalizePath fp
    else pure (normalise fp)

-- | Record that a file was read by the agent (for read-before-edit verification).
recordFileRead :: CheckpointRegistry -> FilePath -> IO ()
recordFileRead CheckpointRegistry {..} rawPath = do
  normPath <- normalizeForTracking rawPath
  atomically $ modifyTVar' regReadFiles (Set.insert normPath)

-- | Check whether a file was previously inspected via read_file.
hasReadFile :: CheckpointRegistry -> FilePath -> IO Bool
hasReadFile CheckpointRegistry {..} rawPath = do
  normPath <- normalizeForTracking rawPath
  readsSet <- readTVarIO regReadFiles
  pure $ Set.member normPath readsSet

-- | Capture an undo snapshot before modifying or writing a file.
createFileCheckpoint ::
  CheckpointRegistry ->
  FilePath ->
  Text ->
  Text ->
  IO Text
createFileCheckpoint CheckpointRegistry {..} rawPath prevContent desc = do
  normPath <- normalizeForTracking rawPath
  now <- getCurrentTime
  atomically $ do
    currId <- readTVar regNextId
    writeTVar regNextId (currId + 1)
    let cpIdentifier = "cp-" <> T.pack (show currId)
        checkpoint =
          FileCheckpoint
            { cpId = cpIdentifier
            , cpTimestamp = now
            , cpFilePath = normPath
            , cpPreviousContent = prevContent
            , cpDescription = desc
            }
    modifyTVar' regFileCheckpoints (checkpoint :)
    pure cpIdentifier

{- | Roll back a file to its previous checkpoint content.

If no 'cpId' is specified, reverts the most recent checkpoint.
-}
rollbackCheckpoint ::
  CheckpointRegistry ->
  Maybe Text ->
  IO (Either LangchainError Text)
rollbackCheckpoint CheckpointRegistry {..} mbTargetId = do
  mCp <- atomically $ do
    cps <- readTVar regFileCheckpoints
    case mbTargetId of
      Nothing -> case cps of
        [] -> pure Nothing
        (latest : rest) -> do
          writeTVar regFileCheckpoints rest
          pure (Just latest)
      Just targetId ->
        case break (\c -> cpId c == targetId) cps of
          (_, []) -> pure Nothing
          (before, target : after) -> do
            writeTVar regFileCheckpoints (before ++ after)
            pure (Just target)

  case mCp of
    Nothing ->
      pure $
        Left $
          toolError
            ( case mbTargetId of
                Nothing -> "No checkpoints available to roll back."
                Just tid -> "Checkpoint '" <> tid <> "' not found in registry."
            )
            (Just "undo_file_edit")
            Nothing
    Just FileCheckpoint {..} -> do
      eWrite <- try (TIO.writeFile cpFilePath cpPreviousContent)
      case eWrite of
        Left err ->
          pure $
            Left $
              toolError
                ("Failed to restore file during rollback: " <> T.pack (show (err :: IOError)))
                (Just "undo_file_edit")
                Nothing
        Right () ->
          pure $
            Right $
              "Successfully restored '"
                <> T.pack cpFilePath
                <> "' to checkpoint ["
                <> cpId
                <> "] ("
                <> cpDescription
                <> ")."

-- | Record a completed tool call into the audit log.
recordAuditEntry ::
  CheckpointRegistry ->
  Text ->
  Value ->
  Either Text Text ->
  ToolTier ->
  IO ()
recordAuditEntry CheckpointRegistry {..} toolNm args res tier = do
  now <- getCurrentTime
  atomically $ do
    step <- readTVar regNextId
    let entry =
          ToolAuditEntry
            { auditStep = step
            , auditTimestamp = now
            , auditToolName = toolNm
            , auditArguments = args
            , auditResult = res
            , auditTier = tier
            }
    modifyTVar' regAuditLog (entry :)

-- | Retrieve all audit log entries in chronological order.
getAuditHistory :: CheckpointRegistry -> IO [ToolAuditEntry]
getAuditHistory CheckpointRegistry {..} = reverse <$> readTVarIO regAuditLog

-- | Retrieve all available file checkpoints.
listCheckpoints :: CheckpointRegistry -> IO [FileCheckpoint]
listCheckpoints CheckpointRegistry {..} = readTVarIO regFileCheckpoints

-- | Tool that rolls back a file modification to a previous checkpoint.
undoFileEditTool :: MonadIO m => CheckpointRegistry -> Tool m
undoFileEditTool registry =
  createTool
    "undo_file_edit"
    "Revert a file to its previous state before an edit or write operation was performed. Reverts the most recent edit by default, or a specific checkpoint_id."
    ( object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
              [ "checkpoint_id"
                  .= object
                    [ "type" .= ("string" :: Text)
                    , "description"
                        .= ( "Optional checkpoint ID (e.g. 'cp-1') to revert to. If omitted, rolls back the most recent edit." ::
                               Text
                           )
                    ]
              ]
        ]
    )
    ( \case
        Object o -> do
          let mbId = case parseEither (.:? "checkpoint_id") o of
                Right m -> m
                _ -> Nothing
          liftIO $ rollbackCheckpoint registry mbId
        _ -> liftIO $ rollbackCheckpoint registry Nothing
    )

-- | Tool that lists all active undo checkpoints.
listCheckpointsTool :: MonadIO m => CheckpointRegistry -> Tool m
listCheckpointsTool registry =
  createTool
    "list_checkpoints"
    "List all available file modification checkpoints that can be reverted."
    ( object
        [ "type" .= ("object" :: Text)
        , "properties" .= object []
        ]
    )
    ( \_ -> liftIO $ do
        cps <- listCheckpoints registry
        if null cps
          then pure $ Right "No active file checkpoints."
          else
            let linesFormatted =
                  map
                    ( \FileCheckpoint {..} ->
                        "["
                          <> cpId
                          <> "] "
                          <> T.pack cpFilePath
                          <> " — "
                          <> cpDescription
                          <> " ("
                          <> T.pack (show cpTimestamp)
                          <> ")"
                    )
                    cps
             in pure $ Right $ T.unlines ("Active Checkpoints:" : linesFormatted)
    )

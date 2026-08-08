{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Graph.Checkpointer
Description : SQLite-backed checkpointer for pipeline state persistence
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

SQLite-backed checkpoint persistence for saving and restoring pipeline state.
Enables HITL workflows by allowing the pipeline to pause, persist state,
and resume from any checkpoint. Uses the langchain-hs-graph Checkpointer
typeclass interface.
-}
module Aegis.Graph.Checkpointer
  ( -- * Checkpointer
    AegisCheckpointer (..)
  , newAegisCheckpointer
  , initializeSchema

    -- * Operations
  , saveCheckpoint
  , loadCheckpoint
  , listCheckpoints
  , deleteCheckpoint
  , getLatestCheckpoint

    -- * Types
  , CheckpointEntry (..)
  ) where

import Control.Exception (try, SomeException, bracket)
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Aegis.Core.Types.Pipeline (AegisState)
import Aegis.Core.Types.Config (DatabaseConfig (..))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A saved checkpoint entry
data CheckpointEntry = CheckpointEntry
  { cpThreadId :: Text
  -- ^ Pipeline thread ID
  , cpNodeId :: Text
  -- ^ Graph node where checkpoint was saved
  , cpStateJson :: Text
  -- ^ Serialized pipeline state as JSON
  , cpCreatedAt :: Text
  -- ^ Timestamp string
  }
  deriving (Eq, Show)

instance FromRow CheckpointEntry where
  fromRow = CheckpointEntry <$> field <*> field <*> field <*> field

instance ToRow CheckpointEntry where
  toRow (CheckpointEntry tid nid sj ca) = toRow (tid, nid, sj, ca)

-- ---------------------------------------------------------------------------
-- Checkpointer
-- ---------------------------------------------------------------------------

-- | SQLite-backed checkpointer for AegisCode AI pipeline state
data AegisCheckpointer = AegisCheckpointer
  { acDbPath :: FilePath
  -- ^ Path to the SQLite database file
  }

-- | Create a new checkpointer and initialize the database schema
newAegisCheckpointer :: DatabaseConfig -> IO AegisCheckpointer
newAegisCheckpointer dbConfig = do
  let cp = AegisCheckpointer { acDbPath = dbFilePath dbConfig }
  initializeSchema cp
  pure cp

-- | Initialize the SQLite schema for checkpoints
initializeSchema :: AegisCheckpointer -> IO ()
initializeSchema cp = withDB cp $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS checkpoints (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  thread_id TEXT NOT NULL, \
    \  node_id TEXT NOT NULL, \
    \  state_json TEXT NOT NULL, \
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')), \
    \  UNIQUE(thread_id, node_id, created_at)\
    \)"
  execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_checkpoints_thread ON checkpoints(thread_id)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS scan_history (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  scan_id TEXT NOT NULL UNIQUE, \
    \  thread_id TEXT NOT NULL, \
    \  repo_path TEXT NOT NULL, \
    \  status TEXT NOT NULL, \
    \  total_findings INTEGER DEFAULT 0, \
    \  remediated INTEGER DEFAULT 0, \
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')), \
    \  completed_at TEXT\
    \)"

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Save a checkpoint for a pipeline thread at a specific node
saveCheckpoint :: AegisCheckpointer -> Text -> Text -> AegisState -> IO (Either Text ())
saveCheckpoint cp threadId nodeId state = do
  let stateJson = TE.decodeUtf8 $ BL.toStrict $ encode state
  now <- getCurrentTime
  eRes <- try $ withDB cp $ \conn ->
    execute conn
      "INSERT INTO checkpoints (thread_id, node_id, state_json, created_at) VALUES (?, ?, ?, ?)"
      (threadId, nodeId, stateJson, T.pack (show now))
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ "Checkpoint save failed: " <> T.pack (show err)
    Right () -> pure $ Right ()

-- | Load the most recent checkpoint for a thread
loadCheckpoint :: AegisCheckpointer -> Text -> IO (Either Text AegisState)
loadCheckpoint cp threadId = do
  eRes <- try $ withDB cp $ \conn -> do
    rows <- query conn
      "SELECT thread_id, node_id, state_json, created_at FROM checkpoints \
      \WHERE thread_id = ? ORDER BY created_at DESC LIMIT 1"
      (Only threadId) :: IO [CheckpointEntry]
    pure rows
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ "Checkpoint load failed: " <> T.pack (show err)
    Right [] -> pure $ Left $ "No checkpoint found for thread: " <> threadId
    Right (entry : _) ->
      case decode (BL.fromStrict (TE.encodeUtf8 (cpStateJson entry))) of
        Nothing -> pure $ Left "Failed to deserialize checkpoint state"
        Just state -> pure $ Right state

-- | Load a checkpoint at a specific node
loadCheckpointAtNode :: AegisCheckpointer -> Text -> Text -> IO (Either Text AegisState)
loadCheckpointAtNode cp threadId nodeId = do
  eRes <- try $ withDB cp $ \conn -> do
    rows <- query conn
      "SELECT thread_id, node_id, state_json, created_at FROM checkpoints \
      \WHERE thread_id = ? AND node_id = ? ORDER BY created_at DESC LIMIT 1"
      (threadId, nodeId) :: IO [CheckpointEntry]
    pure rows
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ "Checkpoint load failed: " <> T.pack (show err)
    Right [] -> pure $ Left $ "No checkpoint found for thread " <> threadId <> " at node " <> nodeId
    Right (entry : _) ->
      case decode (BL.fromStrict (TE.encodeUtf8 (cpStateJson entry))) of
        Nothing -> pure $ Left "Failed to deserialize checkpoint state"
        Just state -> pure $ Right state

-- | List all checkpoints for a thread
listCheckpoints :: AegisCheckpointer -> Text -> IO (Either Text [CheckpointEntry])
listCheckpoints cp threadId = do
  eRes <- try $ withDB cp $ \conn ->
    query conn
      "SELECT thread_id, node_id, state_json, created_at FROM checkpoints \
      \WHERE thread_id = ? ORDER BY created_at DESC"
      (Only threadId) :: IO [CheckpointEntry]
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ "Checkpoint list failed: " <> T.pack (show err)
    Right entries -> pure $ Right entries

-- | Get the latest checkpoint across all threads
getLatestCheckpoint :: AegisCheckpointer -> IO (Either Text (Maybe CheckpointEntry))
getLatestCheckpoint cp = do
  eRes <- try $ withDB cp $ \conn ->
    query_ conn
      "SELECT thread_id, node_id, state_json, created_at FROM checkpoints \
      \ORDER BY created_at DESC LIMIT 1" :: IO [CheckpointEntry]
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ "Checkpoint query failed: " <> T.pack (show err)
    Right [] -> pure $ Right Nothing
    Right (entry : _) -> pure $ Right (Just entry)

-- | Delete all checkpoints for a thread
deleteCheckpoint :: AegisCheckpointer -> Text -> IO (Either Text ())
deleteCheckpoint cp threadId = do
  eRes <- try $ withDB cp $ \conn ->
    execute conn "DELETE FROM checkpoints WHERE thread_id = ?" (Only threadId)
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ "Checkpoint delete failed: " <> T.pack (show err)
    Right () -> pure $ Right ()

-- ---------------------------------------------------------------------------
-- Database Helpers
-- ---------------------------------------------------------------------------

-- | Execute an action with a database connection
withDB :: AegisCheckpointer -> (Connection -> IO a) -> IO a
withDB cp action = bracket (open (acDbPath cp)) close action

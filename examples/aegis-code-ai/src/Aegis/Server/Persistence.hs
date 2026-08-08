{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Server.Persistence
Description : SQLite persistence layer for scan history and audit trail
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Persistence layer for storing scan history, vulnerability tracking,
approval audit trails, and report archival in SQLite.
-}
module Aegis.Server.Persistence
  ( -- * Persistence Manager
    PersistenceManager (..)
  , newPersistenceManager

    -- * Scan History
  , saveScanHistory
  , loadScanHistory
  , updateScanStatus
  , listScans

    -- * Report Archival
  , saveReport
  , loadReport

    -- * Types
  , ScanHistoryEntry (..)
  ) where

import Control.Exception (try, SomeException, bracket)
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Aegis.Core.Types.Config (DatabaseConfig (..))
import Aegis.Core.Types.Pipeline (AnalysisReport)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A scan history entry
data ScanHistoryEntry = ScanHistoryEntry
  { sheScanId :: Text
  , sheThreadId :: Text
  , sheRepoPath :: Text
  , sheStatus :: Text
  , sheTotalFindings :: Int
  , sheRemediated :: Int
  , sheCreatedAt :: Text
  , sheCompletedAt :: Maybe Text
  }
  deriving (Eq, Show)

instance FromRow ScanHistoryEntry where
  fromRow = ScanHistoryEntry <$> field <*> field <*> field <*> field
            <*> field <*> field <*> field <*> field

-- ---------------------------------------------------------------------------
-- Persistence Manager
-- ---------------------------------------------------------------------------

-- | Persistence manager for SQLite operations
data PersistenceManager = PersistenceManager
  { pmDbPath :: FilePath
  }

-- | Create a new persistence manager and initialize schema
newPersistenceManager :: DatabaseConfig -> IO PersistenceManager
newPersistenceManager dbConfig = do
  let pm = PersistenceManager { pmDbPath = dbFilePath dbConfig }
  initSchema pm
  pure pm

-- | Initialize database schema
initSchema :: PersistenceManager -> IO ()
initSchema pm = withDB pm $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS scan_history (\
    \  scan_id TEXT PRIMARY KEY, \
    \  thread_id TEXT NOT NULL, \
    \  repo_path TEXT NOT NULL, \
    \  status TEXT NOT NULL DEFAULT 'pending', \
    \  total_findings INTEGER DEFAULT 0, \
    \  remediated INTEGER DEFAULT 0, \
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')), \
    \  completed_at TEXT\
    \)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS reports (\
    \  report_id TEXT PRIMARY KEY, \
    \  scan_id TEXT NOT NULL, \
    \  report_json TEXT NOT NULL, \
    \  created_at TEXT NOT NULL DEFAULT (datetime('now'))\
    \)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS audit_log (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  scan_id TEXT NOT NULL, \
    \  action TEXT NOT NULL, \
    \  actor TEXT NOT NULL, \
    \  details TEXT, \
    \  created_at TEXT NOT NULL DEFAULT (datetime('now'))\
    \)"

-- ---------------------------------------------------------------------------
-- Scan History Operations
-- ---------------------------------------------------------------------------

-- | Save a new scan history entry
saveScanHistory :: PersistenceManager -> Text -> Text -> Text -> IO (Either Text ())
saveScanHistory pm scanId threadId repoPath = do
  eRes <- try $ withDB pm $ \conn ->
    execute conn
      "INSERT OR REPLACE INTO scan_history (scan_id, thread_id, repo_path, status) VALUES (?, ?, ?, 'running')"
      (scanId, threadId, repoPath)
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ T.pack (show err)
    Right () -> pure $ Right ()

-- | Load a scan history entry
loadScanHistory :: PersistenceManager -> Text -> IO (Either Text (Maybe ScanHistoryEntry))
loadScanHistory pm scanId = do
  eRes <- try $ withDB pm $ \conn ->
    query conn
      "SELECT scan_id, thread_id, repo_path, status, total_findings, remediated, created_at, completed_at \
      \FROM scan_history WHERE scan_id = ?"
      (Only scanId) :: IO [ScanHistoryEntry]
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ T.pack (show err)
    Right [] -> pure $ Right Nothing
    Right (entry : _) -> pure $ Right (Just entry)

-- | Update scan status
updateScanStatus :: PersistenceManager -> Text -> Text -> Int -> Int -> IO (Either Text ())
updateScanStatus pm scanId status totalFindings remediated = do
  eRes <- try $ withDB pm $ \conn ->
    execute conn
      "UPDATE scan_history SET status = ?, total_findings = ?, remediated = ?, \
      \completed_at = datetime('now') WHERE scan_id = ?"
      (status, totalFindings, remediated, scanId)
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ T.pack (show err)
    Right () -> pure $ Right ()

-- | List all scans
listScans :: PersistenceManager -> IO (Either Text [ScanHistoryEntry])
listScans pm = do
  eRes <- try $ withDB pm $ \conn ->
    query_ conn
      "SELECT scan_id, thread_id, repo_path, status, total_findings, remediated, created_at, completed_at \
      \FROM scan_history ORDER BY created_at DESC" :: IO [ScanHistoryEntry]
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ T.pack (show err)
    Right entries -> pure $ Right entries

-- ---------------------------------------------------------------------------
-- Report Operations
-- ---------------------------------------------------------------------------

-- | Save an analysis report
saveReport :: PersistenceManager -> Text -> AnalysisReport -> IO (Either Text ())
saveReport pm scanId report = do
  let reportJson = TE.decodeUtf8 $ BL.toStrict $ encode report
  eRes <- try $ withDB pm $ \conn ->
    execute conn
      "INSERT OR REPLACE INTO reports (report_id, scan_id, report_json) VALUES (?, ?, ?)"
      (scanId, scanId, reportJson)
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ T.pack (show err)
    Right () -> pure $ Right ()

-- | Load an analysis report
loadReport :: PersistenceManager -> Text -> IO (Either Text (Maybe AnalysisReport))
loadReport pm scanId = do
  eRes <- try $ withDB pm $ \conn ->
    query conn
      "SELECT report_json FROM reports WHERE scan_id = ?"
      (Only scanId) :: IO [Only Text]
  case eRes of
    Left (err :: SomeException) -> pure $ Left $ T.pack (show err)
    Right [] -> pure $ Right Nothing
    Right (Only jsonText : _) ->
      case decode (BL.fromStrict (TE.encodeUtf8 jsonText)) of
        Nothing -> pure $ Left "Failed to deserialize report"
        Just report -> pure $ Right (Just report)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withDB :: PersistenceManager -> (Connection -> IO a) -> IO a
withDB pm action = bracket (open (pmDbPath pm)) close action

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Graph.Checkpointer
Description : Memory and SQLite state checkpointing implementations
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides Checkpointer typeclass, thread-safe MemoryCheckpointer, and persistent SQLiteCheckpointer.
-}
module Langchain.Graph.Checkpointer
  ( Checkpointer (..)
  , MemoryCheckpointer (..)
  , newMemoryCheckpointer
  , SQLiteCheckpointer (..)
  , newSQLiteCheckpointer
  ) where

import Control.Concurrent.STM
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple

import Langchain.Core.Error (LangchainError, internalError)
import Langchain.Graph.StateGraph (NodeId)

-- | Effect-polymorphic Checkpointer typeclass
class Checkpointer cp m where
  saveCheckpoint :: ToJSON s => cp -> Text -> NodeId -> s -> m (Either LangchainError ())
  loadCheckpoint :: FromJSON s => cp -> Text -> NodeId -> m (Either LangchainError (Maybe s))

-- | In-memory thread-safe checkpointer using TVar
newtype MemoryCheckpointer = MemoryCheckpointer
  { memStore :: TVar (Map (Text, NodeId) BL.ByteString)
  }

-- | Construct a new MemoryCheckpointer instance
newMemoryCheckpointer :: MonadIO m => m MemoryCheckpointer
newMemoryCheckpointer = liftIO $ do
  var <- newTVarIO Map.empty
  pure $ MemoryCheckpointer var

instance MonadIO m => Checkpointer MemoryCheckpointer m where
  saveCheckpoint cp threadId nodeId state = liftIO $ do
    let bytes = encode state
    atomically $ modifyTVar' (memStore cp) (Map.insert (threadId, nodeId) bytes)
    pure $ Right ()

  loadCheckpoint cp threadId nodeId = liftIO $ do
    store <- readTVarIO (memStore cp)
    case Map.lookup (threadId, nodeId) store of
      Nothing -> pure $ Right Nothing
      Just bytes -> case decode bytes of
        Nothing ->
          pure $ Left $ internalError "Failed to decode state checkpoint" (Just "MemoryCheckpointer") Nothing
        Just s -> pure $ Right (Just s)

-- | SQLite persistent checkpointer using sqlite-simple
newtype SQLiteCheckpointer = SQLiteCheckpointer
  { dbFilePath :: FilePath
  }

-- | Construct a new SQLiteCheckpointer and initialize database table
newSQLiteCheckpointer :: MonadIO m => FilePath -> m SQLiteCheckpointer
newSQLiteCheckpointer path = liftIO $ do
  conn <- open path
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS checkpoints (thread_id TEXT, node_id TEXT, state TEXT, PRIMARY KEY (thread_id, node_id))"
  close conn
  pure $ SQLiteCheckpointer path

instance MonadIO m => Checkpointer SQLiteCheckpointer m where
  saveCheckpoint cp threadId nodeId state = liftIO $ do
    let stateTxt = TE.decodeUtf8 (BL.toStrict $ encode state)
    eRes <- try $ do
      conn <- open (dbFilePath cp)
      execute
        conn
        "INSERT OR REPLACE INTO checkpoints (thread_id, node_id, state) VALUES (?, ?, ?)"
        (threadId, nodeId, stateTxt)
      close conn
    case eRes of
      Left err ->
        pure $ Left $ internalError (T.pack $ show (err :: SQLError)) (Just "SQLiteCheckpointer") Nothing
      Right () -> pure $ Right ()

  loadCheckpoint cp threadId nodeId = liftIO $ do
    eRes <- try $ do
      conn <- open (dbFilePath cp)
      rows <-
        query conn "SELECT state FROM checkpoints WHERE thread_id = ? AND node_id = ?" (threadId, nodeId) ::
          IO [[Text]]
      close conn
      pure rows
    case eRes of
      Left err ->
        pure $ Left $ internalError (T.pack $ show (err :: SQLError)) (Just "SQLiteCheckpointer") Nothing
      Right [[stateTxt]] -> case decode (BL.fromStrict $ TE.encodeUtf8 stateTxt) of
        Nothing ->
          pure $
            Left $
              internalError "Failed to decode SQLite checkpoint JSON" (Just "SQLiteCheckpointer") Nothing
        Just s -> pure $ Right (Just s)
      _ -> pure $ Right Nothing

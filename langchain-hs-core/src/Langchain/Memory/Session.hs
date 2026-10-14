{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Memory.Session
Description : Multi-session conversation memory management with TTL eviction
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Thread-safe multi-session conversation store.
Provides per-session memory isolation for web servers, chatbots, and multi-tenant agents
with configurable Time-To-Live (TTL) expiration to avoid unbounded memory leaks.
-}
module Langchain.Memory.Session
  ( SessionId
  , SessionMemoryStore (..)
  , newSessionMemoryStore
  , newSessionMemoryStoreWithTTL
  , getSessionMemory
  , listSessions
  , hasSession
  , deleteSession
  , clearAllSessions
  , sessionCount
  , cleanupExpiredSessions
  , exportAllSessions
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Langchain.Model (Message)

-- | Unique identifier for a conversation session (e.g. user ID, session token, or UUID)
type SessionId = Text

-- | Thread-safe multi-session memory repository
data SessionMemoryStore mem = SessionMemoryStore
  { storeVar :: !(TVar (Map SessionId (mem, UTCTime)))
  , memFactory :: !(SessionId -> IO mem)
  , sessionTTL :: !(Maybe NominalDiffTime)
  }

-- | Create a new SessionMemoryStore with no TTL (sessions persist until explicitly deleted)
newSessionMemoryStore ::
  MonadIO m =>
  -- | Factory creating a fresh memory instance for a given SessionId
  (SessionId -> IO mem) ->
  m (SessionMemoryStore mem)
newSessionMemoryStore factory = liftIO $ do
  var <- newTVarIO Map.empty
  pure $ SessionMemoryStore var factory Nothing

-- | Create a new SessionMemoryStore with automatic Time-To-Live expiration
newSessionMemoryStoreWithTTL ::
  MonadIO m =>
  -- | Maximum idle time before session is considered expired
  NominalDiffTime ->
  -- | Factory creating a fresh memory instance for a given SessionId
  (SessionId -> IO mem) ->
  m (SessionMemoryStore mem)
newSessionMemoryStoreWithTTL ttl factory = liftIO $ do
  var <- newTVarIO Map.empty
  pure $ SessionMemoryStore var factory (Just ttl)

-- | Retrieve (or create) the memory instance for a specific session, updating its last access timestamp
getSessionMemory ::
  MonadIO m =>
  SessionMemoryStore mem ->
  SessionId ->
  m mem
getSessionMemory SessionMemoryStore {..} sId = liftIO $ do
  now <- getCurrentTime
  mExisting <- atomically $ do
    m <- readTVar storeVar
    case Map.lookup sId m of
      Just (mem, lastAccess) ->
        case sessionTTL of
          Just ttl | diffUTCTime now lastAccess > ttl -> pure Nothing
          _ -> do
            modifyTVar' storeVar (Map.insert sId (mem, now))
            pure (Just mem)
      Nothing -> pure Nothing

  case mExisting of
    Just mem -> pure mem
    Nothing -> do
      newMem <- memFactory sId
      atomically $ modifyTVar' storeVar (Map.insert sId (newMem, now))
      pure newMem

-- | List all active session IDs
listSessions :: MonadIO m => SessionMemoryStore mem -> m [SessionId]
listSessions SessionMemoryStore {..} = liftIO $ do
  m <- readTVarIO storeVar
  pure $ Map.keys m

-- | Check if a session exists and is not expired
hasSession :: MonadIO m => SessionMemoryStore mem -> SessionId -> m Bool
hasSession SessionMemoryStore {..} sId = liftIO $ do
  now <- getCurrentTime
  m <- readTVarIO storeVar
  pure $ case Map.lookup sId m of
    Just (_, lastAccess) ->
      case sessionTTL of
        Just ttl -> diffUTCTime now lastAccess <= ttl
        Nothing -> True
    Nothing -> False

-- | Delete a specific session from the store
deleteSession :: MonadIO m => SessionMemoryStore mem -> SessionId -> m ()
deleteSession SessionMemoryStore {..} sId = liftIO $ atomically $ do
  modifyTVar' storeVar (Map.delete sId)

-- | Remove all sessions from the store
clearAllSessions :: MonadIO m => SessionMemoryStore mem -> m ()
clearAllSessions SessionMemoryStore {..} = liftIO $ atomically $ do
  writeTVar storeVar Map.empty

-- | Count active sessions
sessionCount :: MonadIO m => SessionMemoryStore mem -> m Int
sessionCount SessionMemoryStore {..} = liftIO $ do
  m <- readTVarIO storeVar
  pure $ Map.size m

-- | Purge all sessions that have exceeded the TTL limit. Returns the number of evicted sessions.
cleanupExpiredSessions :: MonadIO m => SessionMemoryStore mem -> m Int
cleanupExpiredSessions SessionMemoryStore {..} = liftIO $ case sessionTTL of
  Nothing -> pure 0
  Just ttl -> do
    now <- getCurrentTime
    atomically $ do
      m <- readTVar storeVar
      let (active, expired) = Map.partition (\(_, lastAccess) -> diffUTCTime now lastAccess <= ttl) m
      writeTVar storeVar active
      pure (Map.size expired)

-- | Export all sessions' message histories
exportAllSessions ::
  MonadIO m =>
  (mem -> m [Message]) ->
  SessionMemoryStore mem ->
  m (Map SessionId [Message])
exportAllSessions getMsgs SessionMemoryStore {..} = do
  sMap <- liftIO $ readTVarIO storeVar
  Map.traverseWithKey (\_ (mem, _) -> getMsgs mem) sMap

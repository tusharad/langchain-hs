{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Cache.Core
Description : LLM response caching layer with in-memory and SQLite backends
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Transparent caching for ChatModel invocations to reduce latency, cost, and API usage.
-}
module Langchain.Cache.Core
  ( CacheBackend (..)
  , InMemoryCache (..)
  , newInMemoryCache
  , SQLiteCache (..)
  , newSQLiteCache
  , CachedModel (..)
  , withCaching
  , computeCacheKey
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple

import Langchain.Core.Model
  ( ChatModel (..)
  , Message (..)
  , extractMessageText
  )

-- | Effect-polymorphic cache backend typeclass
class CacheBackend cb where
  getCache :: MonadIO m => cb -> Text -> m (Maybe Message)
  putCache :: MonadIO m => cb -> Text -> Message -> m ()
  clearCache :: MonadIO m => cb -> m ()

-- | Thread-safe in-memory cache backed by STM TVar
newtype InMemoryCache = InMemoryCache
  { memCacheVar :: TVar (Map Text Message)
  }

-- | Construct a new InMemoryCache
newInMemoryCache :: MonadIO m => m InMemoryCache
newInMemoryCache = liftIO $ do
  var <- newTVarIO Map.empty
  pure $ InMemoryCache var

instance CacheBackend InMemoryCache where
  getCache InMemoryCache {..} key = liftIO $ do
    m <- readTVarIO memCacheVar
    pure $ Map.lookup key m

  putCache InMemoryCache {..} key msg = liftIO $ do
    atomically $ modifyTVar' memCacheVar (Map.insert key msg)

  clearCache InMemoryCache {..} = liftIO $ do
    atomically $ writeTVar memCacheVar Map.empty

-- | Persistent SQLite cache backend
newtype SQLiteCache = SQLiteCache
  { sqliteCacheDbPath :: FilePath
  }

-- | Construct a new SQLiteCache and create cache table
newSQLiteCache :: MonadIO m => FilePath -> m SQLiteCache
newSQLiteCache dbPath = liftIO $ do
  _ <-
    ( try $ withConnection dbPath $ \conn -> do
        execute_
          conn
          "CREATE TABLE IF NOT EXISTS langchain_cache (\
          \ cache_key TEXT PRIMARY KEY,\
          \ response_json TEXT NOT NULL,\
          \ created_at DATETIME DEFAULT CURRENT_TIMESTAMP\
          \);"
    ) ::
      IO (Either SomeException ())
  pure $ SQLiteCache dbPath

instance CacheBackend SQLiteCache where
  getCache SQLiteCache {..} key = liftIO $ do
    rowsRes <-
      ( try $ withConnection sqliteCacheDbPath $ \conn -> do
          query conn "SELECT response_json FROM langchain_cache WHERE cache_key = ?" (Only (TS.unpack key)) ::
            IO [Only String]
      ) ::
        IO (Either SomeException [Only String])
    case rowsRes of
      Right [Only jsonStr] ->
        let bs = LBS.fromStrict (TE.encodeUtf8 (TS.pack jsonStr))
         in pure (decode bs)
      _ -> pure Nothing

  putCache SQLiteCache {..} key msg = liftIO $ do
    let jsonStr = TS.unpack $ TE.decodeUtf8 $ LBS.toStrict (encode msg)
    _ <-
      ( try $ withConnection sqliteCacheDbPath $ \conn -> do
          execute
            conn
            "INSERT OR REPLACE INTO langchain_cache (cache_key, response_json) VALUES (?, ?)"
            (TS.unpack key, jsonStr)
      ) ::
        IO (Either SomeException ())
    pure ()

  clearCache SQLiteCache {..} = liftIO $ do
    _ <-
      ( try $ withConnection sqliteCacheDbPath $ \conn -> do
          execute_ conn "DELETE FROM langchain_cache;"
      ) ::
        IO (Either SomeException ())
    pure ()

-- | ChatModel wrapper that provides transparent response caching
data CachedModel model cache = CachedModel
  { underlyingModel :: model
  , modelCache :: cache
  }

-- | Helper to wrap any ChatModel with a CacheBackend
withCaching :: model -> cache -> CachedModel model cache
withCaching = CachedModel

-- | Compute a deterministic cache key from input messages
computeCacheKey :: [Message] -> Text
computeCacheKey msgs =
  TS.intercalate "|" [TS.pack (show (messageRole m)) <> ":" <> extractMessageText m | m <- msgs]

instance (ChatModel model, CacheBackend cache) => ChatModel (CachedModel model cache) where
  type ModelConfig (CachedModel model cache) = ModelConfig model

  invoke CachedModel {..} msgs mbCfg = do
    let key = computeCacheKey msgs
    mbCached <- getCache modelCache key
    case mbCached of
      Just cachedMsg -> pure cachedMsg
      Nothing -> do
        freshMsg <- invoke underlyingModel msgs mbCfg
        putCache modelCache key freshMsg
        pure freshMsg

  stream CachedModel {..} =
    stream underlyingModel

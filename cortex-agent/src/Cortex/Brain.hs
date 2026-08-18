{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Brain
Description : Multi-Tenant Second-Brain Knowledge Management Engine
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides isolated multi-tenant brain instances, each managing dedicated
system prompts, model parameters, vector spaces, and document collections.
-}
module Cortex.Brain
  ( BrainId (..)
  , BrainConfig (..)
  , Brain (..)
  , BrainStore (..)
  , defaultBrainConfig
  , newBrainStore
  , createBrain
  , getBrain
  , listBrains
  , updateBrainConfig
  , deleteBrain
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import GHC.Generics (Generic)

-- | Unique identifier for a Brain
newtype BrainId = BrainId { unBrainId :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON BrainId
instance FromJSON BrainId

-- | Configuration parameters for a Brain
data BrainConfig = BrainConfig
  { brainName :: !Text
  , brainDescription :: !Text
  , brainSystemPrompt :: !Text
  , brainModel :: !Text
  , brainTemperature :: !Double
  , brainMaxTokens :: !Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON BrainConfig
instance FromJSON BrainConfig

-- | Default configuration for a general-purpose research brain
defaultBrainConfig :: Text -> BrainConfig
defaultBrainConfig name =
  BrainConfig
    { brainName = name
    , brainDescription = "A general-purpose research and knowledge synthesis brain."
    , brainSystemPrompt = "You are Cortex, an expert AI research assistant. Provide thorough, evidence-backed answers citing sources."
    , brainModel = "qwen3.5:9b"
    , brainTemperature = 0.7
    , brainMaxTokens = 4096
    }

-- | Brain entity
data Brain = Brain
  { brainId :: !BrainId
  , brainConfig :: !BrainConfig
  , brainCreatedAt :: !UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON Brain
instance FromJSON Brain

-- | Persistent Brain Store handle (backed by SQLite and STM cache)
data BrainStore = BrainStore
  { storeDbPath :: !FilePath
  , storeCache :: !(TVar (Map BrainId Brain))
  }

-- | Initialize a new BrainStore with SQLite persistence
newBrainStore :: FilePath -> IO BrainStore
newBrainStore dbPath = do
  conn <- open dbPath
  execute_ conn
    "CREATE TABLE IF NOT EXISTS brains (\
    \  id TEXT PRIMARY KEY,\
    \  config_json TEXT NOT NULL,\
    \  created_at TEXT NOT NULL\
    \);"
  close conn
  cacheVar <- newTVarIO Map.empty
  let store = BrainStore dbPath cacheVar
  _ <- reloadCache store
  pure store

reloadCache :: BrainStore -> IO (Map BrainId Brain)
reloadCache BrainStore {..} = do
  conn <- open storeDbPath
  rows <- query_ conn "SELECT id, config_json, created_at FROM brains;" :: IO [(Text, Text, Text)]
  close conn
  let loaded = Map.fromList
        [ (BrainId bId, Brain (BrainId bId) cfg (read (T.unpack tStr)))
        | (bId, cJson, tStr) <- rows
        , Just cfg <- [decode (LBSC.pack (T.unpack cJson))]
        ]
  atomically $ writeTVar storeCache loaded
  pure loaded

-- | Create and persist a new Brain
createBrain :: BrainStore -> BrainConfig -> IO Brain
createBrain BrainStore {..} cfg = do
  now <- getCurrentTime
  let uniqueId = "brain-" <> T.pack (show (T.length (brainName cfg))) <> "-" <> T.take 8 (brainName cfg)
      bId = BrainId uniqueId
      brain = Brain bId cfg now
      cfgJson = T.pack (LBSC.unpack (encode cfg))
      nowStr = T.pack (show now)

  conn <- open storeDbPath
  execute conn "INSERT OR REPLACE INTO brains (id, config_json, created_at) VALUES (?, ?, ?);"
    (unBrainId bId, cfgJson, nowStr)
  close conn

  atomically $ do
    c <- readTVar storeCache
    writeTVar storeCache (Map.insert bId brain c)
  pure brain

-- | Retrieve a Brain by ID
getBrain :: BrainStore -> BrainId -> IO (Maybe Brain)
getBrain BrainStore {..} bId = do
  c <- atomically $ readTVar storeCache
  pure $ Map.lookup bId c

-- | List all active Brains
listBrains :: BrainStore -> IO [Brain]
listBrains BrainStore {..} = do
  c <- atomically $ readTVar storeCache
  pure $ Map.elems c

-- | Update a Brain's configuration
updateBrainConfig :: BrainStore -> BrainId -> BrainConfig -> IO (Maybe Brain)
updateBrainConfig store@BrainStore {..} bId newCfg = do
  mbB <- getBrain store bId
  case mbB of
    Nothing -> pure Nothing
    Just existing -> do
      let updated = existing { brainConfig = newCfg }
          cfgJson = T.pack (LBSC.unpack (encode newCfg))
      conn <- open storeDbPath
      execute conn "UPDATE brains SET config_json = ? WHERE id = ?;" (cfgJson, unBrainId bId)
      close conn
      atomically $ do
        c <- readTVar storeCache
        writeTVar storeCache (Map.insert bId updated c)
      pure (Just updated)

-- | Delete a Brain
deleteBrain :: BrainStore -> BrainId -> IO Bool
deleteBrain BrainStore {..} bId = do
  conn <- open storeDbPath
  execute conn "DELETE FROM brains WHERE id = ?;" (Only (unBrainId bId))
  close conn
  atomically $ do
    c <- readTVar storeCache
    writeTVar storeCache (Map.delete bId c)
  pure True

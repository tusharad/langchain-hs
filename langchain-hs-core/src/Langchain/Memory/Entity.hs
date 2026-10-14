{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Memory.Entity
Description : Structured key-value and entity fact memory
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Thread-safe entity and fact memory.
Retains user attributes, preferences, and extracted facts across conversation turns
for injection into prompt variables and system messages.
-}
module Langchain.Memory.Entity
  ( EntityMemory (..)
  , newEntityMemory
  , setEntity
  , getEntity
  , getAllEntities
  , deleteEntity
  , clearEntities
  , formatEntities
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Model (systemMessage)

-- | Entity memory holding key-value facts and attributes in STM
data EntityMemory = EntityMemory
  { entityVar :: !(TVar (Map Text Text))
  , initialEntities :: !(Map Text Text)
  }

-- | Construct a new EntityMemory with optional initial facts
newEntityMemory :: MonadIO m => Map Text Text -> m EntityMemory
newEntityMemory inits = liftIO $ do
  var <- newTVarIO inits
  pure $ EntityMemory var inits

-- | Set or update a specific entity attribute
setEntity :: MonadIO m => EntityMemory -> Text -> Text -> m ()
setEntity EntityMemory {..} key val = liftIO $ atomically $ do
  modifyTVar' entityVar (Map.insert key val)

-- | Look up a specific entity attribute
getEntity :: MonadIO m => EntityMemory -> Text -> m (Maybe Text)
getEntity EntityMemory {..} key = liftIO $ do
  m <- readTVarIO entityVar
  pure $ Map.lookup key m

-- | Retrieve all current entity facts
getAllEntities :: MonadIO m => EntityMemory -> m (Map Text Text)
getAllEntities EntityMemory {..} = liftIO $ readTVarIO entityVar

-- | Delete an entity attribute
deleteEntity :: MonadIO m => EntityMemory -> Text -> m ()
deleteEntity EntityMemory {..} key = liftIO $ atomically $ do
  modifyTVar' entityVar (Map.delete key)

-- | Clear all entity attributes
clearEntities :: MonadIO m => EntityMemory -> m ()
clearEntities EntityMemory {..} = liftIO $ atomically $ do
  writeTVar entityVar Map.empty

-- | Format key-value entities into bullet points
formatEntities :: Map Text Text -> Text
formatEntities m
  | Map.null m = ""
  | otherwise =
      T.intercalate "\n" ["- " <> k <> ": " <> v | (k, v) <- Map.toList m]

instance BaseMemory EntityMemory where
  messages EntityMemory {..} = liftIO $ do
    m <- readTVarIO entityVar
    if Map.null m
      then pure []
      else pure [systemMessage ("Relevant context facts:\n" <> formatEntities m)]

  addMessage _ _ = pure ()

  clear = clearEntities

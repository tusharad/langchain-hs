{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Memory.Entity
Description : Entity extraction and tracking conversation memory
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Extracts and tracks key named entities and facts across multi-turn conversations.
-}
module Langchain.Memory.Entity
  ( EntityMemory (..)
  , newEntityMemory
  , getEntities
  , setEntity
  ) where

import Control.Concurrent.STM
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , Message (..)
  , Role (..)
  , assistantMessage
  , extractMessageText
  , systemMessage
  , userMessage
  )
import Langchain.Memory.Core (BaseMemory (..))

-- | Entity tracking memory backed by STM TVars
data EntityMemory model = EntityMemory
  { entityModel :: model
  , entityStoreVar :: !(TVar (Map Text Text))
  , entityMessagesVar :: !(TVar [Message])
  }

-- | Construct a new EntityMemory instance
newEntityMemory :: MonadIO m => model -> [Message] -> m (EntityMemory model)
newEntityMemory model initMsgs = liftIO $ do
  eVar <- newTVarIO Map.empty
  mVar <- newTVarIO initMsgs
  pure $ EntityMemory model eVar mVar

-- | Retrieve all currently tracked entities
getEntities :: MonadIO m => EntityMemory model -> m (Map Text Text)
getEntities EntityMemory {..} = liftIO $ readTVarIO entityStoreVar

-- | Manually set or update an entity definition
setEntity :: MonadIO m => EntityMemory model -> Text -> Text -> m ()
setEntity EntityMemory {..} k v =
  liftIO $ atomically $ modifyTVar' entityStoreVar (Map.insert k v)

instance (ChatModel model) => BaseMemory (EntityMemory model) where
  messages EntityMemory {..} = liftIO $ do
    entities <- readTVarIO entityStoreVar
    msgs <- readTVarIO entityMessagesVar
    if Map.null entities
      then pure msgs
      else
        let entityCtx =
              "Known Entities & Context:\n"
                <> T.unlines ["- " <> k <> ": " <> v | (k, v) <- Map.toList entities]
         in pure (systemMessage entityCtx : msgs)

  addMessage mem@EntityMemory {..} newMsg = do
    liftIO $ atomically $ modifyTVar' entityMessagesVar (\msgs -> msgs ++ [newMsg])
    -- If user message, prompt entityModel to extract any entities
    if messageRole newMsg == User
      then do
        let prompt =
              "Extract any key entities, topics, or facts mentioned in this message in the format 'Entity: Description'.\n"
                <> "Message: "
                <> extractMessageText newMsg
        resp <- invoke entityModel [userMessage prompt] Nothing
        let extracted = parseEntityLines (extractMessageText resp)
        liftIO $ atomically $ modifyTVar' entityStoreVar (\m -> Map.union (Map.fromList extracted) m)
      else pure ()

  clear EntityMemory {..} = liftIO $ atomically $ do
    writeTVar entityStoreVar Map.empty
    writeTVar entityMessagesVar []

parseEntityLines :: Text -> [(Text, Text)]
parseEntityLines txt =
  [ (T.strip (T.dropAround (`elem` ['*', '-', ' ']) k), T.strip v)
  | line <- T.lines txt
  , let (k, rest) = T.breakOn ":" line
  , not (T.null rest)
  , let v = T.drop 1 rest
  , not (T.null (T.strip k)) && not (T.null (T.strip v))
  ]

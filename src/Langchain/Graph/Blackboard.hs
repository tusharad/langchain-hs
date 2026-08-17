{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.Blackboard
Description : Blackboard architecture multi-agent pattern with shared STM knowledge base
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Shared workspace (blackboard) pattern where specialist knowledge sources asynchronously inspect,
read, and write partial solutions until a coordinator determines the overall problem is solved.
-}
module Langchain.Graph.Blackboard
  ( Blackboard (..)
  , KnowledgeSource (..)
  , BlackboardConfig (..)
  , newBlackboard
  , readBlackboard
  , writeBlackboard
  , runBlackboard
  ) where

import Control.Concurrent.STM
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, agentError)

-- | Shared in-memory blackboard backed by STM TVar
newtype Blackboard = Blackboard
  { blackboardVar :: TVar (Map Text Text)
  }

-- | Specialist knowledge source agent
data KnowledgeSource m = KnowledgeSource
  { ksName :: !Text
  , ksCanContribute :: Map Text Text -> Bool
  , ksExecute :: Map Text Text -> m (Map Text Text) -- returns updates to write
  }

-- | Blackboard execution configuration
data BlackboardConfig = BlackboardConfig
  { maxIterations :: Int
  , isComplete :: Map Text Text -> Bool
  }

-- | Construct a new Blackboard
newBlackboard :: MonadIO m => [(Text, Text)] -> m Blackboard
newBlackboard initData = liftIO $ do
  var <- newTVarIO (Map.fromList initData)
  pure $ Blackboard var

-- | Read the current state of the blackboard
readBlackboard :: MonadIO m => Blackboard -> m (Map Text Text)
readBlackboard Blackboard {..} = liftIO $ readTVarIO blackboardVar

-- | Write/update key-value entries into the blackboard
writeBlackboard :: MonadIO m => Blackboard -> Map Text Text -> m ()
writeBlackboard Blackboard {..} updates =
  liftIO $ atomically $ modifyTVar' blackboardVar (\current -> Map.union updates current)

-- | Run the blackboard agent loop until isComplete or maxIterations
runBlackboard
  :: (MonadIO m, MonadError LangchainError m)
  => Blackboard
  -> [KnowledgeSource m]
  -> BlackboardConfig
  -> m (Map Text Text)
runBlackboard bb sources BlackboardConfig {..} = loop 1
  where
    loop iter
      | iter > maxIterations = readBlackboard bb
      | otherwise = do
          currState <- readBlackboard bb
          if isComplete currState
            then pure currState
            else do
              let eligibleSources = filter (\ks -> ksCanContribute ks currState) sources
              if null eligibleSources
                then pure currState -- No more contributions can be made
                else do
                  updatesList <- mapM (\ks -> ksExecute ks currState) eligibleSources
                  let combinedUpdates = Map.unions updatesList
                  writeBlackboard bb combinedUpdates
                  loop (iter + 1)

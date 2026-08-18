{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Chain.Sequential
Description : Sequential variable-threaded chain execution
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Passes variable maps through an ordered sequence of transformation steps.
-}
module Langchain.Chain.Sequential
  ( ChainStep (..)
  , SequentialChain (..)
  , newSequentialChain
  , runSequentialChain
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)

-- | A single transformation step in a sequential chain
data ChainStep m = ChainStep
  { stepName :: Text
  , stepAction :: Map Text Text -> m (Map Text Text)
  }

-- | Sequential chain composed of an ordered list of steps
data SequentialChain m = SequentialChain
  { chainSteps :: [ChainStep m]
  }

-- | Construct a new SequentialChain
newSequentialChain :: [ChainStep m] -> SequentialChain m
newSequentialChain = SequentialChain

-- | Execute sequential chain passing and accumulating variables through all steps
runSequentialChain ::
  (MonadIO m, MonadError LangchainError m) =>
  SequentialChain m ->
  Map Text Text ->
  m (Map Text Text)
runSequentialChain SequentialChain {..} initVars = go chainSteps initVars
  where
    go [] vars = pure vars
    go (step : rest) vars = do
      outVars <- stepAction step vars
      let mergedVars = Map.union outVars vars
      go rest mergedVars

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Core.Model
Description : Effect-polymorphic ChatModel typeclass and multi-modal Message model
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Defines the effect-polymorphic ChatModel typeclass and re-exports multi-modal Message types.
-}
module Langchain.Core.Model
  ( -- * Re-exported Message Types
    module Langchain.Core.Model.Types

    -- * Effect-Polymorphic ChatModel
  , ChatModel (..)
  , MockModel (..)
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Conduit (ConduitT, yield)
import Data.Text (Text)

import Data.Kind (Type)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model.Types
import Langchain.Core.Stream (StreamEvent (..))

-- | Unified effect-polymorphic chat model typeclass.
-- Parameterized over monad 'm' (no hardcoded IO).
class ChatModel model where
  type ModelConfig model :: Type

  -- | Single synchronous invocation
  invoke
    :: (MonadIO m, MonadError LangchainError m)
    => model
    -> [Message]
    -> Maybe (ModelConfig model)
    -> m Message

  -- | Batch invocations (default: sequential)
  batch
    :: (MonadIO m, MonadError LangchainError m)
    => model
    -> [[Message]]
    -> Maybe (ModelConfig model)
    -> m [Message]
  batch model msgs cfg = mapM (\m -> invoke model m cfg) msgs

  -- | Streaming invocation yielding structured StreamEvents via Conduit
  stream
    :: (MonadIO m, MonadError LangchainError m)
    => model
    -> [Message]
    -> Maybe (ModelConfig model)
    -> ConduitT () StreamEvent m ()

-- | Mock model implementation for pure monadic testing.
data MockModel = MockModel
  { mockResponse :: Text
  , mockModelName :: Text
  }
  deriving (Eq, Show)

instance ChatModel MockModel where
  type ModelConfig MockModel = ()

  invoke model _ _ = pure $ assistantMessage (mockResponse model)

  stream model inputMsgs _ = do
    let rId = "mock-run-id"
        resText = mockResponse model
        finalMsg = assistantMessage resText
    yield $ LLMStart rId (mockModelName model) inputMsgs
    yield $ LLMChunk rId resText Nothing
    yield $ LLMEnd rId finalMsg Nothing

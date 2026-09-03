{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Core.Model
Description : Central ChatModel typeclass and MockModel provider for pure testing
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides effect-polymorphic 'ChatModel' interface and 'MockModel'.
-}
module Langchain.Core.Model
  ( ChatModel (..)
  , MockModel (..)
  , newMockModel
  , module Langchain.Core.Model.Types
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Conduit (yield)
import Data.Kind (Type)
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model.Types
import Langchain.Core.Stream (ChatStream, StreamEvent (..))

-- | Effect-polymorphic ChatModel typeclass for LLM providers
class ChatModel model where
  type ModelConfig model :: Type

  -- | Single synchronous invocation
  invoke ::
    (MonadIO m, MonadError LangchainError m) =>
    model ->
    [Message] ->
    Maybe (ModelConfig model) ->
    m Message

  -- | Batch invocations (default: sequential)
  batch ::
    (MonadIO m, MonadError LangchainError m) =>
    model ->
    [[Message]] ->
    Maybe (ModelConfig model) ->
    m [Message]
  batch model msgs cfg = mapM (\m -> invoke model m cfg) msgs

  -- | Streaming invocation yielding structured StreamEvents via Conduit
  stream ::
    model ->
    [Message] ->
    Maybe (ModelConfig model) ->
    ChatStream

-- | Mock model implementation for pure monadic testing.
data MockModel = MockModel
  { mockResponse :: Text
  , mockModelName :: Text
  }
  deriving (Eq, Show)

-- | Construct a MockModel with a default model name
newMockModel :: Text -> MockModel
newMockModel resp = MockModel resp "mock-model"

instance ChatModel MockModel where
  type ModelConfig MockModel = ()

  invoke model _ _ = pure $ assistantMessage (mockResponse model)

  stream model inputMsgs _ = do
    let rId = "mock-run-id"
    yield $ LLMStart rId (mockModelName model) inputMsgs
    yield $ LLMChunk rId (mockResponse model) Nothing
    yield $ LLMEnd rId (assistantMessage $ mockResponse model) Nothing

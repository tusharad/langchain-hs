{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Core.Model
Description : Central ChatModel typeclass for LLM providers
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides effect-polymorphic 'ChatModel' interface.
-}
module Langchain.Core.Model
  ( ChatModel (..)
  , module Langchain.Core.Model.Types
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model.Types
import Langchain.Core.Stream (ChatStream)

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

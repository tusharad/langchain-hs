{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Provider.Mock
Description : Mock chat model provider for testing and deterministic evaluation
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides a purely in-memory 'MockModel' implementing 'ChatModel' for testing and offline workflows.
-}
module Langchain.Provider.Mock
  ( MockModel (..)
  , newMockModel
  ) where

import Data.Conduit (yield)
import Data.Text (Text)

import Langchain.Core.Model
  ( ChatModel (..)
  , assistantMessage
  )
import Langchain.Core.Stream (StreamEvent (..))

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

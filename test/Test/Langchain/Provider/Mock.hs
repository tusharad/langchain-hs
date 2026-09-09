{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Test.Langchain.Provider.Mock
Description : Mock chat model provider for testing and deterministic evaluation
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides a purely in-memory 'MockModel' implementing 'ChatModel' for testing and offline workflows.
-}
module Test.Langchain.Provider.Mock
  ( MockModel (..)
  , newMockModel
  ) where

import Data.Aeson (object, (.=))
import Data.Conduit (yield)
import Data.Text (Text)

import Langchain.Cache.Core (CacheableChatModel (..))
import Langchain.Core.Model
  ( ChatModel (..)
  , assistantMessage
  )
import Langchain.Core.Stream (StreamEvent (..))
import Langchain.Tool.Binding (ToolBinder (..))

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

instance ToolBinder MockModel m where
  bindToolsConfig _ _ = Nothing

instance CacheableChatModel MockModel where
  cacheModelIdentity (MockModel response mName) _ =
    object
      [ "provider" .= ("mock" :: Text)
      , "model" .= mName
      , "response" .= response
      ]

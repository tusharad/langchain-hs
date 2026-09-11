{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Core.TestModel
  ( TestChatModel (..)
  ) where

import Data.Conduit (yield)
import Data.Text (Text)

import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..))

data TestChatModel = TestChatModel
  { testResponse :: Text
  , testModelName :: Text
  }
  deriving (Eq, Show)

instance ChatModel TestChatModel where
  type ModelConfig TestChatModel = ()

  invoke model _ _ = pure $ assistantMessage (testResponse model)

  stream model inputMsgs _ = do
    let rId = "test-run-id"
    yield $ LLMStart rId (testModelName model) inputMsgs
    yield $ LLMChunk rId (testResponse model) Nothing
    yield $ LLMEnd rId (assistantMessage $ testResponse model) Nothing

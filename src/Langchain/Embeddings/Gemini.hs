{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Embeddings.Gemini
Description : Gemini integration for text embeddings in LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Gemini implementation of LangChain's embedding interface. Supports document and query
embedding generation through Gemini's OpenAI compatible API.
Checkout docs here: https://ai.google.dev/gemini-api/docs/openai#embeddings
-}
module Langchain.Embeddings.Gemini
  ( -- * Types
    GeminiEmbeddings (..)
  , defaultGeminiEmbeddings
  , module Langchain.Embeddings.Core
  ) where

import Data.Text (Text, unpack)
import GHC.Generics
import Langchain.Embeddings.Core
import Langchain.Embeddings.OpenAI

data GeminiEmbeddings = GeminiEmbeddings
  { apiKey :: Text
  -- ^ OpenAI API Key
  , baseUrl :: Maybe String
  -- ^ base url; default "https://generativelanguage.googleapis.com/v1beta/openai"
  , model :: Text
  -- ^ Model name for embeddings
  , dimensions :: Maybe Int
  -- ^ The number of dimensions the resulting output embeddings should have.
  , encodingFormat :: Maybe EncodingFormat
  {- ^ The format to return the embeddings in.
  ^ For now, only float is supported
  -}
  , embeddingsUser :: Maybe Text
  -- ^ A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
  , timeout :: Maybe Int
  -- ^ Override default responsetime out. unit = seconds.
  }
  deriving (Eq, Generic)

instance Show GeminiEmbeddings where
  show GeminiEmbeddings {..} = "GeminiEmbeddings " <> "model " <> unpack model

-- | Default values GeminiEmbeddings, api-key is empty
defaultGeminiEmbeddings :: GeminiEmbeddings
defaultGeminiEmbeddings =
  GeminiEmbeddings
    { apiKey = ""
    , baseUrl = pure "https://generativelanguage.googleapis.com/v1beta/openai"
    , model = "gemini-embedding-001"
    , dimensions = Nothing
    , encodingFormat = Nothing
    , embeddingsUser = Nothing
    , timeout = Nothing
    }

instance Embeddings GeminiEmbeddings where
  embedDocuments GeminiEmbeddings {..} = embedDocuments OpenAIEmbeddings {..}
  embedQuery GeminiEmbeddings {..} = embedQuery OpenAIEmbeddings {..}

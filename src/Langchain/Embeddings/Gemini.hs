{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Embeddings.Gemini
Description : Gemini integration for text embeddings in LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Gemini implementation of LangChain's embedding interface.
-}
module Langchain.Embeddings.Gemini
  ( GeminiEmbeddings (..)
  , defaultGeminiEmbeddings
  ) where

import Data.Text (Text, unpack)
import GHC.Generics
import Langchain.Embeddings.Core
import Langchain.Embeddings.OpenAI

data GeminiEmbeddings = GeminiEmbeddings
  { apiKey :: Text
  , baseUrl :: Maybe String
  , model :: Text
  , dimensions :: Maybe Int
  , encodingFormat :: Maybe EncodingFormat
  , timeout :: Maybe Int
  }
  deriving (Eq, Generic)

instance Show GeminiEmbeddings where
  show GeminiEmbeddings {..} = "GeminiEmbeddings " <> "model " <> unpack model

defaultGeminiEmbeddings :: GeminiEmbeddings
defaultGeminiEmbeddings =
  GeminiEmbeddings
    { apiKey = ""
    , baseUrl = pure "https://generativelanguage.googleapis.com/v1beta/openai"
    , model = "gemini-embedding-001"
    , dimensions = Nothing
    , encodingFormat = Nothing
    , timeout = Nothing
    }

instance Embeddings GeminiEmbeddings where
  embedDocuments GeminiEmbeddings {..} = embedDocuments OpenAIEmbeddings {..}
  embedQuery GeminiEmbeddings {..} = embedQuery OpenAIEmbeddings {..}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Embeddings.Gemini
Description : Google Gemini integration for text embeddings
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Gemini implementation of LangChain's 'Embeddings' interface using Servant bindings.
-}
module Langchain.Embeddings.Gemini
  ( GeminiEmbeddings (..)
  , defaultGeminiEmbeddings
  , newGeminiEmbeddings
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)

import Gemini.Client
  ( BatchEmbedContentsRequest (..)
  , BatchEmbedContentsResponse (..)
  , BatchEmbedRequestItem (..)
  , EmbedContentPayload (..)
  , EmbedContentRequest (..)
  , EmbedContentResponse (..)
  , EmbedPart (..)
  , EmbeddingValues (..)
  , batchEmbedContents
  , embedContent
  , newGeminiClientEnv
  , newGeminiClientEnvWithBaseUrl
  , runGeminiClient
  )
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.Error (llmError)

-- | Configuration for Gemini embeddings.
data GeminiEmbeddings = GeminiEmbeddings
  { embeddingApiKey :: !Text
  -- ^ Gemini API key
  , embeddingModel :: !Text
  -- ^ Model name, e.g. @"gemini-embedding-001"@
  , embeddingBaseUrl :: !(Maybe Text)
  -- ^ Optional custom base URL
  }
  deriving (Eq, Show, Generic)

-- | Default embedding configuration using @gemini-embedding-001@.
defaultGeminiEmbeddings :: Text -> GeminiEmbeddings
defaultGeminiEmbeddings key = GeminiEmbeddings key "gemini-embedding-001" Nothing

-- | Create a custom Gemini embedding configuration.
newGeminiEmbeddings :: Text -> Text -> Maybe Text -> GeminiEmbeddings
newGeminiEmbeddings = GeminiEmbeddings

instance Embeddings GeminiEmbeddings where
  embedDocuments _ [] = pure []
  embedDocuments GeminiEmbeddings {..} docs = do
    env <- liftIO $ case embeddingBaseUrl of
      Just u -> newGeminiClientEnvWithBaseUrl u embeddingApiKey
      Nothing -> newGeminiClientEnv embeddingApiKey
    let items =
          [ BatchEmbedRequestItem
              { itemModel = "models/" <> embeddingModel
              , itemContent = EmbedContentPayload [EmbedPart (TL.toStrict (pageContent doc))]
              }
          | doc <- docs
          ]
        req = BatchEmbedContentsRequest items
    eRes <- liftIO $ runGeminiClient env (batchEmbedContents embeddingApiKey embeddingModel req)
    case eRes of
      Left err ->
        throwError $
          llmError (T.pack (show err)) (Just "GeminiEmbeddings") Nothing
      Right resp ->
        pure [embeddingValues ev | ev <- batchEmbeddings resp]

  embedQuery GeminiEmbeddings {..} query = do
    env <- liftIO $ case embeddingBaseUrl of
      Just u -> newGeminiClientEnvWithBaseUrl u embeddingApiKey
      Nothing -> newGeminiClientEnv embeddingApiKey
    let req = EmbedContentRequest (EmbedContentPayload [EmbedPart query])
    eRes <- liftIO $ runGeminiClient env (embedContent embeddingApiKey embeddingModel req)
    case eRes of
      Left err ->
        throwError $
          llmError (T.pack (show err)) (Just "GeminiEmbeddings") Nothing
      Right resp ->
        pure $ embeddingValues (responseEmbedding resp)

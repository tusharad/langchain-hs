{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Gemini.Client.API
Description : Servant API definition for Google Gemini Interactions v1 & Embeddings
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Servant type definitions describing the Gemini Interactions v1 API endpoints
and embedding operations according to the Google GenAI OpenAPI specification.
-}
module Gemini.Client.API
  ( GeminiAPI
  , geminiApi
  ) where

import Data.Aeson (Value)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API

import Gemini.Client.Types

-- | Complete Servant API specification for Gemini Interactions v1 & Embeddings.
type GeminiAPI =
  -- POST /v1/interactions
  "v1"
    :> "interactions"
    :> Header "x-goog-api-key" Text
    :> ReqBody '[JSON] CreateInteractionRequest
    :> Post '[JSON] Interaction
    :<|>
    -- GET /v1/interactions/:id
    "v1"
      :> "interactions"
      :> Capture "id" Text
      :> Header "x-goog-api-key" Text
      :> QueryParam "stream" Bool
      :> QueryParam "last_event_id" Text
      :> Get '[JSON] Interaction
    :<|>
    -- POST /v1/interactions/:id/cancel
    "v1"
      :> "interactions"
      :> Capture "id" Text
      :> "cancel"
      :> Header "x-goog-api-key" Text
      :> Post '[JSON] Interaction
    :<|>
    -- DELETE /v1/interactions/:id
    "v1"
      :> "interactions"
      :> Capture "id" Text
      :> Header "x-goog-api-key" Text
      :> Delete '[JSON] Value
    :<|>
    -- POST /v1/models/:modelAction (e.g. "gemini-embedding-001:embedContent")
    "v1"
      :> "models"
      :> Capture "modelAction" Text
      :> Header "x-goog-api-key" Text
      :> ReqBody '[JSON] EmbedContentRequest
      :> Post '[JSON] EmbedContentResponse
    :<|>
    -- POST /v1/models/:modelAction (e.g. "gemini-embedding-001:batchEmbedContents")
    "v1"
      :> "models"
      :> Capture "modelAction" Text
      :> Header "x-goog-api-key" Text
      :> ReqBody '[JSON] BatchEmbedContentsRequest
      :> Post '[JSON] BatchEmbedContentsResponse

-- | Proxy for 'GeminiAPI'.
geminiApi :: Proxy GeminiAPI
geminiApi = Proxy

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- |
Module      : Gemini.Client
Description : Servant client bindings for Google Gemini Interactions v1 API & Embeddings
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Type-safe HTTP client functions generated directly from the Servant API specification
for the Google Gemini Interactions v1 API (<https://ai.google.dev/api/interactions-api-v1>).
-}
module Gemini.Client
  ( -- * Client Operations
    createInteraction
  , getInteraction
  , cancelInteraction
  , deleteInteraction
  , embedContent
  , batchEmbedContents

    -- * Client Environment
  , module Gemini.Client.Env

    -- * Data Models
  , module Gemini.Client.Types
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Servant.API ((:<|>) (..))
import Servant.Client (ClientM, client)

import Gemini.Client.API (geminiApi)
import Gemini.Client.Env
import Gemini.Client.Types

-- Internal servant client functions derived from the API type
createInteractionInternal
  :<|> getInteractionInternal
  :<|> cancelInteractionInternal
  :<|> deleteInteractionInternal
  :<|> embedContentInternal
  :<|> batchEmbedContentsInternal =
    client geminiApi

-- | Create a new interaction (model-based or agent-based).
createInteraction ::
  -- | Google Gemini API key
  Text ->
  -- | Interaction request payload
  CreateInteractionRequest ->
  ClientM Interaction
createInteraction apiKey = createInteractionInternal (Just apiKey)

-- | Retrieve full details of an interaction by ID.
getInteraction ::
  -- | Google Gemini API key
  Text ->
  -- | Interaction ID
  Text ->
  -- | Optional stream parameter
  Maybe Bool ->
  -- | Optional last event ID
  Maybe Text ->
  ClientM Interaction
getInteraction apiKey interId =
  getInteractionInternal interId (Just apiKey)

-- | Cancel a background interaction by ID.
cancelInteraction ::
  -- | Google Gemini API key
  Text ->
  -- | Interaction ID
  Text ->
  ClientM Interaction
cancelInteraction apiKey interId =
  cancelInteractionInternal interId (Just apiKey)

-- | Delete an interaction by ID.
deleteInteraction ::
  -- | Google Gemini API key
  Text ->
  -- | Interaction ID
  Text ->
  ClientM Value
deleteInteraction apiKey interId =
  deleteInteractionInternal interId (Just apiKey)

-- | Generate embeddings for single content via @POST \/v1\/models\/{model}:embedContent@.
embedContent ::
  -- | Google Gemini API key
  Text ->
  -- | Model name (e.g. @"gemini-embedding-001"@)
  Text ->
  -- | Content payload
  EmbedContentRequest ->
  ClientM EmbedContentResponse
embedContent apiKey modelName req =
  let modelAction = modelName <> ":embedContent"
   in embedContentInternal modelAction (Just apiKey) req

-- | Generate batch embeddings via @POST \/v1\/models\/{model}:batchEmbedContents@.
batchEmbedContents ::
  -- | Google Gemini API key
  Text ->
  -- | Model name (e.g. @"gemini-embedding-001"@)
  Text ->
  -- | Batch request payload
  BatchEmbedContentsRequest ->
  ClientM BatchEmbedContentsResponse
batchEmbedContents apiKey modelName req =
  let modelAction = modelName <> ":batchEmbedContents"
   in batchEmbedContentsInternal modelAction (Just apiKey) req

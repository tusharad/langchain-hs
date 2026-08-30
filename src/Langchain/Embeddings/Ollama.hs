{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Embeddings.Ollama
Description : Ollama integration for text embeddings in LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Ollama implementation of LangChain's embedding interface using ollama-haskell 0.3.0.0.
-}
module Langchain.Embeddings.Ollama
  ( OllamaEmbeddings (..)
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import Langchain.Core.Error (llmError)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core
import Langchain.Utils (showText)

import Ollama.API.Embed (EmbedRequest (..), EmbedResponse (..), embed)
import Ollama.Client (defaultClient)
import Ollama.Types.Common (ModelName (..))
import Ollama.Types.Options (ModelOptions)

data OllamaEmbeddings = OllamaEmbeddings
  { model :: Text
  , defaultTruncate :: Maybe Bool
  , defaultKeepAlive :: Maybe Text
  , modelOptions :: Maybe ModelOptions
  }
  deriving (Show, Eq)

instance Embeddings OllamaEmbeddings where
  embedDocuments (OllamaEmbeddings {..}) docs = do
    client <- liftIO defaultClient
    let inputs = map (T.toStrict . pageContent) docs
        req =
          EmbedRequest
            { embModel = ModelName model
            , embInput = Right inputs
            , embTruncate = defaultTruncate
            , embOptions = modelOptions
            , embKeepAlive = defaultKeepAlive
            , embDimensions = Nothing
            }
    eRes <- liftIO $ embed client req
    case eRes of
      Left ollamaErr -> throwError $ llmError (showText ollamaErr) (Just "OllamaEmbeddings") Nothing
      Right resp -> pure $ map (map realToFrac) (erEmbeddings resp)

  embedQuery (OllamaEmbeddings {..}) query = do
    client <- liftIO defaultClient
    let req =
          EmbedRequest
            { embModel = ModelName model
            , embInput = Left query
            , embTruncate = defaultTruncate
            , embOptions = modelOptions
            , embKeepAlive = defaultKeepAlive
            , embDimensions = Nothing
            }
    eRes <- liftIO $ embed client req
    case eRes of
      Left err -> throwError $ llmError (showText err) (Just "OllamaEmbeddings") Nothing
      Right resp -> case erEmbeddings resp of
        (vec : _) -> pure $ map realToFrac vec
        [] -> throwError $ llmError "Embeddings are empty" (Just "OllamaEmbeddings") Nothing

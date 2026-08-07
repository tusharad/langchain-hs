{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Embeddings.Ollama
Description : Ollama integration for text embeddings in LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Ollama implementation of LangChain's embedding interface using ollama-haskell 0.3.0.0.
-}
module Langchain.Embeddings.Ollama
  ( OllamaEmbeddings (..)
  , module Langchain.DocumentLoader.Core
  ) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import Langchain.DocumentLoader.Core
import Langchain.Embeddings.Core
import Langchain.Error (llmError)
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
    client <- defaultClient
    let inputs = map (T.toStrict . pageContent) docs
        req = EmbedRequest
          { embModel = ModelName model
          , embInput = Right inputs
          , embTruncate = defaultTruncate
          , embOptions = modelOptions
          , embKeepAlive = defaultKeepAlive
          , embDimensions = Nothing
          }
    eRes <- embed client req
    case eRes of
      Left ollamaErr -> return $ Left $ llmError (showText ollamaErr) Nothing Nothing
      Right resp -> return $ Right $ map (map realToFrac) (erEmbeddings resp)

  embedQuery (OllamaEmbeddings {..}) query = do
    client <- defaultClient
    let req = EmbedRequest
          { embModel = ModelName model
          , embInput = Left query
          , embTruncate = defaultTruncate
          , embOptions = modelOptions
          , embKeepAlive = defaultKeepAlive
          , embDimensions = Nothing
          }
    eRes <- embed client req
    case eRes of
      Left err -> pure $ Left (llmError (showText err) Nothing Nothing)
      Right resp -> case erEmbeddings resp of
        (vec : _) -> pure $ Right $ map realToFrac vec
        [] -> pure $ Left (llmError "Embeddings are empty" Nothing Nothing)

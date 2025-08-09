{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.OpenAICompatible
Description : Internal module for OpenAI chat completion API interactions
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the 'OpenAICompatible' data type and
implements the 'LLM' typeclass for interacting with OpenAICompatible APIs.
Also provides some convenience functions for `LMStudio` and LLama.cpp
-}
module Langchain.LLM.OpenAICompatible
  ( OpenAICompatible (..)
  , OpenAI.OpenAIParams (..)
  , toOpenAI
  , mkLMStudio
  , mkLlamaCpp
  , mkOpenRouter
  , OpenAI.defaultOpenAIParams
  , module Langchain.LLM.Core
  ) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Callback
import Langchain.LLM.Core
import qualified Langchain.LLM.Core as LLM
import qualified Langchain.LLM.OpenAI as OpenAI

-- | Generic OpenAICompatible implementation for any service with an OpenAI-compatible API
data OpenAICompatible = OpenAICompatible
  { apiKey :: Maybe Text
  -- ^ The API key for authenticating (optional for some instances)
  , modelName :: Text
  -- ^ The name or identifier of the model being served
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations
  , baseUrl :: Maybe String
  -- ^ Base URL for the service
  , defaultBaseUrl :: String
  -- ^ Default base URL to use when none is provided
  , providerName :: Text
  -- ^ The provider or service name
  }

instance Show OpenAICompatible where
  show OpenAICompatible {..} = show providerName ++ " " ++ T.unpack modelName

-- | Convert OpenAICompatible configuration to OpenAI configuration
toOpenAI :: OpenAICompatible -> OpenAI.OpenAI
toOpenAI OpenAICompatible {..} =
  OpenAI.OpenAI
    { OpenAI.apiKey = fromMaybe "" apiKey
    , OpenAI.openAIModelName = modelName
    , OpenAI.callbacks = callbacks
    , OpenAI.baseUrl = Just $ fromMaybe defaultBaseUrl baseUrl
    }

instance LLM.LLM OpenAICompatible where
  type LLMParams OpenAICompatible = OpenAI.OpenAIParams
  type LLMStreamTokenType OpenAICompatible = Text
  generate model = LLM.generate (toOpenAI model)
  chat model = LLM.chat (toOpenAI model)
  stream model = LLM.stream (toOpenAI model)

-- | Create an LMStudio instance
mkLMStudio :: Text -> [Callback] -> Maybe String -> Maybe Text -> OpenAICompatible
mkLMStudio modelName' callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = apiKey'
    , modelName = modelName'
    , callbacks = callbacks'
    , baseUrl = baseUrl'
    , defaultBaseUrl = "http://localhost:1234/v1"
    , providerName = "LMStudio"
    }

-- | Create a llama.cpp instance
mkLlamaCpp :: Text -> [Callback] -> Maybe String -> Maybe Text -> OpenAICompatible
mkLlamaCpp modelName' callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = apiKey'
    , modelName = modelName'
    , callbacks = callbacks'
    , baseUrl = baseUrl'
    , defaultBaseUrl = "http://localhost:8080/v1"
    , providerName = "LlamaCpp"
    }

{- | Create an OpenRouter instance
OpenRouter provides access to multiple model providers through a single API
Model name should be in the format "provider/model" (e.g., "anthropic/claude-3-opus")
-}
mkOpenRouter :: Text -> [Callback] -> Maybe String -> Text -> OpenAICompatible
mkOpenRouter modelName' callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = Just apiKey' -- OpenRouter requires an API key
    , modelName = modelName'
    , callbacks = callbacks'
    , baseUrl = baseUrl'
    , defaultBaseUrl = "https://openrouter.ai/api/v1"
    , providerName = "OpenRouter"
    }

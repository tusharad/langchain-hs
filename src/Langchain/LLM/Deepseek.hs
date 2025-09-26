{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.OpenAI
Description : Deepseek integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the 'Deepseek' data type and implements the 'LLM' typeclass for interacting with Deepseek's language models.
It supports generating text, handling chat interactions, and streaming responses using Deepseek's API.

This deepseek type uses OpenAI module with baseUrl as "https://api.deepseek.com";
-}
module Langchain.LLM.Deepseek
  ( Deepseek (..)
  , module Langchain.LLM.Core
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Langchain.Callback
import Langchain.LLM.Core
import qualified Langchain.LLM.Core as LLM
import qualified Langchain.LLM.Internal.OpenAI as OpenAI
import qualified Langchain.LLM.OpenAI as OpenAI

data Deepseek = Deepseek
  { apiKey :: Text
  -- ^ The API key for authenticating with OpenAI's services.
  , deepseekModelName :: Text
  -- ^ The name of the Deepseek model to use (e.g., "gpt-3.5-turbo", "gpt-4").
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations.
  , baseUrl :: Maybe String
  -- ^ Base url; default "https://api.deepseek.com"
  }

instance Show Deepseek where
  show Deepseek {..} = "Deepseek " ++ show deepseekModelName

toOpenAI :: Deepseek -> OpenAI.OpenAI
toOpenAI Deepseek {..} =
  OpenAI.OpenAI
    { OpenAI.apiKey = apiKey
    , OpenAI.openAIModelName = deepseekModelName
    , OpenAI.callbacks = callbacks
    , OpenAI.baseUrl = Just $ fromMaybe "https://api.deepseek.com" baseUrl
    }

instance LLM.LLM Deepseek where
  type LLMParams Deepseek = OpenAI.OpenAIParams
  type LLMStreamTokenType Deepseek = OpenAI.ChatCompletionChunk

  generate deepseek = LLM.generate (toOpenAI deepseek)
  chat deepseek = LLM.chat (toOpenAI deepseek)
  stream deepseek = LLM.stream (toOpenAI deepseek)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.Deepseek
Description : Deepseek integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the 'Deepseek' data type and implements the 'LLM' typeclass for interacting with Deepseek's language models.
It supports generating text, handling chat interactions, and streaming responses using Deepseek's API.

This implementation uses the OpenAI-compatible interface with baseUrl as "https://api.deepseek.com".

For more information on Deepseek's API, see: <https://platform.deepseek.com/api-docs/>
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
import Langchain.LLM.OpenAICompatible (OpenAICompatible (..))
import qualified Langchain.Runnable.Core as Run
import qualified OpenAI.V1.Chat.Completions as OpenAIV1

data Deepseek = Deepseek
  { apiKey :: Text
  -- ^ The API key for authenticating with Deepseek's services.
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations.
  , baseUrl :: Maybe String
  -- ^ Base url; default "https://api.deepseek.com"
  }

instance Show Deepseek where
  show _ = "Deepseek"

toOpenAI :: Deepseek -> OpenAICompatible
toOpenAI Deepseek {..} =
  OpenAICompatible
    { apiKey = apiKey
    , callbacks = callbacks
    , baseUrl = Just $ fromMaybe "https://api.deepseek.com" baseUrl
    , providerName = "Deepseek"
    }

instance LLM.LLM Deepseek where
  type LLMParams Deepseek = OpenAIV1.CreateChatCompletion
  type LLMStreamTokenType Deepseek = OpenAIV1.ChatCompletionChunk

  generate deepseek = LLM.generate (toOpenAI deepseek)
  chat deepseek = LLM.chat (toOpenAI deepseek)
  stream deepseek = LLM.stream (toOpenAI deepseek)

instance Run.Runnable Deepseek where
  type RunnableInput Deepseek = (ChatHistory, Maybe OpenAIV1.CreateChatCompletion)
  type RunnableOutput Deepseek = LLM.Message

  invoke = uncurry . chat

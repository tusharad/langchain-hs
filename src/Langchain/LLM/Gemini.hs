{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.Gemini
Description : Gemini integration for LangChain-hs
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the 'Gemini' data type and implements the 'LLM' typeclass for interacting with Gemini models.
It supports generating text, handling chat interactions, and streaming responses using Gemini's OpenAI compatibilty API.
for more info, https://ai.google.dev/gemini-api/docs/openai

This gemini type uses OpenAI module with baseUrl as "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions";
-}
module Langchain.LLM.Gemini
  ( Gemini (..)
  , defaultGemini
  , module Langchain.LLM.Core
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Langchain.Callback
import Langchain.LLM.Core
import qualified Langchain.LLM.Core as LLM
import qualified Langchain.LLM.Internal.OpenAI as OpenAI
import qualified Langchain.LLM.OpenAI as OpenAI
import qualified Langchain.Runnable.Core as Run

data Gemini = Gemini
  { apiKey :: Text
  -- ^ The API key for authenticating with Gemini's services.
  , geminiModelName :: Text
  -- ^ The name of the Gemini model to use (e.g., "gemini-2.0-flash").
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations.
  , baseUrl :: Maybe String
  -- ^ Base url; default "https://generativelanguage.googleapis.com/v1beta/openai"
  }

instance Show Gemini where
  show Gemini {..} = "Gemini " ++ show geminiModelName

toOpenAI :: Gemini -> OpenAI.OpenAI
toOpenAI Gemini {..} =
  OpenAI.OpenAI
    { OpenAI.apiKey = apiKey
    , OpenAI.openAIModelName = geminiModelName
    , OpenAI.callbacks = callbacks
    , OpenAI.baseUrl =
        Just $
          fromMaybe
            "https://generativelanguage.googleapis.com/v1beta/openai"
            baseUrl
    }

instance LLM.LLM Gemini where
  type LLMParams Gemini = OpenAI.OpenAIParams
  type LLMStreamTokenType Gemini = OpenAI.ChatCompletionChunk

  generate llm = LLM.generate (toOpenAI llm)
  chat llm = LLM.chat (toOpenAI llm)
  stream llm = LLM.stream (toOpenAI llm)

instance Run.Runnable Gemini where
  type RunnableInput Gemini = (LLM.ChatMessage, Maybe OpenAI.OpenAIParams)
  type RunnableOutput Gemini = LLM.Message

  invoke gemini (chatMessage, mbParams) = LLM.chat gemini chatMessage mbParams

defaultGemini :: Gemini
defaultGemini =
  Gemini
    { apiKey = ""
    , geminiModelName = "gemini-2.5-flash"
    , callbacks = []
    , baseUrl = Just "https://generativelanguage.googleapis.com/v1beta/openai"
    }

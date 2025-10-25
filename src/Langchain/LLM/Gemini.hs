{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.Gemini
Description : Google Gemini integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the 'Gemini' data type and implements the 'LLM' typeclass for interacting
with Google's Gemini language models through OpenAI-compatible API endpoints.

The 'Gemini' type encapsulates the API key, model name, and callbacks for event handling.
The 'LLM' instance methods ('generate', 'chat', 'stream') allow for seamless integration
with LangChain's processing pipelines.

For more information on Gemini API, see: <https://ai.google.dev/gemini-api/docs>

Notes:
* Gemini only supports base64 encoded image content. Check out examples.
* Uses OpenAI-compatible endpoint: https://ai.google.dev/gemini-api/docs/openai

Example usage:

@
import Data.Text (Text)
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.Gemini (Gemini(..), defaultGemini)

main :: IO()
main = do
  let gemini = defaultGemini { apiKey = "your-api-key" }
  result <- LLM.generate gemini "Explain functional programming" Nothing
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right response -> print response
@
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
import qualified Langchain.LLM.Internal.OpenAI as Internal
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
  type LLMStreamTokenType Gemini = Internal.ChatCompletionChunk

  generate = LLM.generate . toOpenAI
  chat = LLM.chat . toOpenAI
  stream = LLM.stream . toOpenAI

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

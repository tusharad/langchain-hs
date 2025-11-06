{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.OpenAI
Description : OpenAI integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides the 'OpenAI' data type and implements the 'LLM' typeclass for interacting with OpenAI's language models.
It supports generating text, handling chat interactions, and streaming responses using OpenAI's API.

The 'OpenAI' type encapsulates the API key, model name, and callbacks for event handling.
The 'LLM' instance methods ('generate', 'chat', 'stream') allow for seamless integration with LangChain's processing pipelines.

For more information on OpenAI's API, see: <https://platform.openai.com/docs/api-reference>

@
import Data.Text (Text)
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAI (OpenAI(..))

main :: IO()
main = do
  let openAI = OpenAI
        { apiKey = "your-api-key"
        , openAIModelName = "gpt-3.5-turbo"
        , callbacks = []
        , baseUrl = Nothing
        }
  result <- LLM.generate openAI "Tell me a joke" Nothing
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> putStrLn response
@
-}
module Langchain.LLM.OpenAI
  ( -- * Types
    OpenAI (..)

    -- * Default functions
  , defaultOpenAI

    -- * Re-export
  , module Langchain.LLM.Core
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Langchain.Callback (Callback)
import Langchain.LLM.Core
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAICompatible (OpenAICompatible)
import qualified Langchain.LLM.OpenAICompatible as OpenAICompatible
import qualified Langchain.Runnable.Core as Run
import qualified OpenAI.V1.Chat.Completions as OpenAIV1

{- | Configuration for OpenAI's language models.

This data type holds the necessary information to interact with OpenAI's API,
including the API key, the model name, and a list of callbacks for handling events.
-}
data OpenAI = OpenAI
  { apiKey :: Text
  -- ^ The API key for authenticating with OpenAI's services.
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations.
  , baseUrl :: Maybe String
  -- ^ Base url; default "https://api.openai.com"
  }

-- | Not including API key to avoid accidental leak
instance Show OpenAI where
  show _ = "OpenAI"

toOpenAI :: OpenAI -> OpenAICompatible
toOpenAI OpenAI {..} =
  OpenAICompatible.OpenAICompatible
    { apiKey = apiKey
    , callbacks = callbacks
    , baseUrl =
        Just $
          fromMaybe
            "https://generativelanguage.googleapis.com/v1beta/openai"
            baseUrl
    , providerName = "OpenAI"
    }

{- | Implementation of the 'LLM' typeclass for OpenAI models.

This instance provides methods for generating text, handling chat interactions,
and streaming responses using OpenAI's API.
-}
instance LLM.LLM OpenAI where
  type LLMParams OpenAI = OpenAIV1.CreateChatCompletion
  type LLMStreamTokenType OpenAI = OpenAIV1.ChatCompletionChunk

  generate = LLM.generate . toOpenAI
  chat = LLM.chat . toOpenAI
  stream = LLM.stream . toOpenAI

instance Run.Runnable OpenAI where
  type RunnableInput OpenAI = (ChatHistory, Maybe OpenAIV1.CreateChatCompletion)
  type RunnableOutput OpenAI = LLM.Message

  invoke = uncurry . chat

-- | Default values for OpenAI
defaultOpenAI :: OpenAI
defaultOpenAI = OpenAI "your-api-key" [] Nothing

{-
ghci> :set -XOverloadedStrings
ghci> let o = OpenAI { apiKey = <my api key>
    , openAIModelName = "gpt-4.1-nano"
    , Langchain.LLM.OpenAI.callbacks = []
    }
ghci> eRes <- generate o "What is 2+2" Nothing
ghci> eRes
Right "2 + 2 equals 4."
ghci> import qualified Data.List.NonEmpty as NE
ghci> let msg = Langchain.LLM.Core.Message Langchain.LLM.Core.User "What is 2+2" defaultMessageData
ghci> let chatMsg = NE.fromList [msg]
ghci> eRes <- chat o chatMsg Nothing
ghci> eRes
Right "2 + 2 equals 4."
-}

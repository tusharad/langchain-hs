{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.OpenAICompatible
Description : Generic OpenAI-compatible API integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides a generic 'OpenAICompatible' data type and
implements the 'LLM' typeclass for interacting with any service that provides
an OpenAI-compatible API interface.

Also provides convenience functions for popular providers like LMStudio, llama.cpp,
and OpenRouter.

Example usage:

@
-- Using with LMStudio
let lmStudio = mkLMStudio "my-model" [] Nothing Nothing
result <- generate lmStudio "What is functional programming?" Nothing

-- Using with OpenRouter
let openRouter = mkOpenRouter "anthropic/claude-3-opus" [] Nothing "your-api-key"
result <- generate openRouter "Explain Haskell monads" Nothing
@
-}
module Langchain.LLM.OpenAICompatible
  ( OpenAICompatible (..)
  -- , OpenAI.OpenAIParams (..)
  , mkLMStudio
  , mkLlamaCpp
  , mkOpenRouter
  , module Langchain.LLM.Core
  ) where

import Control.Exception (SomeException, try)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Langchain.Callback
import qualified Langchain.Error as Error
import Langchain.LLM.Core
import qualified Langchain.LLM.Core as LLM
import qualified Langchain.Runnable.Core as LLM
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import qualified OpenAI.V1.Chat.Completions as CreateCompletion (CreateChatCompletion (..))
import qualified OpenAI.V1.Chat.Completions as OpenAIV1

-- import qualified Langchain.LLM.Internal.OpenAI as OpenAI
-- import qualified Langchain.LLM.OpenAI as OpenAI

-- | Generic OpenAICompatible implementation for any service with an OpenAI-compatible API
data OpenAICompatible = OpenAICompatible
  { apiKey :: T.Text
  -- ^ The API key for authenticating.
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations
  , baseUrl :: Maybe String
  -- ^ Base URL for the service. Default "https://api.openai.com"
  , providerName :: T.Text
  -- ^ The provider or service name
  }

instance Show OpenAICompatible where
  show OpenAICompatible {..} = show providerName

toOpenAIMsg :: LLM.Message -> OpenAIV1.Message (V.Vector Content)
toOpenAIMsg LLM.Message {..} = toRole role content
  where
    toRole LLM.User content1 =
      OpenAIV1.User
        { OpenAIV1.content = V.fromList [OpenAIV1.Text content1]
        , name = Nothing
        }
    toRole LLM.System content1 =
      OpenAIV1.System
        { OpenAIV1.content = V.fromList [OpenAIV1.Text content1]
        , name = Nothing
        }
    toRole LLM.Assistant content1 =
      OpenAIV1.Assistant
        { OpenAIV1.assistant_content = Just $ V.fromList [OpenAIV1.Text content1]
        , name = Nothing
        , refusal = Nothing
        , assistant_audio = Nothing
        , tool_calls = Nothing
        }
    toRole LLM.Tool content1 =
      OpenAIV1.Tool
        { OpenAIV1.content = V.fromList [OpenAIV1.Text content1]
        , tool_call_id = ""
        }
    toRole _ content1 =
      OpenAIV1.User
        { OpenAIV1.content = V.fromList [OpenAIV1.Text content1]
        , name = Nothing
        }

fromOpenAIMsg :: OpenAIV1.Message T.Text -> LLM.Message
fromOpenAIMsg OpenAIV1.User {..} =
  defaultMessage
    { role = LLM.User
    , content = content
    }
fromOpenAIMsg OpenAIV1.System {..} =
  defaultMessage
    { role = LLM.System
    , content = content
    }
fromOpenAIMsg OpenAIV1.Assistant {..} =
  defaultMessage
    { role = LLM.Assistant
    , content = fromMaybe "" assistant_content
    }
fromOpenAIMsg OpenAIV1.Tool {..} =
  defaultMessage
    { role = LLM.Tool
    , content = content
    }

instance LLM.LLM OpenAICompatible where
  type LLMParams OpenAICompatible = OpenAIV1.CreateChatCompletion
  type LLMStreamTokenType OpenAICompatible = OpenAIV1.ChatCompletionChunk

  generate OpenAICompatible {..} prompt mbLLMParams = do
    clientEnv <- getClientEnv $ maybe "https://api.openai.com" T.pack baseUrl
    let Methods {createChatCompletion} = makeMethods clientEnv apiKey Nothing Nothing
    eRes <-
      try $
        createChatCompletion
          _CreateChatCompletion
            { OpenAIV1.messages =
                V.fromList
                  [ OpenAIV1.User
                      { OpenAIV1.content = V.fromList [OpenAIV1.Text prompt]
                      , name = Nothing
                      }
                  ]
            , OpenAIV1.model = maybe "gpt-4o-mini" CreateCompletion.model mbLLMParams
            }
    case eRes of
      Left err -> pure $ Left $ Error.fromString $ show (err :: SomeException)
      Right (ChatCompletionObject {choices}) -> do
        let Choice {message} = V.head choices
        pure (Right $ messageToContent message)

  chat OpenAICompatible {..} chatHistory mbLLMParams = do
    clientEnv <- getClientEnv $ maybe "https://api.openai.com" T.pack baseUrl
    let Methods {createChatCompletion} = makeMethods clientEnv apiKey Nothing Nothing
    eRes <-
      try $
        createChatCompletion
          _CreateChatCompletion
            { OpenAIV1.messages =
                V.fromList $ map toOpenAIMsg (NE.toList chatHistory)
            , OpenAIV1.model = maybe "gpt-4o-mini" CreateCompletion.model mbLLMParams
            }
    case eRes of
      Left err -> pure $ Left $ Error.fromString $ show (err :: SomeException)
      Right (ChatCompletionObject {choices}) -> do
        let Choice {message} = V.head choices
        pure (Right $ fromOpenAIMsg message)

  stream OpenAICompatible {..} chatHistory streamHandler mbLLMParams = do
    let onEvent (Left _) = pure () -- ignore for now
        onEvent (Right chunk) = onToken streamHandler chunk

    clientEnv <- getClientEnv $ maybe "https://api.openai.com" T.pack baseUrl
    let Methods {createChatCompletionStreamTyped} = makeMethods clientEnv apiKey Nothing Nothing
    let req_ =
          _CreateChatCompletion
            { OpenAIV1.messages =
                V.fromList $ map toOpenAIMsg (NE.toList chatHistory)
            , OpenAIV1.model = maybe "gpt-4o-mini" CreateCompletion.model mbLLMParams
            }
    _ <- createChatCompletionStreamTyped req_ onEvent
    pure $ Right ()

-- | Create an LMStudio instance
mkLMStudio :: [Callback] -> Maybe String -> T.Text -> OpenAICompatible
mkLMStudio callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = apiKey'
    , callbacks = callbacks'
    , baseUrl = Just $ fromMaybe "http://localhost:1234/v1" baseUrl'
    , providerName = "LMStudio"
    }

-- | Create a llama.cpp instance
mkLlamaCpp :: [Callback] -> Maybe String -> T.Text -> OpenAICompatible
mkLlamaCpp callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = apiKey'
    , callbacks = callbacks'
    , baseUrl = Just $ fromMaybe "http://localhost:8080/v1" baseUrl'
    , providerName = "LlamaCpp"
    }

{- | Create an OpenRouter instance
OpenRouter provides access to multiple model providers through a single API
Model name should be in the format "provider/model" (e.g., "anthropic/claude-3-opus")
-}
mkOpenRouter :: [Callback] -> Maybe String -> T.Text -> OpenAICompatible
mkOpenRouter callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = apiKey' -- OpenRouter requires an API key
    , callbacks = callbacks'
    , baseUrl = Just $ fromMaybe "https://openrouter.ai" baseUrl'
    , providerName = "OpenRouter"
    }

instance LLM.Runnable OpenAICompatible where
  type RunnableInput OpenAICompatible = (ChatHistory, Maybe OpenAIV1.CreateChatCompletion)
  type RunnableOutput OpenAICompatible = LLM.Message

  invoke = uncurry . chat

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , OpenAIParams (..)

    -- * Default functions
  , defaultOpenAIParams
  , defaultOpenAI

    -- * Re-export
  , module Langchain.LLM.Core
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Langchain.Callback (Callback)
import Langchain.Error (llmError)
import Langchain.LLM.Core
import qualified Langchain.LLM.Core as LLM
import qualified Langchain.LLM.Internal.OpenAI as OpenAI
import qualified Langchain.Runnable.Core as Run

{- | Configuration for OpenAI's language models.

This data type holds the necessary information to interact with OpenAI's API,
including the API key, the model name, and a list of callbacks for handling events.
-}
data OpenAI = OpenAI
  { apiKey :: Text
  -- ^ The API key for authenticating with OpenAI's services.
  , openAIModelName :: Text
  -- ^ The name of the OpenAI model to use (e.g., "gpt-3.5-turbo", "gpt-4").
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations.
  , baseUrl :: Maybe String
  -- ^ Base url; default "https://api.openai.com/v1"
  }

-- | Not including API key to avoid accidental leak
instance Show OpenAI where
  show OpenAI {..} = "OpenAI " ++ show openAIModelName

{- | Implementation of the 'LLM' typeclass for OpenAI models.

This instance provides methods for generating text, handling chat interactions,
and streaming responses using OpenAI's API.
-}
instance LLM.LLM OpenAI where
  type LLMParams OpenAI = OpenAIParams
  type LLMStreamTokenType OpenAI = OpenAI.ChatCompletionChunk

  generate OpenAI {..} prompt mbOpenAIParams = do
    eRes <-
      OpenAI.createChatCompletion
        apiKey
        ( OpenAI.defaultChatCompletionRequest
            { OpenAI.model = openAIModelName
            , OpenAI.baseUrl = baseUrl
            , OpenAI.messages =
                [ OpenAI.defaultMessage
                    { OpenAI.content =
                        Just
                          (OpenAI.StringContent prompt)
                    }
                ]
            , OpenAI.timeout = timeout =<< mbOpenAIParams
            , OpenAI.frequencyPenalty = frequencyPenalty =<< mbOpenAIParams
            , OpenAI.logitBias = logitBias =<< mbOpenAIParams
            , OpenAI.logprobs = logprobs =<< mbOpenAIParams
            , OpenAI.maxCompletionTokens = maxCompletionTokens =<< mbOpenAIParams
            , OpenAI.maxTokens = maxTokens =<< mbOpenAIParams
            , OpenAI.metadata = metadata =<< mbOpenAIParams
            , OpenAI.modalities = modalities =<< mbOpenAIParams
            , OpenAI.n = n =<< mbOpenAIParams
            , OpenAI.parallelToolCalls = parallelToolCalls =<< mbOpenAIParams
            , OpenAI.prediction = prediction =<< mbOpenAIParams
            , OpenAI.presencePenalty = presencePenalty =<< mbOpenAIParams
            , OpenAI.reasoningEffort = reasoningEffort =<< mbOpenAIParams
            , OpenAI.responseFormat = responseFormat =<< mbOpenAIParams
            , OpenAI.seed = seed =<< mbOpenAIParams
            , OpenAI.serviceTier = serviceTier =<< mbOpenAIParams
            , OpenAI.stop = stop =<< mbOpenAIParams
            , OpenAI.store = store =<< mbOpenAIParams
            , OpenAI.temperature = temperature =<< mbOpenAIParams
            , OpenAI.toolChoice = toolChoice =<< mbOpenAIParams
            , OpenAI.tools = tools =<< mbOpenAIParams
            , OpenAI.topLogprobs = topLogprobs =<< mbOpenAIParams
            , OpenAI.topP = topP =<< mbOpenAIParams
            , OpenAI.user = user =<< mbOpenAIParams
            , OpenAI.webSearchOptions = webSearchOptions =<< mbOpenAIParams
            , OpenAI.audio = audio =<< mbOpenAIParams
            }
        )
    case eRes of
      Left err -> return $ Left err
      Right r -> do
        case listToMaybe ((\OpenAI.ChatCompletionResponse {..} -> choices) r) of
          Nothing -> return $ Left (llmError "Did not received any response" Nothing Nothing)
          Just resp ->
            let OpenAI.Message {..} = OpenAI.message resp
             in pure $
                  Right $
                    maybe
                      ""
                      ( \case
                          OpenAI.StringContent t -> t
                          OpenAI.ContentParts _ -> ""
                      )
                      content
  chat OpenAI {..} msgs mbOpenAIParams = do
    eRes <-
      OpenAI.createChatCompletion
        apiKey
        ( OpenAI.defaultChatCompletionRequest
            { OpenAI.model = openAIModelName
            , OpenAI.baseUrl = baseUrl
            , OpenAI.messages = NE.toList $ NE.map LLM.to msgs
            , OpenAI.timeout = timeout =<< mbOpenAIParams
            , OpenAI.frequencyPenalty = frequencyPenalty =<< mbOpenAIParams
            , OpenAI.logitBias = logitBias =<< mbOpenAIParams
            , OpenAI.logprobs = logprobs =<< mbOpenAIParams
            , OpenAI.maxCompletionTokens = maxCompletionTokens =<< mbOpenAIParams
            , OpenAI.maxTokens = maxTokens =<< mbOpenAIParams
            , OpenAI.metadata = metadata =<< mbOpenAIParams
            , OpenAI.modalities = modalities =<< mbOpenAIParams
            , OpenAI.n = n =<< mbOpenAIParams
            , OpenAI.parallelToolCalls = parallelToolCalls =<< mbOpenAIParams
            , OpenAI.prediction = prediction =<< mbOpenAIParams
            , OpenAI.presencePenalty = presencePenalty =<< mbOpenAIParams
            , OpenAI.reasoningEffort = reasoningEffort =<< mbOpenAIParams
            , OpenAI.responseFormat = responseFormat =<< mbOpenAIParams
            , OpenAI.seed = seed =<< mbOpenAIParams
            , OpenAI.serviceTier = serviceTier =<< mbOpenAIParams
            , OpenAI.stop = stop =<< mbOpenAIParams
            , OpenAI.store = store =<< mbOpenAIParams
            , OpenAI.temperature = temperature =<< mbOpenAIParams
            , OpenAI.toolChoice = toolChoice =<< mbOpenAIParams
            , OpenAI.tools = tools =<< mbOpenAIParams
            , OpenAI.topLogprobs = topLogprobs =<< mbOpenAIParams
            , OpenAI.topP = topP =<< mbOpenAIParams
            , OpenAI.user = user =<< mbOpenAIParams
            , OpenAI.webSearchOptions = webSearchOptions =<< mbOpenAIParams
            , OpenAI.audio = audio =<< mbOpenAIParams
            }
        )
    case eRes of
      Left err -> return $ Left err
      Right r -> do
        case listToMaybe ((\OpenAI.ChatCompletionResponse {..} -> choices) r) of
          Nothing -> return $ Left (llmError "Did not received any response" Nothing Nothing)
          Just resp -> return $ Right $ LLM.from $ OpenAI.message resp

  stream OpenAI {..} msgs LLM.StreamHandler {onComplete, onToken} mbOpenAIParams = do
    let req =
          OpenAI.defaultChatCompletionRequest
            { OpenAI.model = openAIModelName
            , OpenAI.baseUrl = baseUrl
            , OpenAI.messages = NE.toList $ NE.map LLM.to msgs
            , OpenAI.stream = Just True -- Enable streaming'
            , OpenAI.timeout = timeout =<< mbOpenAIParams
            , OpenAI.frequencyPenalty = frequencyPenalty =<< mbOpenAIParams
            , OpenAI.logitBias = logitBias =<< mbOpenAIParams
            , OpenAI.logprobs = logprobs =<< mbOpenAIParams
            , OpenAI.maxCompletionTokens = maxCompletionTokens =<< mbOpenAIParams
            , OpenAI.maxTokens = maxTokens =<< mbOpenAIParams
            , OpenAI.metadata = metadata =<< mbOpenAIParams
            , OpenAI.modalities = modalities =<< mbOpenAIParams
            , OpenAI.n = n =<< mbOpenAIParams
            , OpenAI.parallelToolCalls = parallelToolCalls =<< mbOpenAIParams
            , OpenAI.prediction = prediction =<< mbOpenAIParams
            , OpenAI.presencePenalty = presencePenalty =<< mbOpenAIParams
            , OpenAI.reasoningEffort = reasoningEffort =<< mbOpenAIParams
            , OpenAI.responseFormat = responseFormat =<< mbOpenAIParams
            , OpenAI.seed = seed =<< mbOpenAIParams
            , OpenAI.serviceTier = serviceTier =<< mbOpenAIParams
            , OpenAI.stop = stop =<< mbOpenAIParams
            , OpenAI.store = store =<< mbOpenAIParams
            , OpenAI.temperature = temperature =<< mbOpenAIParams
            , OpenAI.toolChoice = toolChoice =<< mbOpenAIParams
            , OpenAI.tools = tools =<< mbOpenAIParams
            , OpenAI.topLogprobs = topLogprobs =<< mbOpenAIParams
            , OpenAI.topP = topP =<< mbOpenAIParams
            , OpenAI.user = user =<< mbOpenAIParams
            , OpenAI.webSearchOptions = webSearchOptions =<< mbOpenAIParams
            , OpenAI.audio = audio =<< mbOpenAIParams
            }
    OpenAI.createChatCompletionStream
      apiKey
      req
      OpenAI.OpenAIStreamHandler
        { OpenAI.onComplete = onComplete
        , OpenAI.onToken = onToken
        }

instance Run.Runnable OpenAI where
  type RunnableInput OpenAI = (LLM.ChatHistory, Maybe OpenAIParams)
  type RunnableOutput OpenAI = LLM.Message

  invoke = uncurry . LLM.chat

-- | Parameters for customizing OpenAI API calls.
data OpenAIParams = OpenAIParams
  { timeout :: Maybe Int
  , frequencyPenalty :: Maybe Double
  , logitBias :: Maybe (Map Text Int)
  , logprobs :: Maybe Bool
  , maxCompletionTokens :: Maybe Int
  , maxTokens :: Maybe Int
  , metadata :: Maybe (Map Text Text)
  , modalities :: Maybe [OpenAI.Modality]
  , n :: Maybe Int
  , parallelToolCalls :: Maybe Bool
  , prediction :: Maybe OpenAI.PredictionOutput
  , presencePenalty :: Maybe Double
  , reasoningEffort :: Maybe OpenAI.ReasoningEffort
  , responseFormat :: Maybe OpenAI.ResponseFormat
  , seed :: Maybe Int
  , serviceTier :: Maybe Text
  , stop :: Maybe (Either Text [Text])
  , store :: Maybe Bool
  , temperature :: Maybe Double
  , toolChoice :: Maybe OpenAI.ToolChoice
  , tools :: Maybe [OpenAI.InputTool]
  , topLogprobs :: Maybe Int
  , topP :: Maybe Double
  , user :: Maybe Text
  , webSearchOptions :: Maybe OpenAI.WebSearchOptions
  , audio :: Maybe OpenAI.AudioConfig
  }

-- | Default parameters for OpenAI API calls.
defaultOpenAIParams :: OpenAIParams
defaultOpenAIParams =
  OpenAIParams
    { timeout = Just 60
    , frequencyPenalty = Nothing
    , logitBias = Nothing
    , logprobs = Nothing
    , maxCompletionTokens = Nothing
    , maxTokens = Nothing
    , metadata = Nothing
    , modalities = Nothing
    , n = Nothing
    , parallelToolCalls = Nothing
    , prediction = Nothing
    , presencePenalty = Nothing
    , reasoningEffort = Nothing
    , responseFormat = Nothing
    , seed = Nothing
    , serviceTier = Nothing
    , stop = Nothing
    , store = Nothing
    , temperature = Nothing
    , toolChoice = Nothing
    , tools = Nothing
    , topLogprobs = Nothing
    , topP = Nothing
    , user = Nothing
    , webSearchOptions = Nothing
    , audio = Nothing
    }

-- | Default values for OpenAI
defaultOpenAI :: OpenAI
defaultOpenAI = OpenAI "your-api-key" "gpt-4.1-nano" [] Nothing

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

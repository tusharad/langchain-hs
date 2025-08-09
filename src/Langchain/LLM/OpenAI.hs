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

    -- * Re-export
  , module Langchain.LLM.Core
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Langchain.Callback (Callback)
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
  type LLMStreamTokenType OpenAI = Text

  generate OpenAI {..} prompt mbOpenAIParams = do
    eRes <-
      OpenAI.createChatCompletion
        apiKey
        ( OpenAI.defaultChatCompletionRequest
            { OpenAI.model = openAIModelName
            , OpenAI.baseUrl = baseUrl
            , OpenAI.messages =
                [OpenAI.defaultMessage {OpenAI.content = Just (OpenAI.StringContent prompt)}]
            , OpenAI.timeout = maybe Nothing timeout mbOpenAIParams
            , OpenAI.frequencyPenalty = maybe Nothing frequencyPenalty mbOpenAIParams
            , OpenAI.logitBias = maybe Nothing logitBias mbOpenAIParams
            , OpenAI.logprobs = maybe Nothing logprobs mbOpenAIParams
            , OpenAI.maxCompletionTokens = maybe Nothing maxCompletionTokens mbOpenAIParams
            , OpenAI.maxTokens = maybe Nothing maxTokens mbOpenAIParams
            , OpenAI.metadata = maybe Nothing metadata mbOpenAIParams
            , OpenAI.modalities = maybe Nothing modalities mbOpenAIParams
            , OpenAI.n = maybe Nothing n mbOpenAIParams
            , OpenAI.parallelToolCalls = maybe Nothing parallelToolCalls mbOpenAIParams
            , OpenAI.prediction = maybe Nothing prediction mbOpenAIParams
            , OpenAI.presencePenalty = maybe Nothing presencePenalty mbOpenAIParams
            , OpenAI.reasoningEffort = maybe Nothing reasoningEffort mbOpenAIParams
            , OpenAI.responseFormat = maybe Nothing responseFormat mbOpenAIParams
            , OpenAI.seed = maybe Nothing seed mbOpenAIParams
            , OpenAI.serviceTier = maybe Nothing serviceTier mbOpenAIParams
            , OpenAI.stop = maybe Nothing stop mbOpenAIParams
            , OpenAI.store = maybe Nothing store mbOpenAIParams
            , OpenAI.temperature = maybe Nothing temperature mbOpenAIParams
            , OpenAI.toolChoice = maybe Nothing toolChoice mbOpenAIParams
            , OpenAI.tools = maybe Nothing tools mbOpenAIParams
            , OpenAI.topLogprobs = maybe Nothing topLogprobs mbOpenAIParams
            , OpenAI.topP = maybe Nothing topP mbOpenAIParams
            , OpenAI.user = maybe Nothing user mbOpenAIParams
            , OpenAI.webSearchOptions = maybe Nothing webSearchOptions mbOpenAIParams
            , OpenAI.audio = maybe Nothing audio mbOpenAIParams
            }
        )
    case eRes of
      Left err -> return $ Left err
      Right r -> do
        case listToMaybe ((\OpenAI.ChatCompletionResponse {..} -> choices) r) of
          Nothing -> return $ Left "Did not received any response"
          Just resp ->
            let OpenAI.Message {..} = OpenAI.message resp
             in pure $
                  Right $
                    maybe
                      ""
                      ( \c -> case c of
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
            , OpenAI.messages = toOpenAIMessages msgs
            , OpenAI.timeout = maybe Nothing timeout mbOpenAIParams
            , OpenAI.frequencyPenalty = maybe Nothing frequencyPenalty mbOpenAIParams
            , OpenAI.logitBias = maybe Nothing logitBias mbOpenAIParams
            , OpenAI.logprobs = maybe Nothing logprobs mbOpenAIParams
            , OpenAI.maxCompletionTokens = maybe Nothing maxCompletionTokens mbOpenAIParams
            , OpenAI.maxTokens = maybe Nothing maxTokens mbOpenAIParams
            , OpenAI.metadata = maybe Nothing metadata mbOpenAIParams
            , OpenAI.modalities = maybe Nothing modalities mbOpenAIParams
            , OpenAI.n = maybe Nothing n mbOpenAIParams
            , OpenAI.parallelToolCalls = maybe Nothing parallelToolCalls mbOpenAIParams
            , OpenAI.prediction = maybe Nothing prediction mbOpenAIParams
            , OpenAI.presencePenalty = maybe Nothing presencePenalty mbOpenAIParams
            , OpenAI.reasoningEffort = maybe Nothing reasoningEffort mbOpenAIParams
            , OpenAI.responseFormat = maybe Nothing responseFormat mbOpenAIParams
            , OpenAI.seed = maybe Nothing seed mbOpenAIParams
            , OpenAI.serviceTier = maybe Nothing serviceTier mbOpenAIParams
            , OpenAI.stop = maybe Nothing stop mbOpenAIParams
            , OpenAI.store = maybe Nothing store mbOpenAIParams
            , OpenAI.temperature = maybe Nothing temperature mbOpenAIParams
            , OpenAI.toolChoice = maybe Nothing toolChoice mbOpenAIParams
            , OpenAI.tools = maybe Nothing tools mbOpenAIParams
            , OpenAI.topLogprobs = maybe Nothing topLogprobs mbOpenAIParams
            , OpenAI.topP = maybe Nothing topP mbOpenAIParams
            , OpenAI.user = maybe Nothing user mbOpenAIParams
            , OpenAI.webSearchOptions = maybe Nothing webSearchOptions mbOpenAIParams
            , OpenAI.audio = maybe Nothing audio mbOpenAIParams
            }
        )
    case eRes of
      Left err -> return $ Left err
      Right r -> do
        case listToMaybe ((\OpenAI.ChatCompletionResponse {..} -> choices) r) of
          Nothing -> return $ Left "Did not received any response"
          Just resp -> return $ Right $ LLM.from $ OpenAI.message resp

  {-
  let OpenAI.Message {..} = OpenAI.message resp
   in pure $
        Right $
          maybe
            ""
            ( \c -> case c of
                OpenAI.StringContent t -> t
                OpenAI.ContentParts _ -> ""
            )
            content
            -}

  stream OpenAI {..} msgs LLM.StreamHandler {onComplete, onToken} mbOpenAIParams = do
    let req =
          OpenAI.defaultChatCompletionRequest
            { OpenAI.model = openAIModelName
            , OpenAI.baseUrl = baseUrl
            , OpenAI.messages = toOpenAIMessages msgs
            , OpenAI.stream = Just True -- Enable streaming'
            , OpenAI.timeout = maybe Nothing timeout mbOpenAIParams
            , OpenAI.frequencyPenalty = maybe Nothing frequencyPenalty mbOpenAIParams
            , OpenAI.logitBias = maybe Nothing logitBias mbOpenAIParams
            , OpenAI.logprobs = maybe Nothing logprobs mbOpenAIParams
            , OpenAI.maxCompletionTokens = maybe Nothing maxCompletionTokens mbOpenAIParams
            , OpenAI.maxTokens = maybe Nothing maxTokens mbOpenAIParams
            , OpenAI.metadata = maybe Nothing metadata mbOpenAIParams
            , OpenAI.modalities = maybe Nothing modalities mbOpenAIParams
            , OpenAI.n = maybe Nothing n mbOpenAIParams
            , OpenAI.parallelToolCalls = maybe Nothing parallelToolCalls mbOpenAIParams
            , OpenAI.prediction = maybe Nothing prediction mbOpenAIParams
            , OpenAI.presencePenalty = maybe Nothing presencePenalty mbOpenAIParams
            , OpenAI.reasoningEffort = maybe Nothing reasoningEffort mbOpenAIParams
            , OpenAI.responseFormat = maybe Nothing responseFormat mbOpenAIParams
            , OpenAI.seed = maybe Nothing seed mbOpenAIParams
            , OpenAI.serviceTier = maybe Nothing serviceTier mbOpenAIParams
            , OpenAI.stop = maybe Nothing stop mbOpenAIParams
            , OpenAI.store = maybe Nothing store mbOpenAIParams
            , OpenAI.temperature = maybe Nothing temperature mbOpenAIParams
            , OpenAI.toolChoice = maybe Nothing toolChoice mbOpenAIParams
            , OpenAI.tools = maybe Nothing tools mbOpenAIParams
            , OpenAI.topLogprobs = maybe Nothing topLogprobs mbOpenAIParams
            , OpenAI.topP = maybe Nothing topP mbOpenAIParams
            , OpenAI.user = maybe Nothing user mbOpenAIParams
            , OpenAI.webSearchOptions = maybe Nothing webSearchOptions mbOpenAIParams
            , OpenAI.audio = maybe Nothing audio mbOpenAIParams
            }
    OpenAI.createChatCompletionStream
      apiKey
      req
      OpenAI.OpenAIStreamHandler
        { OpenAI.onComplete = onComplete
        , OpenAI.onToken = onToken . chunkToText
        }
    where
      chunkToText :: OpenAI.ChatCompletionChunk -> Text
      chunkToText OpenAI.ChatCompletionChunk {..} = do
        case listToMaybe chunkChoices of
          Nothing -> ""
          Just OpenAI.ChunkChoice {..} ->
            fromMaybe "" ((\OpenAI.Delta {..} -> contentForDelta) delta)

toOpenAIMessages :: LLM.ChatMessage -> [OpenAI.Message]
toOpenAIMessages msgs = map go (NE.toList msgs)
  where
    toRole :: LLM.Role -> OpenAI.Role
    toRole r = case r of
      LLM.System -> OpenAI.System
      LLM.User -> OpenAI.User
      LLM.Assistant -> OpenAI.Assistant
      LLM.Tool -> OpenAI.Tool
      LLM.Developer -> OpenAI.Developer
      LLM.Function -> OpenAI.Function

    go :: LLM.Message -> OpenAI.Message
    go msg =
      OpenAI.defaultMessage
        { OpenAI.role = toRole $ LLM.role msg
        , OpenAI.content = Just $ OpenAI.StringContent (LLM.content msg)
        }

instance Run.Runnable OpenAI where
  type RunnableInput OpenAI = (LLM.ChatMessage, Maybe OpenAIParams)
  type RunnableOutput OpenAI = LLM.Message

  invoke = uncurry . LLM.chat

-- | Parameters for customizing OpenAI API calls.
data OpenAIParams = OpenAIParams
  { timeout :: Maybe Int
  , frequencyPenalty :: Maybe Double
  , logitBias :: Maybe (Map Text Double)
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      Langchain.LLM.Huggingface
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

Huggingface inference implementation Langchain's LLM Interface.
https://huggingface.co/docs/inference-providers/providers/cerebras

* Support for text generation, chat, and streaming responses
* Configuration of Huggingface-specific parameters (temperature, max tokens, etc.)
* Conversion between Langchain's message format and Huggingface's API requirements
* Compatibility with Huggingface's hosted inference API and other providers
-}
module Langchain.LLM.Huggingface
  ( -- * Types
    Huggingface (..)
  , Huggingface.Provider (..)
  , HuggingfaceParams (..)

    -- * Functions
  , defaultHuggingfaceParams
  , Huggingface.defaultMessage

    -- * Re-export
  , module LLM
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Langchain.Callback
import Langchain.Error (llmError)
import Langchain.LLM.Core as LLM
import qualified Langchain.LLM.Internal.Huggingface as Huggingface

-- | Configuration for Huggingface LLM integration
data Huggingface = Huggingface
  { provider :: Huggingface.Provider
  -- ^ Service provider (e.g., HostedInferenceAPI)
  , apiKey :: Text
  -- ^ Huggingface API authentication key
  , modelName :: Text
  -- ^ Model identifier (e.g., "google/flan-t5-xl")
  , callbacks :: [Callback]
  -- ^ Event handlers for inference lifecycle
  }

instance Show Huggingface where
  show Huggingface {..} =
    "Huggingface { provider = "
      <> show provider
      <> ", modelName = "
      <> unpack modelName
      <> " }"

-- | Generation parameters specific to Huggingface models
data HuggingfaceParams = HuggingfaceParams
  { frequencyPenalty :: Maybe Double
  -- ^ Penalty for token frequency (0.0-2.0)
  , maxTokens :: Maybe Integer
  -- ^ Token limit for output
  , presencePenalty :: Maybe Double
  -- ^ Penalty for token presence (0.0-2.0)
  , stop :: Maybe [String]
  -- ^ Stop sequences to terminate generation
  , toolPrompt :: Maybe String
  -- ^ Special prompt for tool interactions
  , topP :: Maybe Double
  -- ^ Nucleus sampling probability threshold
  , temperature :: Maybe Double
  -- ^ Sampling temperature (0.0-1.0)
  , timeout :: Maybe Int
  -- ^ Number of seconds for request timeout
  }
  deriving (Eq, Show)

-- | Default values for huggingface params
defaultHuggingfaceParams :: HuggingfaceParams
defaultHuggingfaceParams =
  HuggingfaceParams
    { frequencyPenalty = Nothing
    , maxTokens = Nothing
    , presencePenalty = Nothing
    , stop = Nothing
    , toolPrompt = Nothing
    , topP = Nothing
    , temperature = Nothing
    , timeout = Just 60
    }

instance LLM Huggingface where
  type LLMParams Huggingface = HuggingfaceParams
  type LLMStreamTokenType Huggingface = Text

  generate Huggingface {..} prompt mbHuggingfaceParams = do
    eRes <-
      Huggingface.createChatCompletion
        apiKey
        Huggingface.defaultHuggingfaceChatCompletionRequest
          { Huggingface.provider = provider
          , Huggingface.messages =
              [ Huggingface.defaultMessage
                  { Huggingface.content = Huggingface.TextContent prompt
                  }
              ]
          , Huggingface.model = modelName
          , Huggingface.stream = False
          , Huggingface.maxTokens = maxTokens =<< mbHuggingfaceParams
          , Huggingface.frequencyPenalty = frequencyPenalty =<< mbHuggingfaceParams
          , -- , Huggingface.logProbs = maybe Nothing logProbs mbHuggingfaceParams
            Huggingface.presencePenalty = presencePenalty =<< mbHuggingfaceParams
          , -- , Huggingface.seed = maybe Nothing seed mbHuggingfaceParams
            Huggingface.stop = stop =<< mbHuggingfaceParams
          , Huggingface.temperature = temperature =<< mbHuggingfaceParams
          , -- , Huggingface.toolPrompt = maybe Nothing toolPrompt mbHuggingfaceParams
            -- , Huggingface.topLogprobs = maybe Nothing topLogProbs mbHuggingfaceParams
            Huggingface.topP = topP =<< mbHuggingfaceParams
          , Huggingface.timeout = timeout =<< mbHuggingfaceParams
          -- , Huggingface.streamOptions = maybe Nothing streamOptions mbHuggingfaceParams
          -- , Huggingface.responseFormat = maybe Nothing responseFormat mbHuggingfaceParams
          -- , Huggingface.tools = maybe Nothing tools mbHuggingfaceParams
          -- , Huggingface.toolChoice = maybe Nothing toolChoice mbHuggingfaceParams
          }
    case eRes of
      Left err -> return $ Left (llmError (T.pack err) Nothing Nothing)
      Right r -> do
        case listToMaybe ((\Huggingface.ChatCompletionResponse {..} -> choices) r) of
          Nothing ->
            return $
              Left
                (llmError "Did not received any response" Nothing Nothing)
          Just resp ->
            let Huggingface.Message {..} = Huggingface.message resp
             in pure $
                  Right $
                    ( \case
                        Huggingface.TextContent t -> t
                        _ -> ""
                    )
                      content

  chat Huggingface {..} msgs mbHuggingfaceParams = do
    eRes <-
      Huggingface.createChatCompletion
        apiKey
        Huggingface.defaultHuggingfaceChatCompletionRequest
          { Huggingface.provider = provider
          , Huggingface.messages = toHuggingfaceMessages msgs
          , Huggingface.model = modelName
          , Huggingface.stream = False
          , Huggingface.maxTokens = maxTokens =<< mbHuggingfaceParams
          , Huggingface.frequencyPenalty = frequencyPenalty =<< mbHuggingfaceParams
          , -- , Huggingface.logProbs = maybe Nothing logProbs mbHuggingfaceParams
            Huggingface.presencePenalty = presencePenalty =<< mbHuggingfaceParams
          , -- , Huggingface.seed = maybe Nothing seed mbHuggingfaceParams
            Huggingface.stop = stop =<< mbHuggingfaceParams
          , Huggingface.temperature = temperature =<< mbHuggingfaceParams
          , -- , Huggingface.toolPrompt = maybe Nothing toolPrompt mbHuggingfaceParams
            -- , Huggingface.topLogprobs = maybe Nothing topLogProbs mbHuggingfaceParams
            Huggingface.topP = topP =<< mbHuggingfaceParams
          , Huggingface.timeout = timeout =<< mbHuggingfaceParams
          -- , Huggingface.streamOptions = maybe Nothing streamOptions mbHuggingfaceParams
          -- , Huggingface.responseFormat = maybe Nothing responseFormat mbHuggingfaceParams
          -- , Huggingface.tools = maybe Nothing tools mbHuggingfaceParams
          -- , Huggingface.toolChoice = maybe Nothing toolChoice mbHuggingfaceParams
          }
    case eRes of
      Left err -> return $ Left $ llmError (T.pack err) Nothing Nothing
      Right r -> do
        case listToMaybe
          ((\Huggingface.ChatCompletionResponse {..} -> choices) r) of
          Nothing ->
            return $
              Left (llmError "Did not received any response" Nothing Nothing)
          Just resp -> return $ Right $ from (Huggingface.message resp)

  stream Huggingface {..} msgs LLM.StreamHandler {..} mbHuggingfaceParams = do
    eRes <-
      Huggingface.createChatCompletionStream
        apiKey
        Huggingface.defaultHuggingfaceChatCompletionRequest
          { Huggingface.provider = provider
          , Huggingface.messages = toHuggingfaceMessages msgs
          , Huggingface.model = modelName
          , Huggingface.stream = True
          , Huggingface.maxTokens = maxTokens =<< mbHuggingfaceParams
          , Huggingface.frequencyPenalty = frequencyPenalty =<< mbHuggingfaceParams
          , -- , Huggingface.logProbs = maybe Nothing logProbs mbHuggingfaceParams
            Huggingface.presencePenalty = presencePenalty =<< mbHuggingfaceParams
          , -- , Huggingface.seed = maybe Nothing seed mbHuggingfaceParams
            Huggingface.stop = stop =<< mbHuggingfaceParams
          , Huggingface.temperature = temperature =<< mbHuggingfaceParams
          , -- , Huggingface.toolPrompt = maybe Nothing toolPrompt mbHuggingfaceParams
            -- , Huggingface.topLogprobs = maybe Nothing topLogProbs mbHuggingfaceParams
            Huggingface.topP = topP =<< mbHuggingfaceParams
          , Huggingface.timeout = timeout =<< mbHuggingfaceParams
          -- , Huggingface.streamOptions = maybe Nothing streamOptions mbHuggingfaceParams
          -- , Huggingface.responseFormat = maybe Nothing responseFormat mbHuggingfaceParams
          -- , Huggingface.tools = maybe Nothing tools mbHuggingfaceParams
          -- , Huggingface.toolChoice = maybe Nothing toolChoice mbHuggingfaceParams
          }
        Huggingface.HuggingfaceStreamHandler
          { Huggingface.onComplete = onComplete
          , Huggingface.onToken = onToken . chunkToText
          }
    case eRes of
      Left err -> pure $ Left $ llmError (T.pack err) Nothing Nothing
      Right r -> pure $ Right r
    where
      chunkToText :: Huggingface.ChatCompletionChunk -> Text
      chunkToText Huggingface.ChatCompletionChunk {..} = do
        case listToMaybe chunkChoices of
          Nothing -> ""
          Just Huggingface.ChoiceChunk {..} ->
            fromMaybe "" ((\Huggingface.Delta {..} -> deltaContent) delta)

toHuggingfaceMessages :: LLM.ChatMessage -> [Huggingface.Message]
toHuggingfaceMessages msgs = map go (NE.toList msgs)
  where
    toRole :: LLM.Role -> Huggingface.Role
    toRole r = case r of
      LLM.System -> Huggingface.System
      LLM.User -> Huggingface.User
      LLM.Assistant -> Huggingface.Assistant
      LLM.Tool -> Huggingface.Tool
      _ -> Huggingface.System
    -- LLM.Developer -> Huggingface.Developer
    -- LLM.Function -> Huggingface.Function

    go :: LLM.Message -> Huggingface.Message
    go msg =
      Huggingface.defaultMessage
        { Huggingface.role = toRole $ LLM.role msg
        , Huggingface.content = Huggingface.TextContent (LLM.content msg)
        }

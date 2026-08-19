{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: This is not tested

{- |
Module      : Langchain.Provider.OpenAI
Description : OpenAI provider implementing effect-polymorphic ChatModel
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

OpenAI and OpenAICompatible provider with multi-modal content and streaming support.
-}
module Langchain.Provider.OpenAI
  ( OpenAI (..)
  , OpenAIConfig (..)
  , defaultConfig
  , newOpenAI
  , openAICompatible
  , parseOpenAIResponse
  ) where

import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Conduit (yield)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Simple

import Langchain.Core.Error (llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..))

-- | Configuration for OpenAI provider
data OpenAIConfig = OpenAIConfig
  { configApiKey :: Text
  , configModel :: Text
  , configBaseUrl :: Maybe Text
  , configTemperature :: Maybe Double
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultConfig :: Text -> OpenAIConfig
defaultConfig key = OpenAIConfig key "gpt-4o" Nothing (Just 0.7)

-- | OpenAI ChatModel implementation
data OpenAI = OpenAI
  { apiKey :: Text
  , model :: Text
  , baseUrl :: Text
  , temperature :: Maybe Double
  }
  deriving (Eq, Show)

-- | Create standard OpenAI provider instance
newOpenAI :: Text -> Text -> OpenAI
newOpenAI key mName =
  OpenAI
    { apiKey = key
    , model = mName
    , baseUrl = "https://api.openai.com/v1/chat/completions"
    , temperature = Just 0.7
    }

-- | Create OpenAICompatible provider instance for OpenRouter/Fireworks/Together
openAICompatible :: Text -> Text -> Text -> OpenAI
openAICompatible key mName endpoint =
  OpenAI
    { apiKey = key
    , model = mName
    , baseUrl = endpoint
    , temperature = Just 0.7
    }

-- Helper to convert core Role to OpenAI role string
roleToText :: Role -> Text
roleToText System = "system"
roleToText User = "user"
roleToText Assistant = "assistant"
roleToText Tool = "tool"
roleToText Developer = "developer"
roleToText Function = "function"

-- Helper to format content block into OpenAI JSON object
contentBlockToValue :: ContentBlock -> Value
contentBlockToValue (TextBlock t) =
  object ["type" .= ("text" :: Text), "text" .= t]
contentBlockToValue (ImageBlock mime b64) =
  object
    [ "type" .= ("image_url" :: Text)
    , "image_url" .= object ["url" .= ("data:" <> mime <> ";base64," <> b64)]
    ]
contentBlockToValue (AudioBlock mime _) =
  object ["type" .= ("text" :: Text), "text" .= ("[Audio block " <> mime <> "]")]
contentBlockToValue (DataBlock _) =
  object ["type" .= ("text" :: Text), "text" .= ("[Data block]" :: Text)]

-- Convert Message to OpenAI JSON payload
messageToValue :: Message -> Value
messageToValue msg =
  let r = roleToText (messageRole msg)
      blocks = map contentBlockToValue (NonEmpty.toList (messageContents msg))
   in object
        [ "role" .= r
        , "content" .= blocks
        ]

instance ChatModel OpenAI where
  type ModelConfig OpenAI = Value

  invoke provider inputMsgs _ = do
    let payload =
          object
            [ "model" .= model provider
            , "messages" .= map messageToValue inputMsgs
            , "temperature" .= temperature provider
            ]
        initReq = parseRequest_ (T.unpack (baseUrl provider))
        req =
          setRequestMethod "POST" $
            setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 (apiKey provider)] $
              setRequestHeader "Content-Type" ["application/json"] $
                setRequestBodyJSON payload initReq

    eRes <- liftIO $ safeHttpRequest req
    case eRes of
      Left err -> throwError $ llmError err Nothing Nothing
      Right bodyVal -> case parseOpenAIResponse bodyVal of
        Left parseErr -> throwError $ llmError (T.pack parseErr) Nothing Nothing
        Right (respMsg, _) -> pure respMsg

  stream provider inputMsgs _ = do
    let rId = "openai-stream-run"
    yield $ LLMStart rId (model provider) inputMsgs
    let payload =
          object
            [ "model" .= model provider
            , "messages" .= map messageToValue inputMsgs
            , "temperature" .= temperature provider
            ]
        initReq = parseRequest_ (T.unpack (baseUrl provider))
        req =
          setRequestMethod "POST" $
            setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 (apiKey provider)] $
              setRequestHeader "Content-Type" ["application/json"] $
                setRequestBodyJSON payload initReq

    eRes <- liftIO $ safeHttpRequest req
    case eRes of
      Left err -> yield $ LLMChunk rId err Nothing
      Right bodyVal -> case parseOpenAIResponse bodyVal of
        Left parseErr -> yield $ LLMChunk rId (T.pack parseErr) Nothing
        Right (respMsg, mbUsage) -> do
          yield $ LLMChunk rId (extractMessageText respMsg) Nothing
          yield $ LLMEnd rId respMsg mbUsage

-- Helper for HTTP requests
safeHttpRequest :: Request -> IO (Either Text Value)
safeHttpRequest req = do
  res <- httpJSONEither req
  case getResponseBody res of
    Left err -> pure $ Left (T.pack $ show err)
    Right val -> pure $ Right val

-- Helper for parsing OpenAI response JSON
parseOpenAIResponse :: Value -> Either String (Message, Maybe TokenUsage)
parseOpenAIResponse = parseEither $ withObject "OpenAIResponse" $ \o -> do
  choices <- o .: "choices"
  usageVal <- o .:? "usage"
  mbUsage <- case usageVal of
    Nothing -> pure Nothing
    Just u -> flip (withObject "Usage") u $ \uo -> do
      pTok <- uo .:? "prompt_tokens" .!= 0
      cTok <- uo .:? "completion_tokens" .!= 0
      tTok <- uo .:? "total_tokens" .!= 0
      pure $ Just $ TokenUsage pTok cTok tTok
  case choices of
    [] -> fail "Empty choices array in OpenAI response"
    (c : _) -> flip (withObject "Choice") c $ \ch -> do
      msgObj <- ch .: "message"
      contentTxt <- msgObj .:? "content" .!= ""
      mbToolCalls <- msgObj .:? "tool_calls"
      cToolCalls <- case mbToolCalls of
        Nothing -> pure Nothing
        Just tcs -> do
          calls <- forM (tcs :: [Value]) $ withObject "ToolCall" $ \tcObj -> do
            tcId <- tcObj .:? "id" .!= ""
            fnObj <- tcObj .: "function"
            fnName <- fnObj .: "name"
            fnArgs <- fnObj .:? "arguments" .!= object []
            pure $ ToolCall tcId "function" fnName fnArgs
          pure (Just calls)
      let msg = (assistantMessage contentTxt) {messageToolCalls = cToolCalls}
      pure (msg, mbUsage)

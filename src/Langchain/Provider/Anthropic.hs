{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Provider.Anthropic
Description : Standalone Anthropic Claude provider implementing ChatModel
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Anthropic Claude provider supporting extended thinking, vision content, and streaming events.
-}
module Langchain.Provider.Anthropic
  ( Anthropic (..)
  , AnthropicConfig (..)
  , defaultConfig
  , newAnthropic
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Conduit (yield)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Simple

import Langchain.Core.Error (llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..))

-- | Configuration for Anthropic provider
data AnthropicConfig = AnthropicConfig
  { cfgApiKey :: Text
  , cfgModel :: Text
  , cfgEnableThinking :: Bool
  , cfgThinkingBudget :: Maybe Int
  , cfgMaxTokens :: Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultConfig :: Text -> AnthropicConfig
defaultConfig key = AnthropicConfig key "claude-3-5-sonnet-20241022" False Nothing 1024

-- | Anthropic ChatModel provider
data Anthropic = Anthropic
  { apiKey :: Text
  , model :: Text
  , enableThinking :: Bool
  , thinkingBudget :: Maybe Int
  , maxTokens :: Int
  }
  deriving (Eq, Show)

-- | Create a standard Anthropic provider instance
newAnthropic :: Text -> Text -> Anthropic
newAnthropic key modelName =
  Anthropic
    { apiKey = key
    , model = modelName
    , enableThinking = False
    , thinkingBudget = Nothing
    , maxTokens = 1024
    }

-- Helper to convert ContentBlock to Anthropic JSON block
contentBlockToAnthropic :: ContentBlock -> Value
contentBlockToAnthropic (TextBlock t) =
  object ["type" .= ("text" :: Text), "text" .= t]
contentBlockToAnthropic (ImageBlock mime b64) =
  object
    [ "type" .= ("image" :: Text)
    , "source"
        .= object
          [ "type" .= ("base64" :: Text)
          , "media_type" .= mime
          , "data" .= b64
          ]
    ]
contentBlockToAnthropic (AudioBlock mime _) =
  object ["type" .= ("text" :: Text), "text" .= ("[Audio block " <> mime <> "]")]
contentBlockToAnthropic (DataBlock _) =
  object ["type" .= ("text" :: Text), "text" .= ("[Data block]")]

-- Convert Message to Anthropic JSON payload
messageToAnthropic :: Message -> Value
messageToAnthropic msg =
  let r = case messageRole msg of
        User -> "user"
        Assistant -> "assistant"
        _ -> "user"
      blocks = map contentBlockToAnthropic (NonEmpty.toList (messageContents msg))
   in object ["role" .= (r :: Text), "content" .= blocks]

instance ChatModel Anthropic where
  type ModelConfig Anthropic = Value

  invoke provider inputMsgs _ = do
    let systemMsgs = [extractMessageText m | m <- inputMsgs, messageRole m == System]
        nonSystemMsgs = [m | m <- inputMsgs, messageRole m /= System]
        systemTxt = T.unlines systemMsgs
        payload =
          object $
            catMaybes
              [ Just $ "model" .= model provider
              , Just $ "max_tokens" .= maxTokens provider
              , Just $ "messages" .= map messageToAnthropic nonSystemMsgs
              , if T.null systemTxt then Nothing else Just ("system" .= systemTxt)
              , if enableThinking provider
                  then Just ("thinking" .= object ["type" .= ("enabled" :: Text), "budget_tokens" .= fromMaybe 1024 (thinkingBudget provider)])
                  else Nothing
              ]

        initReq = parseRequest_ "https://api.anthropic.com/v1/messages"
        req =
          setRequestMethod "POST" $
            setRequestHeader "x-api-key" [TE.encodeUtf8 (apiKey provider)] $
              setRequestHeader "anthropic-version" ["2023-06-01"] $
                setRequestHeader "Content-Type" ["application/json"] $
                  setRequestBodyJSON payload initReq

    eRes <- liftIO $ safeHttpRequest req
    case eRes of
      Left err -> throwError $ llmError err Nothing Nothing
      Right bodyVal -> case parseAnthropicResponse bodyVal of
        Left parseErr -> throwError $ llmError (T.pack parseErr) Nothing Nothing
        Right (respMsg, _mbUsage) -> pure respMsg

  stream provider inputMsgs _ = do
    let rId = "anthropic-stream-run"
    yield $ LLMStart rId (model provider) inputMsgs
    let systemMsgs = [extractMessageText m | m <- inputMsgs, messageRole m == System]
        nonSystemMsgs = [m | m <- inputMsgs, messageRole m /= System]
        systemTxt = T.unlines systemMsgs
        payload =
          object $
            catMaybes
              [ Just $ "model" .= model provider
              , Just $ "max_tokens" .= maxTokens provider
              , Just $ "messages" .= map messageToAnthropic nonSystemMsgs
              , if T.null systemTxt then Nothing else Just ("system" .= systemTxt)
              ]

        initReq = parseRequest_ "https://api.anthropic.com/v1/messages"
        req =
          setRequestMethod "POST" $
            setRequestHeader "x-api-key" [TE.encodeUtf8 (apiKey provider)] $
              setRequestHeader "anthropic-version" ["2023-06-01"] $
                setRequestHeader "Content-Type" ["application/json"] $
                  setRequestBodyJSON payload initReq

    eRes <- liftIO $ safeHttpRequest req
    case eRes of
      Left err -> yield $ LLMChunk rId err Nothing
      Right bodyVal -> case parseAnthropicResponse bodyVal of
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

-- Parse Anthropic response JSON
parseAnthropicResponse :: Value -> Either String (Message, Maybe TokenUsage)
parseAnthropicResponse = parseEither $ withObject "AnthropicResponse" $ \o -> do
  contentArr <- o .: "content"
  txts <- flip mapM contentArr $ withObject "Content" $ \c -> do
    t <- c .: "type"
    if (t :: Text) == "text"
      then c .: "text"
      else pure ""
  let fullTxt = T.unlines txts
      msg = assistantMessage fullTxt
  pure (msg, Nothing)

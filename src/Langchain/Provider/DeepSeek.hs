{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Provider.DeepSeek
Description : DeepSeek provider with reasoning chain (<think>...</think>) extraction
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

DeepSeek provider supporting R1 reasoning models and V3 chat models.
-}
module Langchain.Provider.DeepSeek
  ( DeepSeek (..)
  , DeepSeekConfig (..)
  , defaultConfig
  , newDeepSeek
  , extractReasoningChain
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Conduit (yield)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Simple

import Langchain.Core.Error (LangchainError, llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..))

-- | DeepSeek provider configuration
data DeepSeekConfig = DeepSeekConfig
  { configApiKey :: Text
  , configModel :: Text
  , configBaseUrl :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultConfig :: Text -> DeepSeekConfig
defaultConfig key = DeepSeekConfig key "deepseek-reasoner" "https://api.deepseek.com/chat/completions"

-- | DeepSeek ChatModel provider
data DeepSeek = DeepSeek
  { apiKey :: Text
  , model :: Text
  , baseUrl :: Text
  }
  deriving (Eq, Show)

-- | Create a new DeepSeek provider instance
newDeepSeek :: Text -> Text -> DeepSeek
newDeepSeek key modelName = DeepSeek key modelName "https://api.deepseek.com/chat/completions"

-- Extract reasoning chain (<think>...</think>) from output text
extractReasoningChain :: Text -> (Maybe Text, Text)
extractReasoningChain txt =
  case T.breakOn "<think>" txt of
    (beforeTag, rest) ->
      if T.null rest
        then (Nothing, txt)
        else
          let afterStart = T.drop (T.length ("<think>" :: Text)) rest
           in case T.breakOn "</think>" afterStart of
                (thinkTxt, afterEnd) ->
                  let cleanAfter = T.drop (T.length ("</think>" :: Text)) afterEnd
                   in (Just thinkTxt, T.strip (beforeTag <> cleanAfter))

-- Convert ContentBlock to DeepSeek JSON block
contentBlockToValue :: ContentBlock -> Value
contentBlockToValue (TextBlock t) =
  object ["type" .= ("text" :: Text), "text" .= t]
contentBlockToValue _ =
  object ["type" .= ("text" :: Text), "text" .= ("[Unsupported block]" :: Text)]

-- Convert Message to DeepSeek JSON message
messageToValue :: Message -> Value
messageToValue msg =
  let r = case messageRole msg of
        User -> "user"
        Assistant -> "assistant"
        System -> "system"
        _ -> "user"
      blocks = map contentBlockToValue (NonEmpty.toList (messageContents msg))
   in object ["role" .= (r :: Text), "content" .= blocks]

instance ChatModel DeepSeek where
  type ModelConfig DeepSeek = Value

  invoke provider inputMsgs _ = do
    let payload =
          object
            [ "model" .= model provider
            , "messages" .= map messageToValue inputMsgs
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
      Right bodyVal -> case parseDeepSeekResponse bodyVal of
        Left parseErr -> throwError $ llmError (T.pack parseErr) Nothing Nothing
        Right respMsg -> pure respMsg

  stream provider inputMsgs _ = do
    let rId = "deepseek-stream-run"
    yield $ LLMStart rId (model provider) inputMsgs
    let payload =
          object
            [ "model" .= model provider
            , "messages" .= map messageToValue inputMsgs
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
      Right bodyVal -> case parseDeepSeekResponse bodyVal of
        Left parseErr -> yield $ LLMChunk rId (T.pack parseErr) Nothing
        Right respMsg -> do
          yield $ LLMChunk rId (extractMessageText respMsg) Nothing
          yield $ LLMEnd rId respMsg Nothing

-- Helper for HTTP requests
safeHttpRequest :: Request -> IO (Either Text Value)
safeHttpRequest req = do
  res <- httpJSONEither req
  case getResponseBody res of
    Left err -> pure $ Left (T.pack $ show err)
    Right val -> pure $ Right val

-- Parse DeepSeek response JSON and extract reasoning
parseDeepSeekResponse :: Value -> Either String Message
parseDeepSeekResponse = parseEither $ withObject "DeepSeekResponse" $ \o -> do
  choices <- o .: "choices"
  case choices of
    [] -> fail "Empty choices array in DeepSeek response"
    (c : _) -> flip (withObject "Choice") c $ \ch -> do
      msgObj <- ch .: "message"
      contentTxt <- msgObj .:? "content" .!= ""
      reasoningTxt <- msgObj .:? "reasoning_content" .!= ""
      let (mbThink, cleanTxt) = extractReasoningChain contentTxt
          finalThink = case mbThink of
            Just t -> Just t
            Nothing -> if T.null reasoningTxt then Nothing else Just reasoningTxt
          baseMsg = assistantMessage (if T.null cleanTxt then contentTxt else cleanTxt)
      pure baseMsg

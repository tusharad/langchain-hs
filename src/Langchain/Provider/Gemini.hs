{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Provider.Gemini
Description : Google Gemini provider implementing ChatModel
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Gemini provider with multi-modal content parts support.
-}
module Langchain.Provider.Gemini
  ( Gemini (..)
  , GeminiConfig (..)
  , defaultConfig
  , defaultGeminiConfig
  , newGemini
  , parseGeminiResponse
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Conduit (yield)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Simple

import Langchain.Core.Error (llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..))

-- | Gemini configuration
data GeminiConfig = GeminiConfig
  { configApiKey :: Text
  , configModel :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultConfig :: Text -> GeminiConfig
defaultConfig key = GeminiConfig key "gemini-1.5-pro"

defaultGeminiConfig :: Text -> GeminiConfig
defaultGeminiConfig = defaultConfig

-- | Gemini ChatModel provider
data Gemini = Gemini
  { apiKey :: Text
  , model :: Text
  }
  deriving (Eq, Show)

-- | Create a new Gemini provider instance
newGemini :: Text -> Text -> Gemini
newGemini = Gemini

-- Convert ContentBlock to Gemini Part JSON
contentBlockToPart :: ContentBlock -> Value
contentBlockToPart (TextBlock t) =
  object ["text" .= t]
contentBlockToPart (ImageBlock ImageContent {imageSource = ImageBase64 (Just mime) b64}) =
  object
    [ "inline_data"
        .= object
          [ "mime_type" .= mime
          , "data" .= b64
          ]
    ]
contentBlockToPart (ImageBlock ImageContent {imageSource = ImageUrl url}) =
  object ["text" .= ("[Image URL: " <> url <> "]")]
contentBlockToPart (ImageBlock ImageContent {imageSource = ImageBase64 Nothing _}) =
  object ["text" .= ("[Image data block: base64]" :: Text)]
contentBlockToPart (AudioBlock mime b64) =
  object
    [ "inline_data"
        .= object
          [ "mime_type" .= mime
          , "data" .= b64
          ]
    ]
contentBlockToPart (DataBlock _) =
  object ["text" .= ("[Data block]" :: Text)]

-- Convert Message to Gemini Content JSON
messageToGemini :: Message -> Value
messageToGemini msg =
  let r = case messageRole msg of
        User -> "user"
        Assistant -> "model"
        System -> "user"
        _ -> "user"
      parts = map contentBlockToPart (NonEmpty.toList (messageContents msg))
   in object ["role" .= (r :: Text), "parts" .= parts]

instance ChatModel Gemini where
  type ModelConfig Gemini = Value

  invoke provider inputMsgs _ = do
    let contentsPayload = map messageToGemini inputMsgs
        payload = object ["contents" .= contentsPayload]
        url =
          "https://generativelanguage.googleapis.com/v1beta/models/"
            <> model provider
            <> ":generateContent?key="
            <> apiKey provider
        initReq = parseRequest_ (T.unpack url)
        req =
          setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyJSON payload initReq

    eRes <- liftIO $ safeHttpRequest req
    case eRes of
      Left err -> throwError $ llmError err Nothing Nothing
      Right bodyVal -> case parseGeminiResponse bodyVal of
        Left parseErr -> throwError $ llmError (T.pack parseErr) Nothing Nothing
        Right respMsg -> pure respMsg

  stream provider inputMsgs _ = do
    let rId = "gemini-stream-run"
    yield $ LLMStart rId (model provider) inputMsgs
    let contentsPayload = map messageToGemini inputMsgs
        payload = object ["contents" .= contentsPayload]
        url =
          "https://generativelanguage.googleapis.com/v1beta/models/"
            <> model provider
            <> ":generateContent?key="
            <> apiKey provider
        initReq = parseRequest_ (T.unpack url)
        req =
          setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyJSON payload initReq

    eRes <- liftIO $ safeHttpRequest req
    case eRes of
      Left err -> yield $ LLMChunk rId err Nothing
      Right bodyVal -> case parseGeminiResponse bodyVal of
        Left parseErr -> yield $ LLMChunk rId (T.pack parseErr) Nothing
        Right respMsg -> do
          yield $ LLMChunk rId (extractMessageText respMsg) Nothing
          yield $ LLMEnd rId respMsg Nothing

-- Helper for HTTP requests
safeHttpRequest :: Request -> IO (Either Text Value)
safeHttpRequest req = do
  eRes <-
    try (httpJSONEither req) :: IO (Either SomeException (Response (Either JSONException Value)))
  case eRes of
    Left ex -> pure $ Left (T.pack $ show ex)
    Right res -> case getResponseBody res of
      Left err -> pure $ Left (T.pack $ show err)
      Right val -> pure $ Right val

-- Parse Gemini response JSON
parseGeminiResponse :: Value -> Either String Message
parseGeminiResponse = parseEither $ withObject "GeminiResponse" $ \o -> do
  candidates <- o .: "candidates"
  case candidates of
    [] -> fail "Empty candidates array in Gemini response"
    (c : _) -> flip (withObject "Candidate") c $ \cand -> do
      contentObj <- cand .: "content"
      parts <- contentObj .: "parts"
      txts <- forM parts $ withObject "Part" $ \p -> p .:? "text" .!= ""
      pure $ assistantMessage (T.intercalate "\n" txts)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  , geminiWithBaseUrl
  , parseGeminiResponse
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Conduit (ConduitT, await, runConduit, yield, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import qualified Data.Proxy as Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
import Servant.API (Capture, JSON, QueryParam, ReqBody, (:>))
import Servant.API.EventStream
  ( FromServerEvent (fromServerEvent)
  , PostServerSentEvents
  , jsonData
  )
import Servant.Client.Core.BaseUrl (parseBaseUrl)
import Servant.Client.Streaming (ClientM, client, mkClientEnv, withClientM)
import Servant.Conduit ()

import Langchain.Core.Error (llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..), callbackSource)

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
data Gemini
  = Gemini
      { apiKey :: Text
      , model :: Text
      }
  | GeminiWithBaseUrl
      { apiKey :: Text
      , model :: Text
      , baseUrl :: Text
      }
  deriving (Eq, Show)

-- | Create a new Gemini provider instance
newGemini :: Text -> Text -> Gemini
newGemini = Gemini

{- | Create a Gemini provider with a custom API base URL.

This is primarily useful for local test servers. 'newGemini' remains the
standard constructor and preserves its existing two-argument form.
-}
geminiWithBaseUrl :: Text -> Text -> Text -> Gemini
geminiWithBaseUrl = GeminiWithBaseUrl

geminiApiKey :: Gemini -> Text
geminiApiKey = apiKey

geminiModel :: Gemini -> Text
geminiModel = model

geminiBaseUrl :: Gemini -> Text
geminiBaseUrl Gemini {} = "https://generativelanguage.googleapis.com"
geminiBaseUrl GeminiWithBaseUrl {baseUrl} = T.dropWhileEnd (== '/') baseUrl

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
          geminiBaseUrl provider
            <> "/v1beta/models/"
            <> geminiModel provider
            <> ":generateContent?key="
            <> geminiApiKey provider
        initReq = parseRequest_ (T.unpack url)
        req =
          setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestBodyJSON payload initReq

    eRes <- liftIO $ safeHttpRequest req
    case eRes of
      Left err -> throwError $ llmError err Nothing Nothing
      Right bodyVal -> case parseGeminiResponse bodyVal of
        Left parseErr -> throwError $ llmError (T.pack parseErr) Nothing Nothing
        Right respMsg -> pure respMsg

  stream provider inputMsgs _ = do
    yield $ LLMStart rId (geminiModel provider) inputMsgs
    (accumulated, usage) <-
      callbackSource geminiEvents
        .| receiveChunks "" Nothing
    yield $ LLMEnd rId (assistantMessage accumulated) usage
    where
      receiveChunks accumulated usage = do
        next <- await
        case next of
          Nothing -> pure (accumulated, usage)
          Just (Left err) -> throwError $ llmError err Nothing Nothing
          Just (Right (GeminiStreamEvent GeminiStreamChunk {streamCandidates, streamUsage})) -> do
            let texts = maybe [] streamParts $ candidate0 streamCandidates
                nextUsage = streamUsage <|> usage
            mapM_ (\text -> yield $ LLMChunk rId text Nothing) texts
            receiveChunks (accumulated <> mconcat texts) nextUsage

      candidate0 = List.find ((== 0) . streamCandidateIndex)

      geminiEvents emit = do
        result <- try $ do
          manager <- newManager tlsManagerSettings
          let baseUrl = parseBaseUrl (T.unpack $ geminiBaseUrl provider)
          clientEnv <- mkClientEnv manager <$> baseUrl
          let payload = object ["contents" .= map messageToGemini inputMsgs]
              request =
                geminiStreamClient
                  (geminiModel provider <> ":streamGenerateContent")
                  (Just "sse")
                  (Just $ geminiApiKey provider)
                  payload
          withClientM request clientEnv $ \case
            Left err -> emit . Left . T.pack $ show err
            Right source -> runConduit $ source .| C.mapM_ (emit . Right)
        case result of
          Left err
            | Just AsyncCancelled <- fromException err -> throwIO err
            | otherwise -> emit . Left . T.pack $ show err
          Right () -> pure ()

      rId = "gemini-stream-run"

data GeminiStreamChunk = GeminiStreamChunk
  { streamCandidates :: [GeminiStreamCandidate]
  , streamUsage :: Maybe TokenUsage
  }

instance FromJSON GeminiStreamChunk where
  parseJSON = withObject "GeminiStreamChunk" $ \obj ->
    GeminiStreamChunk
      <$> obj .:? "candidates" .!= []
      <*> (obj .:? "usageMetadata" >>= traverse parseGeminiUsage)

data GeminiStreamCandidate = GeminiStreamCandidate
  { streamCandidateIndex :: Int
  , streamParts :: [Text]
  }

instance FromJSON GeminiStreamCandidate where
  parseJSON = withObject "GeminiStreamCandidate" $ \obj -> do
    streamCandidateIndex <- obj .:? "index" .!= 0
    content <- obj .:? "content"
    streamParts <- case content of
      Nothing -> pure []
      Just contentValue -> withObject "GeminiStreamContent" parseParts contentValue
    pure GeminiStreamCandidate {streamCandidateIndex, streamParts}
    where
      parseParts contentObj = do
        parts <- contentObj .:? "parts" .!= []
        catMaybes <$> traverse (withObject "GeminiStreamPart" (.:? "text")) parts

parseGeminiUsage :: Value -> Parser TokenUsage
parseGeminiUsage = withObject "GeminiUsageMetadata" $ \obj ->
  TokenUsage
    <$> obj .:? "promptTokenCount" .!= 0
    <*> obj .:? "candidatesTokenCount" .!= 0
    <*> obj .:? "totalTokenCount" .!= 0

data GeminiStreamEvent = GeminiStreamEvent GeminiStreamChunk

instance FromServerEvent GeminiStreamEvent where
  fromServerEvent event = GeminiStreamEvent <$> jsonData event

type GeminiStreamApi =
  "v1beta"
    :> "models"
    :> Capture "modelAction" Text
    :> QueryParam "alt" Text
    :> QueryParam "key" Text
    :> ReqBody '[JSON] Value
    :> PostServerSentEvents (ConduitT () GeminiStreamEvent IO ())

geminiStreamClient ::
  Text -> Maybe Text -> Maybe Text -> Value -> ClientM (ConduitT () GeminiStreamEvent IO ())
geminiStreamClient = client (Proxy.Proxy :: Proxy.Proxy GeminiStreamApi)

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

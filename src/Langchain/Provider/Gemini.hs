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
  , geminiTools
  , parseGeminiResponse
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Conduit (ConduitT, await, runConduit, yield, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
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

import Langchain.Core.Error (LangchainError, llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..), callbackSource)
import qualified Langchain.Core.Tool as CoreTool

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

-- | Build Gemini tool declarations from langchain tool definitions.
geminiTools :: [CoreTool.Tool m] -> Value
geminiTools tools =
  object
    [ "tools"
        .= [ object ["functionDeclarations" .= map functionDeclaration tools]
           ]
    ]
  where
    functionDeclaration tool =
      object
        [ "name" .= CoreTool.toolName tool
        , "description" .= CoreTool.toolDescription tool
        , "parameters" .= CoreTool.toolSchema tool
        ]

-- Convert a non-tool Message to Gemini Content JSON.
messageToGemini :: Message -> Value
messageToGemini msg =
  let role = messageRole msg
      geminiRole = case role of
        User -> "user"
        Assistant -> "model"
        System -> "user"
        Developer -> "user"
        Tool -> "user"
        Function -> "user"
      toolCallParts = case role of
        Assistant -> maybe [] (map functionCallPart) (messageToolCalls msg)
        _ -> []
      contentBlocks = NonEmpty.toList (messageContents msg)
      contentParts = map contentBlockToPart contentBlocks
      parts
        | null toolCallParts = contentParts
        | otherwise = map contentBlockToPart (filter (not . emptyTextPart) contentBlocks) <> toolCallParts
   in object ["role" .= (geminiRole :: Text), "parts" .= parts]
  where
    functionCallPart (ToolCall {toolCallName = name, toolCallArguments = args, toolCallId = callId}) =
      object
        [ "functionCall"
            .= object
              ( [ "name" .= name
                , "args" .= args
                ]
                  <> (["id" .= callId | notNull callId])
              )
        ]

    notNull = not . T.null

    emptyTextPart (TextBlock text) = T.null text
    emptyTextPart _ = False

functionResponsePart :: [ToolCall] -> Message -> Either Text Value
functionResponsePart priorToolCalls msg = do
  toolName <-
    maybe
      (Left "Gemini function response is missing a function name")
      Right
      (messageName msg <|> (messageToolId msg >>= lookupToolName))
  let functionResponseFields =
        [ "name" .= toolName
        , "response" .= object ["result" .= extractMessageText msg]
        ]
          <> maybe [] (pure . ("id" .=)) (messageToolId msg)
  pure $ object ["functionResponse" .= object functionResponseFields]
  where
    lookupToolName toolId =
      toolCallName <$> List.find ((== toolId) . toolCallId) priorToolCalls

messagesToGemini :: [ToolCall] -> [Message] -> Either Text [Value]
messagesToGemini _ [] = Right []
messagesToGemini priorToolCalls (msg : remaining)
  | isFunctionResponse msg = do
      let (responseMessages, followingMessages) = span isFunctionResponse remaining
      parts <- traverse (functionResponsePart priorToolCalls) (msg : responseMessages)
      contents <- messagesToGemini priorToolCalls followingMessages
      pure $ object ["role" .= ("user" :: Text), "parts" .= parts] : contents
  | otherwise = do
      contents <- messagesToGemini priorToolCalls remaining
      pure $ messageToGemini msg : contents
  where
    isFunctionResponse message = messageRole message `elem` [Tool, Function]

geminiRequestPayload :: [Message] -> Maybe Value -> Either Text Value
geminiRequestPayload inputMsgs config = do
  let priorToolCalls = concatMap (fromMaybe [] . messageToolCalls) inputMsgs
  contents <- messagesToGemini priorToolCalls inputMsgs
  case config of
    Just (Object fields) -> pure $ Object $ KeyMap.insert "contents" (toJSON contents) fields
    Nothing -> pure $ object ["contents" .= contents]
    Just _ -> Left "Gemini config must be a JSON object"

instance ChatModel Gemini where
  type ModelConfig Gemini = Value

  invoke provider inputMsgs config = do
    payload <-
      either (throwError . \err -> llmError err Nothing Nothing) pure $
        geminiRequestPayload inputMsgs config
    let url =
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

  stream provider inputMsgs config = do
    let model = geminiModel provider
        requestPayload = geminiRequestPayload inputMsgs
    yield $ LLMStart rId model inputMsgs

    payload <-
      either (throwError . llmError') pure $ requestPayload config

    let events = geminiEvents payload
    (accumulated, toolCalls, usage) <-
      callbackSource events
        .| receiveChunks "" [] Nothing

    let message = (assistantMessage accumulated) {messageToolCalls = nonEmpty toolCalls}
    yield $ LLMEnd rId message usage
    where
      receiveChunks accumulated toolCalls usage = do
        next <- await
        case next of
          Nothing -> pure (accumulated, toolCalls, usage)
          Just (Left err) -> throwError $ llmError' err
          Just (Right (GeminiStreamEvent GeminiStreamChunk {streamCandidates, streamUsage})) -> do
            let parts = maybe [] streamParts $ candidate0 streamCandidates
                texts = [text | GeminiText text <- parts]
                calls = [toolCall | GeminiFunctionCall toolCall <- parts]
                nextUsage = streamUsage <|> usage
            emitParts texts calls
            receiveChunks (accumulated <> mconcat texts) (toolCalls <> calls) nextUsage

      candidate0 = List.find ((== 0) . streamCandidateIndex)

      emitParts texts [] = mapM_ (`yieldChunk` Nothing) texts
      emitParts texts (toolCall : remaining) = do
        yieldChunk (mconcat texts) (Just toolCall)
        mapM_ (yieldChunk "" . Just) remaining

      yieldChunk text mbToolCall = yield $ LLMChunk rId text mbToolCall

      nonEmpty [] = Nothing
      nonEmpty calls = Just calls

      geminiEvents requestPayload emit = do
        result <- try $ do
          manager <- newManager tlsManagerSettings
          let baseUrl = parseBaseUrl (T.unpack $ geminiBaseUrl provider)
          clientEnv <- mkClientEnv manager <$> baseUrl
          let request =
                geminiStreamClient
                  (geminiModel provider <> ":streamGenerateContent")
                  (Just "sse")
                  (Just $ geminiApiKey provider)
                  requestPayload
          withClientM request clientEnv $ \case
            Left err -> emit . Left . T.pack $ show err
            Right source -> runConduit $ source .| C.mapM_ (emit . Right)
        case result of
          Left err
            | Just AsyncCancelled <- fromException err -> throwIO err
            | otherwise -> emit . Left . T.pack $ show err
          Right () -> pure ()

      rId = "gemini-stream-run"

llmError' :: Text -> LangchainError
llmError' err = llmError err Nothing Nothing

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
  , streamParts :: [GeminiPart]
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
        traverse parseGeminiPart parts

data GeminiPart
  = GeminiText Text
  | GeminiFunctionCall ToolCall

parseGeminiPart :: Value -> Parser GeminiPart
parseGeminiPart = withObject "GeminiPart" $ \obj -> do
  functionCall <- obj .:? "functionCall"
  case functionCall of
    Just value -> GeminiFunctionCall <$> parseGeminiFunctionCall value
    Nothing -> GeminiText <$> obj .:? "text" .!= ""

parseGeminiFunctionCall :: Value -> Parser ToolCall
parseGeminiFunctionCall = withObject "GeminiFunctionCall" $ \obj ->
  ToolCall
    <$> obj .:? "id" .!= ""
    <*> pure "function"
    <*> obj .: "name"
    <*> obj .:? "args" .!= object []

parseGeminiUsage :: Value -> Parser TokenUsage
parseGeminiUsage = withObject "GeminiUsageMetadata" $ \obj ->
  TokenUsage
    <$> obj .:? "promptTokenCount" .!= 0
    <*> obj .:? "candidatesTokenCount" .!= 0
    <*> obj .:? "totalTokenCount" .!= 0

newtype GeminiStreamEvent = GeminiStreamEvent GeminiStreamChunk

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
      parsedParts <- traverse parseGeminiPart parts
      let texts = [text | GeminiText text <- parsedParts]
          toolCalls = [toolCall | GeminiFunctionCall toolCall <- parsedParts]
      pure $
        (assistantMessage $ T.intercalate "\n" texts)
          { messageToolCalls = case toolCalls of
              [] -> Nothing
              calls -> Just calls
          }

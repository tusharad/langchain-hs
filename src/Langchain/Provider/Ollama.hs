{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Provider.Ollama
Description : Ollama provider implementing the effect-polymorphic ChatModel typeclass
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Ollama provider using 'ollama-haskell' 0.4.0.0. Supports native structured outputs
via JSON Schema grammar sampling, streaming, tool calling, and embeddings.
-}
module Langchain.Provider.Ollama
  ( Ollama (..)
  , newOllama
  , newOllamaWithConfig
  , newOllamaWithTimeout
  , newOllamaWithEndpoint
  , newOllamaWithClient
  , defaultOllama
  , OllamaConfig (..)
  , defaultConfig
  , defaultOllamaConfig
  , toOllamaRole
  , fromOllamaRole
  , toOllamaMessage
  , fromOllamaMessage
  , withJsonFormat
  , withSchemaFormat
  , withStructuredOutput
  , structuredOllamaInvoke
  , structuredOllamaInvokeWithSchema

    -- * Re-exports from ollama-haskell format & schema
  , OFormat.Format (..)
  , OSB.Schema (..)
  , OSB.Property (..)
  , OSB.JsonType (..)
  , OSD.ToSchema (..)
  , OSD.ToJsonType (..)
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, Result (..), decode, fromJSON, toJSON)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Conduit (yield)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Langchain.Core.Error (LangchainError, llmError, parsingError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..))
import Langchain.OutputParser.Structured
  ( StructuredOutput (..)
  , extractJsonFromMarkdown
  , toOllamaSchema
  )

import qualified Ollama.API.Chat as OllamaChat
import Ollama.Client (OllamaClient, defaultClient, newClient)
import qualified Ollama.Client.Config as OConfig
import Ollama.Types.Common (Base64Image (..), ModelName (..))
import qualified Ollama.Types.Format as OFormat
import qualified Ollama.Types.Format.SchemaBuilder as OSB
import qualified Ollama.Types.Format.SchemaDerive as OSD
import qualified Ollama.Types.Message as O
import qualified Ollama.Types.Tool as OTool

-- | Configuration options for Ollama provider
data OllamaConfig = OllamaConfig
  { configModelName :: Text
  , configBaseUrl :: Maybe Text
  , configTimeout :: Maybe Int
  , configKeepAlive :: Maybe Text
  , configApiKey :: Maybe Text
  }
  deriving (Eq, Show)

defaultConfig :: OllamaConfig
defaultConfig =
  OllamaConfig
    { configModelName = "gemma3:latest"
    , configBaseUrl = Nothing
    , configTimeout = Nothing
    , configKeepAlive = Nothing
    , configApiKey = Nothing
    }

defaultOllamaConfig :: OllamaConfig
defaultOllamaConfig = defaultConfig

-- | Ollama provider data type
data Ollama = Ollama
  { ollamaModelName :: Text
  , client :: OllamaClient
  }

instance Show Ollama where
  show (Ollama m _) = "Ollama provider (" ++ show m ++ ")"

-- | Create a new Ollama provider with default client
newOllama :: MonadIO m => Text -> m Ollama
newOllama model = do
  c <- liftIO defaultClient
  pure $ Ollama model c

-- | Create a new Ollama provider with custom OllamaConfig
newOllamaWithConfig :: MonadIO m => OllamaConfig -> m Ollama
newOllamaWithConfig cfg = do
  let baseClientCfg = OConfig.defaultConfig
      clientCfg =
        baseClientCfg
          { OConfig.configBaseUrl = fromMaybe (OConfig.configBaseUrl baseClientCfg) (configBaseUrl cfg)
          , OConfig.configTimeout = fromMaybe (OConfig.configTimeout baseClientCfg) (configTimeout cfg)
          , OConfig.configApiKey = configApiKey cfg
          }
  c <- liftIO $ newClient clientCfg
  pure $ Ollama (configModelName cfg) c

-- | Create a new Ollama provider with a custom timeout (in seconds)
newOllamaWithTimeout :: MonadIO m => Text -> Int -> m Ollama
newOllamaWithTimeout model timeoutSecs = do
  let clientCfg =
        OConfig.defaultConfig
          { OConfig.configTimeout = timeoutSecs
          }
  c <- liftIO $ newClient clientCfg
  pure $ Ollama model c

-- | Create a new Ollama provider with a custom base URL endpoint
newOllamaWithEndpoint :: MonadIO m => Text -> Text -> m Ollama
newOllamaWithEndpoint model endpoint = do
  let clientCfg =
        OConfig.defaultConfig
          { OConfig.configBaseUrl = endpoint
          }
  c <- liftIO $ newClient clientCfg
  pure $ Ollama model c

-- | Create an Ollama provider using an existing OllamaClient handle
newOllamaWithClient :: Text -> OllamaClient -> Ollama
newOllamaWithClient = Ollama

-- | Default Ollama instance
defaultOllama :: MonadIO m => m Ollama
defaultOllama = newOllama "gemma3:latest"

-- | Helper to convert core Role to Ollama Role
toOllamaRole :: Role -> O.Role
toOllamaRole System = O.System
toOllamaRole User = O.User
toOllamaRole Assistant = O.Assistant
toOllamaRole Tool = O.Tool
toOllamaRole Developer = O.System
toOllamaRole Function = O.Tool

-- | Helper to convert Ollama Role to core Role
fromOllamaRole :: O.Role -> Role
fromOllamaRole O.System = System
fromOllamaRole O.User = User
fromOllamaRole O.Assistant = Assistant
fromOllamaRole O.Tool = Tool

-- | Convert core Message to Ollama Message
toOllamaMessage :: Message -> O.Message
toOllamaMessage msg =
  let r = toOllamaRole (messageRole msg)
      txt = extractMessageText msg
      imgs = case [ b64
                  | ImageBlock ImageContent {imageSource = ImageBase64 _ b64} <- NonEmpty.toList (messageContents msg)
                  ] of
        [] -> Nothing
        xs -> Just (map Base64Image xs)
      tools = case messageToolCalls msg of
        Nothing -> Nothing
        Just tcs ->
          Just
            [ OTool.ToolCall
                { OTool.tcFunction =
                    OTool.ToolCallFunction
                      { OTool.tcfName = toolCallName tc
                      , OTool.tcfArguments = case fromJSON (toolCallArguments tc) of
                          Success m -> m
                          _ -> mempty
                      }
                }
            | tc <- tcs
            ]
   in O.Message r txt imgs tools Nothing Nothing

-- | Convert Ollama Message to core Message
fromOllamaMessage :: O.Message -> Message
fromOllamaMessage (O.Message r txt _imgs tools _name _think) =
  let cRole = fromOllamaRole r
      cMsg = textMessage cRole txt
      cTools = case tools of
        Nothing -> Nothing
        Just tcs ->
          Just
            [ ToolCall
                { toolCallId = ""
                , toolCallType = "function"
                , toolCallName = OTool.tcfName (OTool.tcFunction tc)
                , toolCallArguments = toJSON (OTool.tcfArguments (OTool.tcFunction tc))
                }
            | tc <- tcs
            ]
   in cMsg {messageToolCalls = cTools}

instance ChatModel Ollama where
  type ModelConfig Ollama = OllamaChat.ChatRequest

  invoke model inputMsgs mbReq = do
    let oMsgs = case inputMsgs of
          [] -> O.userMessage "" NonEmpty.:| []
          (m : ms) -> NonEmpty.map toOllamaMessage (m NonEmpty.:| ms)
        baseReq = OllamaChat.chatRequest (ModelName (ollamaModelName model)) oMsgs
        req = case mbReq of
          Nothing -> baseReq
          Just r -> r {OllamaChat.chatModel = ModelName (ollamaModelName model), OllamaChat.chatMessages = oMsgs}

    eRes <- liftIO $ OllamaChat.chat (client model) req
    case eRes of
      Left err -> throwError $ llmError (T.pack $ show err) Nothing Nothing
      Right resp -> case OllamaChat.crMessage resp of
        Nothing -> throwError $ llmError "No message in response" Nothing Nothing
        Just oMsg -> pure $ fromOllamaMessage oMsg

  stream model inputMsgs mbReq = do
    let runId_ = "ollama-run"
        oMsgs = case inputMsgs of
          [] -> O.userMessage "" NonEmpty.:| []
          (m : ms) -> NonEmpty.map toOllamaMessage (m NonEmpty.:| ms)
        baseReq = OllamaChat.chatRequest (ModelName (ollamaModelName model)) oMsgs
        req = case mbReq of
          Nothing -> baseReq
          Just r -> r {OllamaChat.chatModel = ModelName (ollamaModelName model), OllamaChat.chatMessages = oMsgs}

    yield $ LLMStart runId_ (ollamaModelName model) inputMsgs
    eRes <- liftIO $ OllamaChat.chat (client model) req
    case eRes of
      Left err -> yield $ LLMChunk runId_ (T.pack $ show err) Nothing
      Right resp -> case OllamaChat.crMessage resp of
        Nothing -> yield $ LLMEnd runId_ (assistantMessage "") Nothing
        Just oMsg -> do
          let finalMsg = fromOllamaMessage oMsg
          yield $ LLMChunk runId_ (extractMessageText finalMsg) Nothing
          yield $ LLMEnd runId_ finalMsg Nothing

-- | Attach generic JSON format constraint to Ollama ChatRequest
withJsonFormat :: OllamaChat.ChatRequest -> OllamaChat.ChatRequest
withJsonFormat req = req {OllamaChat.chatFormat = Just OFormat.JsonFormat}

-- | Attach specific Schema format constraint to Ollama ChatRequest
withSchemaFormat :: OSB.Schema -> OllamaChat.ChatRequest -> OllamaChat.ChatRequest
withSchemaFormat schema req = req {OllamaChat.chatFormat = Just (OFormat.SchemaFormat schema)}

-- | Attach automatic ToSchema derived format constraint to Ollama ChatRequest
withStructuredOutput ::
  forall a.
  (OSD.ToSchema a) =>
  OllamaChat.ChatRequest ->
  OllamaChat.ChatRequest
withStructuredOutput req =
  req {OllamaChat.chatFormat = Just (OFormat.SchemaFormat (OSD.toSchema @a))}

-- | Directly invoke Ollama with structured output constrained by automatic ToSchema derivation
structuredOllamaInvoke ::
  forall a m.
  (OSD.ToSchema a, FromJSON a, MonadIO m, MonadError LangchainError m) =>
  Ollama ->
  [Message] ->
  m a
structuredOllamaInvoke model inputMsgs = do
  let oMsgs = case inputMsgs of
        [] -> O.userMessage "" NonEmpty.:| []
        (m : ms) -> NonEmpty.map toOllamaMessage (m NonEmpty.:| ms)
      baseReq = OllamaChat.chatRequest (ModelName (ollamaModelName model)) oMsgs
      req = withStructuredOutput @a baseReq
  respMsg <- invoke model inputMsgs (Just req)
  let rawText = extractMessageText respMsg
      cleanJson = extractJsonFromMarkdown rawText
      bs = LBSC.fromStrict (TE.encodeUtf8 cleanJson)
  case decode bs of
    Just val -> pure val
    Nothing ->
      throwError $
        parsingError
          ("Failed to parse Ollama structured response into typed value: " <> rawText)
          (Just "structuredOllamaInvoke")
          Nothing

-- | Directly invoke Ollama with structured output constrained by a Langchain StructuredOutput instance
structuredOllamaInvokeWithSchema ::
  forall a m.
  (StructuredOutput a, MonadIO m, MonadError LangchainError m) =>
  Ollama ->
  [Message] ->
  m a
structuredOllamaInvokeWithSchema model inputMsgs = do
  let oMsgs = case inputMsgs of
        [] -> O.userMessage "" NonEmpty.:| []
        (m : ms) -> NonEmpty.map toOllamaMessage (m NonEmpty.:| ms)
      baseReq = OllamaChat.chatRequest (ModelName (ollamaModelName model)) oMsgs
      valSchema = outputSchema (Proxy :: Proxy a)
      req = case toOllamaSchema valSchema of
        Just s -> withSchemaFormat s baseReq
        Nothing -> withJsonFormat baseReq
  respMsg <- invoke model inputMsgs (Just req)
  let rawText = extractMessageText respMsg
      cleanJson = extractJsonFromMarkdown rawText
      bs = LBSC.fromStrict (TE.encodeUtf8 cleanJson)
  case decode bs of
    Just val -> pure val
    Nothing ->
      throwError $
        parsingError
          ("Failed to parse Ollama structured response into typed value: " <> rawText)
          (Just "structuredOllamaInvokeWithSchema")
          Nothing

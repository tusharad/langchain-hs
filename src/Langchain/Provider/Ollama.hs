{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  , newOllamaWithOptions
  , newOllamaWithConfig
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
  , HasModelOptions (..)
  , withOptions
  , withTemperature
  , withTopP
  , withNumCtx
  , withSeed
  , withStop
  , withKeepAlive
  , withChatKeepAlive
  , chatRequestFor
  , invokeWithOptions
  , streamWithOptions
  , structuredOllamaInvoke
  , structuredOllamaInvokeWithOptions
  , structuredOllamaInvokeWithSchema
  , structuredOllamaInvokeWithSchemaOptions

    -- * Re-exports from ollama-haskell format, schema & options
  , module Ollama.API.Chat
  , module Ollama.Types.Common
  , module Ollama.Types.Options
  , OFormat.Format (..)
  , OSB.Schema (..)
  , OSB.Property (..)
  , OSB.JsonType (..)
  , OSD.ToSchema (..)
  , OSD.ToJsonType (..)
  ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, Result (..), decode, fromJSON, toJSON)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Conduit (await, transPipe, yield, (.|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Langchain.Core.Error (LangchainError, llmError, parsingError)
import Langchain.Core.Model
import Langchain.Core.Stream (ChatStream, StreamEvent (..), TokenUsage (..))
import Langchain.OutputParser.Structured
  ( StructuredOutput (..)
  , extractJsonFromMarkdown
  , toOllamaSchema
  )

import Ollama.API.Chat
import qualified Ollama.API.Chat as OllamaChat
import Ollama.Client (OllamaClient, defaultClient, newClient)
import qualified Ollama.Client.Config as OConfig
import Ollama.Types.Common (Base64Image (..), ModelName (..))
import qualified Ollama.Types.Format as OFormat
import qualified Ollama.Types.Format.SchemaBuilder as OSB
import qualified Ollama.Types.Format.SchemaDerive as OSD
import qualified Ollama.Types.Message as O
import Ollama.Types.Options (ModelOptions (..), defaultOptions)
import qualified Ollama.Types.Tool as OTool

-- | Configuration options for Ollama provider
data OllamaConfig = OllamaConfig
  { configModelName :: Text
  , configBaseUrl :: Maybe Text
  , configTimeout :: Maybe Int
  , configKeepAlive :: Maybe Text
  , configApiKey :: Maybe Text
  , configOptions :: Maybe ModelOptions
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
    , configOptions = Nothing
    }

defaultOllamaConfig :: OllamaConfig
defaultOllamaConfig = defaultConfig

-- | Ollama provider data type
data Ollama = Ollama
  { ollamaModelName :: Text
  , client :: OllamaClient
  , ollamaOptions :: Maybe ModelOptions
  , ollamaKeepAlive :: Maybe Text
  }

instance Show Ollama where
  show (Ollama m _ mbOpts mbKa) =
    "Ollama provider ("
      ++ show m
      ++ maybe "" (\o -> ", options: " ++ show o) mbOpts
      ++ maybe "" (\k -> ", keepAlive: " ++ show k) mbKa
      ++ ")"

-- | Create a new Ollama provider with default client
newOllama :: MonadIO m => Text -> m Ollama
newOllama model = do
  c <- liftIO defaultClient
  pure $ Ollama model c Nothing Nothing

-- | Create a new Ollama provider with specific 'ModelOptions'
newOllamaWithOptions :: MonadIO m => Text -> ModelOptions -> m Ollama
newOllamaWithOptions model opts = do
  c <- liftIO defaultClient
  pure $ Ollama model c (Just opts) Nothing

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
  pure $ Ollama (configModelName cfg) c (configOptions cfg) (configKeepAlive cfg)

-- | Create an Ollama provider using an existing OllamaClient handle
newOllamaWithClient :: Text -> OllamaClient -> Ollama
newOllamaWithClient model c = Ollama model c Nothing Nothing

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

{- | Construct a 'ChatRequest' for an 'Ollama' instance with the given messages,
pre-populated with any model-level options and keepAlive settings.
-}
chatRequestFor :: Ollama -> [Message] -> OllamaChat.ChatRequest
chatRequestFor model inputMsgs =
  let oMsgs = case inputMsgs of
        [] -> O.userMessage "" NonEmpty.:| []
        (m : ms) -> NonEmpty.map toOllamaMessage (m NonEmpty.:| ms)
   in (OllamaChat.chatRequest (ModelName (ollamaModelName model)) oMsgs)
        { OllamaChat.chatOptions = ollamaOptions model
        , OllamaChat.chatKeepAlive = ollamaKeepAlive model
        }

instance ChatModel Ollama where
  type ModelConfig Ollama = OllamaChat.ChatRequest

  invoke model inputMsgs mbReq = do
    let baseReq = chatRequestFor model inputMsgs
        req = case mbReq of
          Nothing -> baseReq
          Just r ->
            r
              { OllamaChat.chatModel = ModelName (ollamaModelName model)
              , OllamaChat.chatMessages = OllamaChat.chatMessages baseReq
              , OllamaChat.chatOptions = case OllamaChat.chatOptions r of
                  Nothing -> ollamaOptions model
                  opts -> opts
              , OllamaChat.chatKeepAlive = case OllamaChat.chatKeepAlive r of
                  Nothing -> ollamaKeepAlive model
                  ka -> ka
              }

    eRes <- liftIO $ OllamaChat.chat (client model) req
    case eRes of
      Left err -> throwError $ llmError (T.pack $ show err) Nothing Nothing
      Right resp -> case OllamaChat.crMessage resp of
        Nothing -> throwError $ llmError "No message in response" Nothing Nothing
        Just oMsg -> pure $ fromOllamaMessage oMsg

  stream model inputMsgs mbReq = do
    let runId_ = "ollama-run"
        baseReq = chatRequestFor model inputMsgs
        req = case mbReq of
          Nothing -> baseReq
          Just r ->
            r
              { OllamaChat.chatModel = ModelName (ollamaModelName model)
              , OllamaChat.chatMessages = OllamaChat.chatMessages baseReq
              , OllamaChat.chatOptions = case OllamaChat.chatOptions r of
                  Nothing -> ollamaOptions model
                  opts -> opts
              , OllamaChat.chatKeepAlive = case OllamaChat.chatKeepAlive r of
                  Nothing -> ollamaKeepAlive model
                  ka -> ka
              }

    yield $ LLMStart runId_ (ollamaModelName model) inputMsgs
    transPipe liftIO (OllamaChat.chatStream (client model) req) .| processChunks runId_
    where
      processChunks rId = loop [] Nothing Nothing
        where
          loop accChunks mbLastUsage mbLastTools =
            await >>= \case
              Nothing -> do
                let fullText = T.concat (reverse accChunks)
                    finalMsg =
                      (assistantMessage fullText)
                        { messageToolCalls = mbLastTools
                        }
                yield $ LLMEnd rId finalMsg mbLastUsage
              Just resp -> do
                let mbMsg = OllamaChat.crMessage resp
                    chunkTxt = maybe "" O.messageContent mbMsg
                    mbTools = mbMsg >>= O.messageToolCalls
                    toolCalls = case mbTools of
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
                    toolDelta = case toolCalls of
                      Just (tc : _) -> Just tc
                      _ -> Nothing
                    newAccChunks = if T.null chunkTxt then accChunks else chunkTxt : accChunks
                    newTools = case toolCalls of
                      Just tcs -> Just $ maybe tcs (++ tcs) mbLastTools
                      Nothing -> mbLastTools
                    newUsage = case (OllamaChat.crPromptEvalCount resp, OllamaChat.crEvalCount resp) of
                      (Just p, Just c) -> Just $ TokenUsage p c (p + c)
                      _ -> mbLastUsage
                when (not (T.null chunkTxt) || isJust toolDelta) $
                  yield $
                    LLMChunk rId chunkTxt toolDelta
                loop newAccChunks newUsage newTools

-- | Typeclass for entities that support Ollama 'ModelOptions' configuration
class HasModelOptions a where
  -- | Modify existing options (or initialized from 'defaultOptions' if none exist)
  modifyModelOptions :: (ModelOptions -> ModelOptions) -> a -> a

  -- | Explicitly set model options
  setModelOptions :: ModelOptions -> a -> a
  setModelOptions opts = modifyModelOptions (const opts)

instance HasModelOptions Ollama where
  modifyModelOptions f o =
    let cur = fromMaybe defaultOptions (ollamaOptions o)
     in o {ollamaOptions = Just (f cur)}
  setModelOptions opts o = o {ollamaOptions = Just opts}

instance HasModelOptions OllamaChat.ChatRequest where
  modifyModelOptions f req =
    let cur = fromMaybe defaultOptions (OllamaChat.chatOptions req)
     in req {OllamaChat.chatOptions = Just (f cur)}
  setModelOptions opts req = req {OllamaChat.chatOptions = Just opts}

instance HasModelOptions ModelOptions where
  modifyModelOptions f = f
  setModelOptions opts _ = opts

-- | Set model options on an Ollama model, ChatRequest, or ModelOptions
withOptions :: HasModelOptions a => ModelOptions -> a -> a
withOptions = setModelOptions

-- | Set temperature (sampling temperature between 0.0 and 2.0)
withTemperature :: HasModelOptions a => Double -> a -> a
withTemperature t = modifyModelOptions (\opts -> opts {optTemperature = Just t})

-- | Set top-p (nucleus sampling probability)
withTopP :: HasModelOptions a => Double -> a -> a
withTopP p = modifyModelOptions (\opts -> opts {optTopP = Just p})

-- | Set number of tokens in the context window (context size)
withNumCtx :: HasModelOptions a => Int -> a -> a
withNumCtx n = modifyModelOptions (\opts -> opts {optNumCtx = Just n})

-- | Set RNG seed for deterministic generation
withSeed :: HasModelOptions a => Int -> a -> a
withSeed s = modifyModelOptions (\opts -> opts {optSeed = Just s})

-- | Set stop sequences
withStop :: HasModelOptions a => [Text] -> a -> a
withStop stops = modifyModelOptions (\opts -> opts {optStop = Just stops})

-- | Set keep-alive duration on an Ollama model (e.g. "5m", "1h", "0")
withKeepAlive :: Text -> Ollama -> Ollama
withKeepAlive ka o = o {ollamaKeepAlive = Just ka}

-- | Set keep-alive duration on a ChatRequest
withChatKeepAlive :: Text -> OllamaChat.ChatRequest -> OllamaChat.ChatRequest
withChatKeepAlive ka req = req {OllamaChat.chatKeepAlive = Just ka}

-- | Invoke Ollama with specific ModelOptions
invokeWithOptions ::
  (MonadIO m, MonadError LangchainError m) =>
  Ollama ->
  ModelOptions ->
  [Message] ->
  m Message
invokeWithOptions model opts msgs = invoke (withOptions opts model) msgs Nothing

-- | Stream Ollama with specific ModelOptions
streamWithOptions ::
  Ollama ->
  ModelOptions ->
  [Message] ->
  ChatStream
streamWithOptions model opts msgs = stream (withOptions opts model) msgs Nothing

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
  let baseReq = chatRequestFor model inputMsgs
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

-- | Directly invoke Ollama with structured output constrained by automatic ToSchema derivation and ModelOptions
structuredOllamaInvokeWithOptions ::
  forall a m.
  (OSD.ToSchema a, FromJSON a, MonadIO m, MonadError LangchainError m) =>
  Ollama ->
  ModelOptions ->
  [Message] ->
  m a
structuredOllamaInvokeWithOptions model opts =
  structuredOllamaInvoke (withOptions opts model)

-- | Directly invoke Ollama with structured output constrained by a Langchain StructuredOutput instance
structuredOllamaInvokeWithSchema ::
  forall a m.
  (StructuredOutput a, MonadIO m, MonadError LangchainError m) =>
  Ollama ->
  [Message] ->
  m a
structuredOllamaInvokeWithSchema model inputMsgs = do
  let baseReq = chatRequestFor model inputMsgs
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

-- | Directly invoke Ollama with structured output constrained by a Langchain StructuredOutput instance and ModelOptions
structuredOllamaInvokeWithSchemaOptions ::
  forall a m.
  (StructuredOutput a, MonadIO m, MonadError LangchainError m) =>
  Ollama ->
  ModelOptions ->
  [Message] ->
  m a
structuredOllamaInvokeWithSchemaOptions model opts =
  structuredOllamaInvokeWithSchema (withOptions opts model)

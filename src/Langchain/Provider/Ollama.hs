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

== Request & Precedence Semantics
Because Ollama's 'ModelConfig' is 'OllamaChat.ChatRequest', which contains both
'chatModel' and 'chatMessages', callers may supply messages and model names either
via the 'ChatModel' interface ('invoke' / 'stream' arguments) or within the 'ChatRequest'.

The provider resolves these with well-defined precedence rules:

1. __Messages Precedence__:
   * When @inputMsgs@ is non-empty (@not (null inputMsgs)@), it takes precedence over
     'ChatRequest.chatMessages'. This enables reusing request templates across multiple calls
     and ensures 'batch' processes each item's messages properly.
   * When @inputMsgs@ is empty (@null inputMsgs@), the provider falls back to
     'ChatRequest.chatMessages' if a 'ChatRequest' is provided.
   * If both are empty, defaults to an empty user message.

2. __Model Name Precedence__:
   * When a 'ChatRequest' is provided with a non-empty 'chatModel', it overrides
     the provider's default 'ollamaModelName'.
   * Otherwise, the provider's 'ollamaModelName' is used as the default.
-}
module Langchain.Provider.Ollama
  ( Ollama (..)
  , newOllama
  , newOllamaWithClient
  , toOllamaRole
  , fromOllamaRole
  , toOllamaMessage
  , fromOllamaMessage
  , withJsonFormat
  , withSchemaFormat
  , withStructuredOutput
  , withOptions
  , chatRequestFor
  , resolveChatRequest
  , withTools
  , toOllamaTool
  , toOllamaTools

    -- * Re-exports from ollama-haskell format, schema, options & client config
  , module Ollama.API.Chat
  , module Ollama.Client.Config
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
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Result (..), decode, fromJSON, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Conduit (await, transPipe, yield, (.|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Langchain.Core.Error (llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..))
import Langchain.Core.Tool (Tool, toolToValue)

import Ollama.API.Chat
import qualified Ollama.API.Chat as OllamaChat
import Ollama.Client (OllamaClient, newClient)
import Ollama.Client.Config
import Ollama.Types.Common (Base64Image (..), ModelName (..))
import qualified Ollama.Types.Format as OFormat
import qualified Ollama.Types.Format.SchemaBuilder as OSB
import qualified Ollama.Types.Format.SchemaDerive as OSD
import qualified Ollama.Types.Message as O
import Ollama.Types.Options (ModelOptions (..), defaultOptions)
import qualified Ollama.Types.Tool as OTool

-- | Ollama provider data type wrapping OllamaClient and model name
data Ollama = Ollama
  { client :: OllamaClient
  , ollamaModelName :: Text
  }

instance Show Ollama where
  show (Ollama _ m) = "Ollama provider (" ++ show m ++ ")"

-- | Create a new Ollama provider with model name and client config
newOllama :: MonadIO m => Text -> OllamaClientConfig -> m Ollama
newOllama model cfg = do
  c <- liftIO $ newClient cfg
  pure $ Ollama c model

-- | Create an Ollama provider using an existing OllamaClient handle
newOllamaWithClient :: Text -> OllamaClient -> Ollama
newOllamaWithClient model c = Ollama c model

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
                      , OTool.tcfArguments = parseArgs (toolCallArguments tc)
                      }
                }
            | tc <- tcs
            ]
      parseArgs v = case v of
        Aeson.Object _ -> case fromJSON v of
          Success m -> m
          _ -> mempty
        Aeson.String s -> fromMaybe mempty $ decode (LBSC.fromStrict (TE.encodeUtf8 s))
        _ -> case fromJSON v of
          Success m -> m
          _ -> mempty
   in O.Message r txt imgs tools (messageName msg) Nothing

-- | Convert Ollama Message to core Message
fromOllamaMessage :: O.Message -> Message
fromOllamaMessage (O.Message r txt _imgs tools name _think) =
  let cRole = fromOllamaRole r
      cMsg = (textMessage cRole txt) {messageName = name}
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

{- | Construct a 'ChatRequest' for an 'Ollama' instance with the given messages.

Sets 'chatModel' to the provider's 'ollamaModelName'. When passed to 'invoke'
or 'stream', any non-empty message argument passed directly to 'invoke' or
'stream' will take priority over the messages in this 'ChatRequest'.
-}
chatRequestFor :: Ollama -> [Message] -> OllamaChat.ChatRequest
chatRequestFor model inputMsgs =
  let oMsgs = case inputMsgs of
        [] -> O.userMessage "" NonEmpty.:| []
        (m : ms) -> NonEmpty.map toOllamaMessage (m NonEmpty.:| ms)
   in OllamaChat.chatRequest (ModelName (ollamaModelName model)) oMsgs

{- | Resolve the effective 'ChatRequest', effective model name, and effective messages
given the provider instance, explicit message arguments, and optional 'ChatRequest'.

= Precedence Rules

* __Messages Precedence__:
  1. If @inputMsgs@ is non-empty (@not (null inputMsgs)@), it takes priority and
     is used as the request conversation. This enables reusing a configured
     'ChatRequest' (tools, formats, options) across invocations and guarantees
     that 'batch' processes each item's messages properly.
  2. If @inputMsgs@ is empty (@null inputMsgs@) and a 'ChatRequest' is provided,
     its 'chatMessages' field is preserved and used.
  3. If both are empty (or @inputMsgs@ is empty and 'mbReq' is 'Nothing'),
     it defaults to a single empty user message.

* __Model Name Precedence__:
  1. If a 'ChatRequest' is provided and its 'chatModel' is non-empty,
     it overrides the provider's default 'ollamaModelName'.
  2. Otherwise, the provider's 'ollamaModelName' is used as the default.
-}
resolveChatRequest ::
  Ollama ->
  [Message] ->
  Maybe OllamaChat.ChatRequest ->
  (OllamaChat.ChatRequest, Text, [Message])
resolveChatRequest model inputMsgs mbReq =
  let providerModel = ollamaModelName model
      resolvedModelText = case mbReq of
        Just r ->
          let m = unModelName (OllamaChat.chatModel r)
           in if T.null m then providerModel else m
        Nothing -> providerModel
      resolvedModelName = ModelName resolvedModelText

      (resolvedOMsgs, resolvedCoreMsgs) = case inputMsgs of
        (m : ms) ->
          let oList = NonEmpty.map toOllamaMessage (m NonEmpty.:| ms)
           in (oList, inputMsgs)
        [] -> case mbReq of
          Just r ->
            let oList = OllamaChat.chatMessages r
                coreList = map fromOllamaMessage (NonEmpty.toList oList)
             in (oList, coreList)
          Nothing ->
            (O.userMessage "" NonEmpty.:| [], [])

      resolvedReq = case mbReq of
        Nothing ->
          OllamaChat.chatRequest resolvedModelName resolvedOMsgs
        Just r ->
          r
            { OllamaChat.chatModel = resolvedModelName
            , OllamaChat.chatMessages = resolvedOMsgs
            }
   in (resolvedReq, resolvedModelText, resolvedCoreMsgs)

instance ChatModel Ollama where
  type ModelConfig Ollama = OllamaChat.ChatRequest

  invoke model inputMsgs mbReq = do
    let (req, _modelName, _msgs) = resolveChatRequest model inputMsgs mbReq
    eRes <- liftIO $ OllamaChat.chat (client model) req
    case eRes of
      Left err -> throwError $ llmError (T.pack $ show err) Nothing Nothing
      Right resp -> case OllamaChat.crMessage resp of
        Nothing -> throwError $ llmError "No message in response" Nothing Nothing
        Just oMsg -> pure $ fromOllamaMessage oMsg

  stream model inputMsgs mbReq = do
    let runId_ = "ollama-run"
        (req, resolvedModel, resolvedMsgs) = resolveChatRequest model inputMsgs mbReq

    yield $ LLMStart runId_ resolvedModel resolvedMsgs
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

-- | Set model options on an Ollama ChatRequest
withOptions :: ModelOptions -> OllamaChat.ChatRequest -> OllamaChat.ChatRequest
withOptions opts req = req {OllamaChat.chatOptions = Just opts}

-- | Attach Langchain tools to an Ollama ChatRequest
withTools :: [Tool m] -> OllamaChat.ChatRequest -> OllamaChat.ChatRequest
withTools ts req = req {OllamaChat.chatTools = Just (toOllamaTools ts)}

-- | Convert a Langchain 'Tool' definition to an Ollama 'OTool.Tool'
toOllamaTool :: Tool m -> Maybe OTool.Tool
toOllamaTool t = case fromJSON (toolToValue t) of
  Success ot -> Just ot
  Aeson.Error _ -> Nothing

-- | Convert a list of Langchain 'Tool' definitions to Ollama 'OTool.Tool's
toOllamaTools :: [Tool m] -> [OTool.Tool]
toOllamaTools = mapMaybe toOllamaTool

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

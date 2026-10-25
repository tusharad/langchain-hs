{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

Ollama provider using 'ollama-haskell'. Supports native structured outputs
via JSON Schema grammar sampling, token streaming, tool calling, and model options.
-}
module Langchain.Provider.Ollama
  ( -- * Provider
    Ollama (..)
  , newOllama
  , newOllamaWithClient

    -- * Configuration
  , OllamaConfig (..)
  , OllamaOptions
  , defaultOllamaConfig
  , defaultClientConfig
  , withOptions
  , withTools
  , withJsonFormat
  , withSchemaFormat
  , withStructuredOutput
  , withKeepAlive
  , withThink

    -- * Proxy
  , Proxy (..)

    -- * Tool Binding
  , OllamaWithTools (..)
  , bindTools
  , toOllamaTool
  , toOllamaTools

    -- * Conversions
  , toOllamaRole
  , fromOllamaRole
  , toOllamaMessage
  , fromOllamaMessage

    -- * Re-exports from ollama-haskell
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

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Result (..), decode, fromJSON, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Conduit (await, transPipe, yield, (.|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import Langchain.Cache.Core (CacheableChatModel (..))
import Langchain.Error (llmError)
import Langchain.Model
import Langchain.Stream (StreamEvent (..), TokenUsage (..))
import Langchain.Tool (Tool, toolToValue)
import Langchain.Tool.Binding (ToolBinder (..))

import qualified Ollama.API.Chat as OllamaChat
import Ollama.Client (OllamaClient (..), newClient)
import Ollama.Client.Config (OllamaClientConfig (..), defaultConfig)
import Ollama.Types.Common (Base64Image (..), ModelName (..), Think)
import qualified Ollama.Types.Format as OFormat
import qualified Ollama.Types.Format.SchemaBuilder as OSB
import qualified Ollama.Types.Format.SchemaDerive as OSD
import qualified Ollama.Types.Message as O
import Ollama.Types.Options (ModelOptions (..), defaultOptions)
import qualified Ollama.Types.Tool as OTool

-- | Ollama provider wrapping an 'OllamaClient' and model name.
data Ollama = Ollama
  { client :: !OllamaClient
  -- ^ The underlying HTTP client connection to Ollama
  , ollamaModelName :: !Text
  -- ^ The model name (e.g. @"llama3"@, @"gemma3"@)
  }

instance Show Ollama where
  show (Ollama _ m) = "Ollama provider (" ++ show m ++ ")"

-- | Create a new Ollama provider with a model name and client configuration.
newOllama :: MonadIO m => Text -> OllamaClientConfig -> m Ollama
newOllama model cfg = do
  c <- liftIO $ newClient cfg
  pure $ Ollama c model

-- | Create an Ollama provider using an existing 'OllamaClient' handle.
newOllamaWithClient :: Text -> OllamaClient -> Ollama
newOllamaWithClient model c = Ollama c model

{- | Configuration options for an Ollama invocation.

Contains request-level settings (tools, format constraints, model options,
keep-alive duration, thinking parameter) without redundant model names or message history.
-}
data OllamaConfig = OllamaConfig
  { ollamaTools :: !(Maybe [OTool.Tool])
  -- ^ Tool definitions available for the model to invoke
  , ollamaFormat :: !(Maybe OFormat.Format)
  -- ^ Output format constraint (e.g. JSON format or JSON schema)
  , ollamaOptions :: !(Maybe ModelOptions)
  -- ^ Runtime model parameters (temperature, num_ctx, top_k, top_p, etc.)
  , ollamaKeepAlive :: !(Maybe Text)
  -- ^ Time to keep model in memory (e.g. @"5m"@)
  , ollamaThink :: !(Maybe Think)
  -- ^ Thinking parameter for reasoning models
  }
  deriving (Eq, Show, Generic)

-- | Backward-compatible alias for 'OllamaConfig'.
type OllamaOptions = OllamaConfig

-- | Default empty 'OllamaConfig' with all options unset.
defaultOllamaConfig :: OllamaConfig
defaultOllamaConfig = OllamaConfig Nothing Nothing Nothing Nothing Nothing

-- | Default Ollama client configuration targeting local @http://localhost:11434@.
defaultClientConfig :: OllamaClientConfig
defaultClientConfig = defaultConfig

instance Semigroup OllamaConfig where
  a <> b =
    OllamaConfig
      { ollamaTools = case (ollamaTools a, ollamaTools b) of
          (Just ts1, Just ts2) -> Just (ts1 ++ ts2)
          (t1, t2) -> t2 <|> t1
      , ollamaFormat = ollamaFormat b <|> ollamaFormat a
      , ollamaOptions = ollamaOptions b <|> ollamaOptions a
      , ollamaKeepAlive = ollamaKeepAlive b <|> ollamaKeepAlive a
      , ollamaThink = ollamaThink b <|> ollamaThink a
      }

instance Monoid OllamaConfig where
  mempty = defaultOllamaConfig

-- | Attach runtime 'ModelOptions' (temperature, context size, etc.) to an 'OllamaConfig'.
withOptions :: ModelOptions -> OllamaConfig -> OllamaConfig
withOptions opts cfg = cfg {ollamaOptions = Just opts}

-- | Attach Langchain tools to an 'OllamaConfig'.
withTools :: [Tool m] -> OllamaConfig -> OllamaConfig
withTools ts cfg =
  let newTs = toOllamaTools ts
      combined = case ollamaTools cfg of
        Nothing -> newTs
        Just existing -> existing ++ newTs
   in cfg {ollamaTools = Just combined}

-- | Attach generic JSON format constraint to an 'OllamaConfig'.
withJsonFormat :: OllamaConfig -> OllamaConfig
withJsonFormat cfg = cfg {ollamaFormat = Just OFormat.JsonFormat}

-- | Attach specific Schema format constraint to an 'OllamaConfig'.
withSchemaFormat :: OSB.Schema -> OllamaConfig -> OllamaConfig
withSchemaFormat schema cfg = cfg {ollamaFormat = Just (OFormat.SchemaFormat schema)}

-- | Attach automatic 'OSD.ToSchema' derived format constraint to an 'OllamaConfig'.
withStructuredOutput ::
  forall a.
  (OSD.ToSchema a) =>
  Proxy a ->
  OllamaConfig ->
  OllamaConfig
withStructuredOutput _ cfg =
  cfg {ollamaFormat = Just (OFormat.SchemaFormat (OSD.toSchema @a))}

-- | Set keep-alive duration (e.g. @"5m"@) on an 'OllamaConfig'.
withKeepAlive :: Text -> OllamaConfig -> OllamaConfig
withKeepAlive duration cfg = cfg {ollamaKeepAlive = Just duration}

-- | Set thinking parameter on an 'OllamaConfig'.
withThink :: Think -> OllamaConfig -> OllamaConfig
withThink th cfg = cfg {ollamaThink = Just th}

-- | Convert core 'Role' to Ollama 'O.Role'.
toOllamaRole :: Role -> O.Role
toOllamaRole System = O.System
toOllamaRole User = O.User
toOllamaRole Assistant = O.Assistant
toOllamaRole Tool = O.Tool
toOllamaRole Developer = O.System
toOllamaRole Function = O.Tool

-- | Convert Ollama 'O.Role' to core 'Role'.
fromOllamaRole :: O.Role -> Role
fromOllamaRole O.System = System
fromOllamaRole O.User = User
fromOllamaRole O.Assistant = Assistant
fromOllamaRole O.Tool = Tool

-- | Convert core 'Message' to Ollama 'O.Message'.
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
            [ OTool.mkToolCall
                ( OTool.mkToolCallFunction
                    (toolCallName tc)
                    (parseArgs (toolCallArguments tc))
                )
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
   in O.Message r txt imgs tools (messageName msg) Nothing Nothing

-- | Convert Ollama 'O.Message' to core 'Message'.
fromOllamaMessage :: O.Message -> Message
fromOllamaMessage oMsg =
  let cRole = fromOllamaRole (O.messageRole oMsg)
      cMsg = (textMessage cRole (O.messageContent oMsg)) {messageName = O.messageToolName oMsg}
      cTools = case O.messageToolCalls oMsg of
        Nothing -> Nothing
        Just tcs ->
          Just
            [ ToolCall
                { toolCallId = fromMaybe "" (OTool.tcId tc)
                , toolCallType = "function"
                , toolCallName = OTool.tcfName (OTool.tcFunction tc)
                , toolCallArguments = toJSON (OTool.tcfArguments (OTool.tcFunction tc))
                }
            | tc <- tcs
            ]
   in cMsg {messageToolCalls = cTools}

-- | Convert a Langchain 'Tool' definition to an Ollama 'OTool.Tool'.
toOllamaTool :: Tool m -> Maybe OTool.Tool
toOllamaTool t = case fromJSON (toolToValue t) of
  Success ot -> Just ot
  Aeson.Error _ -> Nothing

-- | Convert a list of Langchain 'Tool' definitions to Ollama 'OTool.Tool's.
toOllamaTools :: [Tool m] -> [OTool.Tool]
toOllamaTools = mapMaybe toOllamaTool

-- | Build an 'OllamaChat.ChatRequest' from an 'Ollama' model, messages, and optional 'OllamaConfig'.
toChatRequest :: Ollama -> [Message] -> Maybe OllamaConfig -> OllamaChat.ChatRequest
toChatRequest (Ollama _ mName) inputMsgs mbConfig =
  let oMsgs = case inputMsgs of
        (m : ms) -> NonEmpty.map toOllamaMessage (m NonEmpty.:| ms)
        [] -> O.userMessage "" NonEmpty.:| []
      cfg = fromMaybe defaultOllamaConfig mbConfig
   in (OllamaChat.chatRequest (ModelName mName) oMsgs)
        { OllamaChat.chatTools = ollamaTools cfg
        , OllamaChat.chatFormat = ollamaFormat cfg
        , OllamaChat.chatOptions = ollamaOptions cfg
        , OllamaChat.chStream = Just False
        , OllamaChat.chatKeepAlive = ollamaKeepAlive cfg
        , OllamaChat.chatThink = ollamaThink cfg
        }

instance ChatModel Ollama where
  type ModelConfig Ollama = OllamaConfig

  invoke model inputMsgs mbConfig = do
    let req = toChatRequest model inputMsgs mbConfig
    eRes <- liftIO $ OllamaChat.chat (client model) req
    case eRes of
      Left err -> throwError $ llmError (T.pack $ show err) (Just "Ollama") Nothing
      Right resp -> case OllamaChat.crMessage resp of
        Nothing -> throwError $ llmError "No message in response" (Just "Ollama") Nothing
        Just oMsg -> pure $ fromOllamaMessage oMsg

  stream model inputMsgs mbConfig = do
    let runId_ = "ollama-run"
        req = toChatRequest model inputMsgs mbConfig
        resolvedModel = ollamaModelName model

    yield $ LLMStart runId_ resolvedModel inputMsgs
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
                              { toolCallId = fromMaybe "" (OTool.tcId tc)
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

instance CacheableChatModel Ollama where
  cacheModelIdentity (Ollama ollamaClient mName) mbCfg =
    Aeson.object
      [ "provider" .= ("ollama" :: Text)
      , "baseUrl" .= configBaseUrl (clientConfig ollamaClient)
      , "model" .= mName
      , "config"
          .= Aeson.object
            [ "tools" .= (ollamaTools =<< mbCfg)
            , "format" .= (ollamaFormat =<< mbCfg)
            , "options" .= (ollamaOptions =<< mbCfg)
            , "keep_alive" .= (ollamaKeepAlive =<< mbCfg)
            , "think" .= (ollamaThink =<< mbCfg)
            ]
      ]

-- | Attach tool definitions to an 'Ollama' invocation config.
instance ToolBinder Ollama m where
  bindToolsConfig tools mbConfig =
    case tools of
      [] -> mbConfig
      _ ->
        let cfg = fromMaybe defaultOllamaConfig mbConfig
         in Just (withTools tools cfg)

-- | Ollama model with pre-bound tools.
data OllamaWithTools m = OllamaWithTools
  { ollamaBaseModel :: !Ollama
  -- ^ The underlying Ollama model
  , ollamaBoundTools :: ![Tool m]
  -- ^ Pre-bound tools attached to this model
  }

instance Show (OllamaWithTools m) where
  show (OllamaWithTools m _) = "OllamaWithTools (" ++ show m ++ ")"

-- | Bind tools to an Ollama model so any invocation automatically includes tool definitions.
bindTools :: [Tool m] -> Ollama -> OllamaWithTools m
bindTools ts model = OllamaWithTools model ts

instance ChatModel (OllamaWithTools m) where
  type ModelConfig (OllamaWithTools m) = OllamaConfig
  invoke (OllamaWithTools model ts) msgs mbCfg =
    invoke model msgs (bindToolsConfig @Ollama ts mbCfg)
  stream (OllamaWithTools model ts) msgs mbCfg =
    stream model msgs (bindToolsConfig @Ollama ts mbCfg)

instance ToolBinder (OllamaWithTools m) n where
  bindToolsConfig tools mbCfg =
    case tools of
      [] -> mbCfg
      _ -> Just (withTools tools (fromMaybe defaultOllamaConfig mbCfg))

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Provider.Ollama
Description : Ollama provider implementing the effect-polymorphic ChatModel typeclass
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Ollama provider using 'ollama-haskell' 0.3.0.0.
-}
module Langchain.Provider.Ollama
  ( Ollama (..)
  , newOllama
  , defaultOllama
  , OllamaConfig (..)
  , defaultConfig
  , OllamaEmbeddings (..)
  , toOllamaRole
  , fromOllamaRole
  , toOllamaMessage
  , fromOllamaMessage
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.Conduit (yield)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Error (llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..))
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.Utils (showText)

import qualified Ollama.API.Chat as OllamaChat
import Ollama.API.Embed (EmbedRequest (..), EmbedResponse (..), embed)
import Ollama.Client (OllamaClient, defaultClient)
import Ollama.Types.Common (Base64Image (..), ModelName (..))
import qualified Ollama.Types.Message as O
import qualified Ollama.Types.Tool as OTool

-- | Configuration options for Ollama provider
data OllamaConfig = OllamaConfig
  { configModelName :: Text
  , configKeepAlive :: Maybe Text
  }
  deriving (Eq, Show)

defaultConfig :: OllamaConfig
defaultConfig = OllamaConfig "gemma3:latest" Nothing

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

-- | Ollama Embeddings Provider
data OllamaEmbeddings = OllamaEmbeddings
  { embedModelName :: Text
  , embedClient :: OllamaClient
  }

instance Embeddings OllamaEmbeddings where
  embedDocuments OllamaEmbeddings {..} docs = do
    let inputs = map (TL.toStrict . pageContent) docs
        req =
          EmbedRequest
            { embModel = ModelName embedModelName
            , embInput = Right inputs
            , embTruncate = Nothing
            , embOptions = Nothing
            , embKeepAlive = Nothing
            , embDimensions = Nothing
            }
    eRes <- liftIO $ embed embedClient req
    case eRes of
      Left err -> throwError $ llmError (showText err) (Just "OllamaEmbeddings") Nothing
      Right resp -> pure $ map (map realToFrac) (erEmbeddings resp)

  embedQuery OllamaEmbeddings {..} query = do
    let req =
          EmbedRequest
            { embModel = ModelName embedModelName
            , embInput = Left query
            , embTruncate = Nothing
            , embOptions = Nothing
            , embKeepAlive = Nothing
            , embDimensions = Nothing
            }
    eRes <- liftIO $ embed embedClient req
    case eRes of
      Left err -> throwError $ llmError (showText err) (Just "OllamaEmbeddings") Nothing
      Right resp -> case erEmbeddings resp of
        (vec : _) -> pure $ map realToFrac vec
        [] -> throwError $ llmError "Empty embeddings vector" (Just "OllamaEmbeddings") Nothing

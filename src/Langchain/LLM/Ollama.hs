{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Langchain.LLM.Ollama
Description : Ollama integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Ollama implementation of LangChain's LLM interface using ollama-haskell 0.3.0.0.
-}
module Langchain.LLM.Ollama
  ( Ollama (..)
  , defaultOllama

    -- * Re-export
  , module Langchain.LLM.Core
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Callback (Callback, Event (..))
import Langchain.Error (llmError)
import qualified Langchain.Error as Error
import Langchain.LLM.Core
import qualified Langchain.Runnable.Core as Run

import qualified Ollama.API.Chat as OllamaChat
import Ollama.Client (OllamaClient, defaultClient)
import Ollama.Types.Common (Base64Image (..), ModelName (..))
import qualified Ollama.Types.Message as O
import qualified Ollama.Types.Tool as OTool

data Ollama = Ollama
  { modelName :: Text
  , callbacks :: [Callback]
  }

instance Show Ollama where
  show (Ollama modelName _) = "Ollama " ++ show modelName

instance LLM Ollama where
  type LLMParams Ollama = OllamaChat.ChatRequest
  type LLMStreamTokenType Ollama = OllamaChat.ChatResponse

  generate (Ollama model cbs) prompt mbOllamaParams = do
    mapM_ (\cb -> cb LLMStart) cbs
    client <- defaultClient
    let userMsg = O.userMessage prompt
        baseReq = OllamaChat.chatRequest (ModelName model) (userMsg NonEmpty.:| [])
        chatOps = case mbOllamaParams of
          Nothing -> baseReq
          Just p -> p { OllamaChat.chatModel = ModelName model, OllamaChat.chatMessages = userMsg NonEmpty.:| [] }

    eRes <- OllamaChat.chat client chatOps
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError $ show err)) cbs
        return $ Left (llmError (T.pack $ show err) Nothing Nothing)
      Right chatResponse -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        case OllamaChat.crMessage chatResponse of
          Nothing -> pure $ Left (Error.fromString "Message not found in response")
          Just m -> pure $ Right $ O.messageContent m

  chat (Ollama model cbs) messages mbOllamaParams = do
    mapM_ (\cb -> cb LLMStart) cbs
    client <- defaultClient
    let oMsgs = NonEmpty.map to messages
        baseReq = OllamaChat.chatRequest (ModelName model) oMsgs
        chatOps = case mbOllamaParams of
          Nothing -> baseReq
          Just p -> p { OllamaChat.chatModel = ModelName model, OllamaChat.chatMessages = oMsgs }
    eRes <- OllamaChat.chat client chatOps
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError $ show err)) cbs
        return $ Left (llmError (T.pack $ show err) Nothing Nothing)
      Right res -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        case OllamaChat.crMessage res of
          Nothing ->
            return $
              Left $
                llmError
                  (T.pack $ "Message field not found: " <> show res)
                  Nothing
                  Nothing
          Just ollamaMsg -> return $ Right (from ollamaMsg)

  stream (Ollama model_ cbs) messages StreamHandler {onToken, onComplete} mbOllamaParams = do
    client <- defaultClient
    let oMsgs = NonEmpty.map to messages
        baseReq = OllamaChat.chatRequest (ModelName model_) oMsgs
        chatOps = case mbOllamaParams of
          Nothing -> baseReq
          Just p -> p { OllamaChat.chatModel = ModelName model_, OllamaChat.chatMessages = oMsgs }
    mapM_ (\cb -> cb LLMStart) cbs
    eRes <- OllamaChat.chat client chatOps
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError $ show err)) cbs
        return $ Left (llmError (T.pack $ show err) Nothing Nothing)
      Right resp -> do
        onToken resp
        onComplete
        mapM_ (\cb -> cb LLMEnd) cbs
        return $ Right ()

toOllamaRole :: Role -> O.Role
toOllamaRole User = O.User
toOllamaRole System = O.System
toOllamaRole Assistant = O.Assistant
toOllamaRole Tool = O.Tool
toOllamaRole _ = O.User

fromOllamaRole :: O.Role -> Role
fromOllamaRole O.User = User
fromOllamaRole O.System = System
fromOllamaRole O.Assistant = Assistant
fromOllamaRole O.Tool = Tool

instance MessageConvertible O.Message where
  to Message {..} =
    O.Message
      (toOllamaRole role)
      content
      (fmap (map Base64Image) (messageImages messageData))
      (fmap toOllamaToolCall <$> toolCalls messageData)
      Nothing
      (thinking messageData)
    where
      toOllamaToolCall :: ToolCall -> OTool.ToolCall
      toOllamaToolCall ToolCall {..} =
        OTool.ToolCall
          { OTool.tcFunction =
              OTool.ToolCallFunction
                { OTool.tcfName = toolFunctionName toolCallFunction
                , OTool.tcfArguments = toolFunctionArguments toolCallFunction
                }
          }

  from (O.Message role' content' imgs tools _toolName think) =
    Message
      { role = fromOllamaRole role'
      , content = content'
      , messageData =
          MessageData
            { messageImages = map unBase64Image <$> imgs
            , toolCalls = fmap toToolCall <$> tools
            , thinking = think
            , name = Nothing
            }
      }
    where
      toToolCall :: OTool.ToolCall -> ToolCall
      toToolCall OTool.ToolCall {..} =
        ToolCall
          { toolCallId = ""
          , toolCallType = "function"
          , toolCallFunction =
              ToolFunction
                { toolFunctionName = OTool.tcfName tcFunction
                , toolFunctionArguments = OTool.tcfArguments tcFunction
                }
          }

instance Run.Runnable Ollama where
  type RunnableInput Ollama = (ChatHistory, Maybe OllamaChat.ChatRequest)
  type RunnableOutput Ollama = Message

  invoke = uncurry . chat

defaultOllama :: Ollama
defaultOllama = Ollama "gemma3:latest" []

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

Ollama implementation of LangChain's LLM interface , supporting:

- Text generation
- Chat interactions
- Streaming responses
- Callback integration

Example usage:

@
-- Create Ollama configuration
ollamaLLM = Ollama "gemma3" [stdOutCallback]

-- Generate text
response <- generate ollamaLLM "Explain Haskell monads" Nothing
-- Right "Monads in Haskell..."

-- Chat interaction
let messages = UserMessage "What's the capital of France?" :| []
chatResponse <- chat ollamaLLM messages Nothing
-- Right "The capital of France is Paris."

-- Streaming
streamHandler = StreamHandler print (putStrLn "Done")
streamResult <- stream ollamaLLM messages streamHandler Nothing
@
-}
module Langchain.LLM.Ollama
  ( Ollama (..)
  , defaultOllama

    -- * Re-export
  , module Langchain.LLM.Core
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Ollama.Chat as OllamaChat
import qualified Data.Ollama.Common.Types as O
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Callback (Callback, Event (..))
import Langchain.Error (llmError)
import qualified Langchain.Error as Error
import Langchain.LLM.Core
import qualified Langchain.Runnable.Core as Run

{- | Ollama LLM configuration
Contains:

- Model name (e.g., "llama3:latest")
- Callbacks for event tracking

Example:

>>> Ollama "nomic-embed" [logCallback]
Ollama "nomic-embed"
-}
data Ollama = Ollama
  { modelName :: Text
  -- ^ The name of the Ollama model
  , callbacks :: [Callback]
  -- ^ Event handlers for LLM operations
  }

instance Show Ollama where
  show (Ollama modelName _) = "Ollama " ++ show modelName

{- | Ollama implementation of the LLM typeclass
Example instance usage:

@
-- Generate text with error handling
case generate ollamaLLM "Hello" Nothing of
  Left err -> putStrLn $ "Error: " ++ err
  Right res -> putStrLn res
@
-}
instance LLM Ollama where
  type LLMParams Ollama = OllamaChat.ChatOps
  type LLMStreamTokenType Ollama = OllamaChat.ChatResponse

  -- \| Generate text from a prompt
  --  Returns Left on API errors, Right on success.
  --
  --  Example:
  --  >>> generate (Ollama "llama3.2" []) "Hello" Nothing
  --  Right "Hello! How can I assist you today?"
  generate (Ollama model cbs) prompt mbOllamaParams = do
    mapM_ (\cb -> cb LLMStart) cbs
    let chatOps_ = fromMaybe OllamaChat.defaultChatOps mbOllamaParams
        msg = OllamaChat.userMessage prompt
        chatOps =
          chatOps_
            { OllamaChat.modelName = model
            , OllamaChat.messages = [msg]
            }

    eRes <- OllamaChat.chat chatOps Nothing
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError $ show err)) cbs
        return $ Left (llmError (T.pack $ show err) Nothing Nothing)
      Right chatResponse -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        case OllamaChat.message chatResponse of
          Nothing -> pure $ Left (Error.fromString "Message not found in response")
          Just m -> pure $ Right $ OllamaChat.content m

  -- \| Chat interaction with message history.
  --  Uses Ollama's chat API for multi-turn conversations.
  --
  --  Example:
  --  >>> let msgs = UserMessage "Hi" :| [AssistantMessage "Hello!"]
  --  >>> chat (Ollama "llama3" []) msgs Nothing
  --  Right "How are you today?"
  chat (Ollama model cbs) messages mbOllamaParams = do
    mapM_ (\cb -> cb LLMStart) cbs
    let chatOps_ = fromMaybe OllamaChat.defaultChatOps mbOllamaParams
        chatOps =
          chatOps_
            { OllamaChat.modelName = model
            , OllamaChat.messages = NonEmpty.map to messages
            }
    eRes <- OllamaChat.chat chatOps Nothing
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError $ show err)) cbs
        return $ Left (llmError (T.pack $ show err) Nothing Nothing)
      Right res -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        case OllamaChat.message res of
          Nothing ->
            return $
              Left $
                llmError
                  (T.pack $ "Message field not found: " <> show res)
                  Nothing
                  Nothing
          Just ollamaMsg -> return $ Right (from ollamaMsg)

  -- \| Streaming response handling.
  --  Processes tokens in real-time via StreamHandler.
  --
  --  Example:
  --  >>> let handler = StreamHandler (putStr . ("Token: " ++)) (putStrLn "Complete")
  --  >>> stream (Ollama "llama3" []) messages handler Nothing
  --  Token: H Token: i Complete
  --
  -- Note: Don't pass streamHandler in ChatOps's stream field. It will be overridden.
  stream
    (Ollama model_ cbs)
    messages
    StreamHandler {onToken, onComplete}
    mbOllamaParams = do
      let chatOps_ = fromMaybe OllamaChat.defaultChatOps mbOllamaParams
          chatOps =
            chatOps_
              { OllamaChat.modelName = model_
              , OllamaChat.messages = NonEmpty.map to messages
              , OllamaChat.stream =
                  Just
                    ( onToken
                    , pure ()
                    )
              }
      mapM_ (\cb -> cb LLMStart) cbs
      eRes <- OllamaChat.chat chatOps Nothing
      case eRes of
        Left err -> do
          mapM_ (\cb -> cb (LLMError $ show err)) cbs
          return $ Left (llmError (T.pack $ show err) Nothing Nothing)
        Right _ -> do
          onComplete
          mapM_ (\cb -> cb LLMEnd) cbs
          return $ Right ()

toOllamaRole :: Role -> OllamaChat.Role
toOllamaRole User = OllamaChat.User
toOllamaRole System = OllamaChat.System
toOllamaRole Assistant = OllamaChat.Assistant
toOllamaRole Tool = OllamaChat.Tool
toOllamaRole _ = OllamaChat.User -- Ollama only supports above 4 Roles, others will be defaulted to user

fromOllamaRole :: OllamaChat.Role -> Role
fromOllamaRole OllamaChat.User = User
fromOllamaRole OllamaChat.System = System
fromOllamaRole OllamaChat.Assistant = Assistant
fromOllamaRole OllamaChat.Tool = Tool

instance MessageConvertible OllamaChat.Message where
  to Message {..} =
    OllamaChat.Message
      (toOllamaRole role)
      content
      (messageImages messageData)
      (fmap toOllamaToolCall <$> toolCalls messageData)
      (thinking messageData)
    where
      toOllamaToolCall :: ToolCall -> O.ToolCall
      toOllamaToolCall ToolCall {..} =
        O.ToolCall
          { O.outputFunction =
              O.OutputFunction
                { O.outputFunctionName = toolFunctionName toolCallFunction
                , O.arguments = toolFunctionArguments toolCallFunction
                }
          }

  from (OllamaChat.Message role' content' imgs tools think) =
    Message
      { role = fromOllamaRole role'
      , content = content'
      , messageData =
          MessageData
            { messageImages = imgs
            , toolCalls = fmap toToolCall <$> tools
            , thinking = think
            , name = Nothing
            }
      }
    where
      toToolCall :: O.ToolCall -> ToolCall
      toToolCall O.ToolCall {..} =
        ToolCall
          { toolCallId = ""
          , toolCallType = "function"
          , toolCallFunction =
              ToolFunction
                { toolFunctionName = O.outputFunctionName outputFunction
                , toolFunctionArguments = O.arguments outputFunction
                }
          }

instance Run.Runnable Ollama where
  type RunnableInput Ollama = (ChatHistory, Maybe OllamaChat.ChatOps)
  type RunnableOutput Ollama = Message

  invoke = uncurry . chat

-- | Default values for Ollama
defaultOllama :: Ollama
defaultOllama = Ollama "llama3.2" []

{-# LANGUAGE NamedFieldPuns #-}
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
  , OllamaParams (..)
  , defaultOllamaParams

    -- * Re-export
  , module Langchain.LLM.Core
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Ollama.Chat as OllamaChat
import qualified Data.Ollama.Common.Types as O
import qualified Data.Ollama.Generate as OllamaGenerate
import Data.Text (Text)
import Langchain.Callback (Callback, Event (..))
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

{- | Ollama Params contains same fields GenerateOps and ChatOps
from [ollama-haskell](https://hackage.haskell.org/package/ollama-haskell)
-}
data OllamaParams = OllamaParams
  { suffix :: Maybe Text
  -- ^ An optional suffix to append to the generated text.
  , images :: Maybe [Text]
  -- ^ Optional list of base64 encoded images to include with the request.
  , format :: Maybe O.Format
  -- ^ An optional format specifier for the response.
  , system :: Maybe Text
  -- ^ Optional system text that can be included in the generation context.
  , template :: Maybe Text
  -- ^ An optional streaming function where the first function handles
  --   each chunk of response, and the second flushes the stream.
  --   ^ This will not work for chat and stream, use promptTemplate instead
  , raw :: Maybe Bool
  -- ^ An optional flag to return the raw response.
  , keepAlive :: Maybe Int
  -- ^ Optional text to specify keep-alive behavior.
  , hostUrl :: Maybe Text
  -- ^ Override default Ollama host url. Default url = "http://127.0.0.1:11434"
  , responseTimeOut :: Maybe Int
  -- ^ Override default response timeout in seconds. Default = 900 seconds
  , options :: Maybe O.ModelOptions
  -- ^ additional model parameters listed in the documentation for
  --   the Modelfile such as temperature
  , tools :: !(Maybe [O.InputTool])
  -- ^ Optional tools that may be used in the chat.
  --   Will only work for chat and stream and not Generate.
  , think :: !(Maybe Bool)
  -- ^ Optional flag to enable thinking mode.
  }
  deriving (Eq, Show)

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
  type LLMParams Ollama = OllamaParams

  -- \| Generate text from a prompt
  --  Returns Left on API errors, Right on success.
  --
  --  Example:
  --  >>> generate (Ollama "llama3.2" []) "Hello" Nothing
  --  Right "Hello! How can I assist you today?"
  generate (Ollama model cbs) prompt mbOllamaParams = do
    mapM_ (\cb -> cb LLMStart) cbs
    eRes <-
      OllamaGenerate.generate
        OllamaGenerate.defaultGenerateOps
          { OllamaGenerate.modelName = model
          , OllamaGenerate.prompt = prompt
          , OllamaGenerate.stream = Nothing
          , OllamaGenerate.suffix = maybe Nothing suffix mbOllamaParams
          , OllamaGenerate.images = maybe Nothing images mbOllamaParams
          , OllamaGenerate.format = maybe Nothing format mbOllamaParams
          , OllamaGenerate.system = maybe Nothing system mbOllamaParams
          , OllamaGenerate.template = maybe Nothing template mbOllamaParams
          , OllamaGenerate.raw = maybe Nothing raw mbOllamaParams
          , OllamaGenerate.keepAlive = maybe Nothing keepAlive mbOllamaParams
          , OllamaGenerate.options = maybe Nothing options mbOllamaParams
          , OllamaGenerate.think = maybe Nothing think mbOllamaParams
          }
        ( Just
            OllamaGenerate.defaultOllamaConfig
              { OllamaGenerate.hostUrl =
                  fromMaybe
                    (OllamaGenerate.hostUrl OllamaGenerate.defaultOllamaConfig)
                    (mbOllamaParams >>= hostUrl)
              , OllamaGenerate.timeout =
                  fromMaybe
                    (OllamaGenerate.timeout OllamaGenerate.defaultOllamaConfig)
                    (mbOllamaParams >>= responseTimeOut)
              }
        )
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError $ show err)) cbs
        return $ Left (show err)
      Right res -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        return $ Right (OllamaGenerate.genResponse res)

  -- \| Chat interaction with message history.
  --  Uses Ollama's chat API for multi-turn conversations.
  --
  --  Example:
  --  >>> let msgs = UserMessage "Hi" :| [AssistantMessage "Hello!"]
  --  >>> chat (Ollama "llama3" []) msgs Nothing
  --  Right "How are you today?"
  --
  chat (Ollama model cbs) messages mbOllamaParams = do
    mapM_ (\cb -> cb LLMStart) cbs
    let ops =
          OllamaChat.defaultChatOps
            { OllamaChat.chatModelName = model
            , OllamaChat.messages = NonEmpty.map to messages
            , OllamaChat.stream = Nothing
            , OllamaChat.tools = maybe Nothing tools mbOllamaParams
            , OllamaChat.format = maybe Nothing format mbOllamaParams
            , OllamaChat.keepAlive = maybe Nothing keepAlive mbOllamaParams
            , OllamaChat.options = maybe Nothing options mbOllamaParams
            , OllamaChat.think = maybe Nothing think mbOllamaParams
            }
    eRes <-
      OllamaChat.chat
        ops
        ( Just
            OllamaChat.defaultOllamaConfig
              { OllamaChat.hostUrl =
                  fromMaybe
                    (OllamaChat.hostUrl OllamaChat.defaultOllamaConfig)
                    (mbOllamaParams >>= hostUrl)
              , OllamaChat.timeout =
                  fromMaybe
                    (OllamaChat.timeout OllamaChat.defaultOllamaConfig)
                    (mbOllamaParams >>= responseTimeOut)
              }
        )
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError $ show err)) cbs
        return $ Left (show err)
      Right res -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        case OllamaChat.message res of
          Nothing -> return $ Left $ "Message field not found: " <> show res
          Just ollamaMsg -> return $ Right (from ollamaMsg)

  -- \| Streaming response handling.
  --  Processes tokens in real-time via StreamHandler.
  --
  --  Example:
  --  >>> let handler = StreamHandler (putStr . ("Token: " ++)) (putStrLn "Complete")
  --  >>> stream (Ollama "llama3" []) messages handler Nothing
  --  Token: H Token: i Complete
  --
  stream (Ollama model_ cbs) messages StreamHandler {onToken, onComplete} mbOllamaParams = do
    mapM_ (\cb -> cb LLMStart) cbs
    eRes <-
      OllamaChat.chat
        OllamaChat.defaultChatOps
          { OllamaChat.chatModelName = model_
          , OllamaChat.messages = NonEmpty.map to messages
          , OllamaChat.stream = Just $ onToken . maybe "" O.content . O.message
          , OllamaChat.tools = maybe Nothing tools mbOllamaParams
          , OllamaChat.format = maybe Nothing format mbOllamaParams
          , OllamaChat.keepAlive = maybe Nothing keepAlive mbOllamaParams
          , OllamaChat.options = maybe Nothing options mbOllamaParams
          }
        ( Just
            OllamaChat.defaultOllamaConfig
              { OllamaChat.hostUrl =
                  fromMaybe
                    (OllamaChat.hostUrl OllamaChat.defaultOllamaConfig)
                    (mbOllamaParams >>= hostUrl)
              , OllamaChat.timeout =
                  fromMaybe
                    (OllamaChat.timeout OllamaChat.defaultOllamaConfig)
                    (mbOllamaParams >>= responseTimeOut)
              }
        )
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError $ show err)) cbs
        return $ Left (show err)
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
  type RunnableInput Ollama = (ChatMessage, Maybe OllamaParams)
  type RunnableOutput Ollama = Message

  invoke = uncurry . chat

-- | Default values for OllamaParams
defaultOllamaParams :: OllamaParams
defaultOllamaParams =
  OllamaParams
    { suffix = Nothing
    , images = Nothing
    , format = Nothing
    , system = Nothing
    , template = Nothing
    , raw = Nothing
    , keepAlive = Nothing
    , hostUrl = Nothing
    , responseTimeOut = Nothing
    , options = Nothing
    , tools = Nothing
    , think = Nothing
    }

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Langchain.LLM.OpenAICompatible
Description : Generic OpenAI-compatible API integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides a generic 'OpenAICompatible' data type and
implements the 'LLM' typeclass for interacting with any service that provides
an OpenAI-compatible API interface.

Also provides convenience functions for popular providers like LMStudio, llama.cpp,
and OpenRouter.

Example usage:

@
-- Using with LMStudio
let lmStudio = mkLMStudio "my-model" [] Nothing Nothing
result <- generate lmStudio "What is functional programming?" Nothing

-- Using with OpenRouter
let openRouter = mkOpenRouter "anthropic/claude-3-opus" [] Nothing "your-api-key"
result <- generate openRouter "Explain Haskell monads" Nothing
@
-}
module Langchain.LLM.OpenAICompatible
  ( OpenAICompatible (..)
  -- , OpenAI.OpenAIParams (..)
  , mkLMStudio
  , mkLlamaCpp
  , mkOpenRouter
  , module Langchain.LLM.Core
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Langchain.Callback
import qualified Langchain.Error as Error
import Langchain.LLM.Core
import qualified Langchain.LLM.Core as LLM
import qualified Langchain.Runnable.Core as LLM
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import qualified OpenAI.V1.Chat.Completions as CreateCompletion (CreateChatCompletion (..))
import qualified OpenAI.V1.Chat.Completions as OpenAIV1
import qualified OpenAI.V1.ToolCall as OpenAIV1

-- | Generic OpenAICompatible implementation for any service with an OpenAI-compatible API
data OpenAICompatible = OpenAICompatible
  { apiKey :: T.Text
  -- ^ The API key for authenticating.
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations
  , baseUrl :: Maybe String
  -- ^ Base URL for the service. Default "https://api.openai.com"
  , providerName :: T.Text
  -- ^ The provider or service name
  }

instance Show OpenAICompatible where
  show OpenAICompatible {..} = show providerName

-- | Helper function to extract text from OpenAI Message T.Text
messageToText :: OpenAIV1.Message T.Text -> T.Text
messageToText (OpenAIV1.User {OpenAIV1.content = c}) = c
messageToText (OpenAIV1.System {OpenAIV1.content = c}) = c
messageToText (OpenAIV1.Assistant {OpenAIV1.assistant_content = ac}) = fromMaybe "" ac
messageToText (OpenAIV1.Tool {OpenAIV1.content = c}) = c

-- | Helper function to extract text from Vector Content
extractTextFromContent :: V.Vector OpenAIV1.Content -> T.Text
extractTextFromContent contents =
  fromMaybe "" $ listToMaybe $ V.toList $ V.mapMaybe getTextContent contents
  where
    getTextContent :: OpenAIV1.Content -> Maybe T.Text
    getTextContent (OpenAIV1.Text txt) = Just txt
    getTextContent _ = Nothing

{- | Helper function to create content list with text
TODO: Add support for images and tool calls when openai library types are better understood
-}
makeContentList :: T.Text -> Maybe [T.Text] -> V.Vector OpenAIV1.Content
makeContentList text mbImageData = do
  let res = V.fromList [OpenAIV1.Text text]
  res <> case mbImageData of
    Just images ->
      V.fromList
        ( map
            ( OpenAIV1.Image_URL
                . (\urlText -> OpenAIV1.ImageURL {url = urlText, detail = Nothing})
            )
            images
        )
    Nothing -> V.empty

toOpenAIToolCall :: [ToolCall] -> V.Vector OpenAIV1.ToolCall
toOpenAIToolCall = V.fromList . map go
  where
    go :: ToolCall -> OpenAIV1.ToolCall
    go = \case
      ToolCall {toolCallId, toolCallFunction = ToolFunction {toolFunctionName, toolFunctionArguments}} ->
        OpenAIV1.ToolCall_Function
          { OpenAIV1.id = toolCallId
          , OpenAIV1.function =
              OpenAIV1.Function
                { OpenAIV1.name = toolFunctionName
                , OpenAIV1.arguments = T.decodeUtf8 $ BSL.toStrict $ Aeson.encode toolFunctionArguments
                }
          }

fromOpenAIToolCall :: OpenAIV1.ToolCall -> ToolCall
fromOpenAIToolCall = \case
  OpenAIV1.ToolCall_Function
    { OpenAIV1.id = tcId
    , OpenAIV1.function =
      OpenAIV1.Function
        { OpenAIV1.name = fnName
        , OpenAIV1.arguments = fnArgs
        }
    } ->
      let argsVal = Aeson.decode (BSL.fromStrict $ T.encodeUtf8 fnArgs) :: Maybe Aeson.Value
          argsMap = case argsVal of
            Just (Aeson.Object o) -> KM.toMapText o
            _ -> mempty
       in ToolCall
            { toolCallId = tcId
            , toolCallType = "function"
            , toolCallFunction =
                ToolFunction
                  { toolFunctionName = fnName
                  , toolFunctionArguments = argsMap
                  }
            }

getToolId :: [ToolCall] -> T.Text
getToolId toolCalls = case toolCalls of
  (ToolCall {toolCallId} : _) -> toolCallId
  [] -> ""

getImageDataIfExists :: V.Vector OpenAIV1.Content -> Maybe [T.Text]
getImageDataIfExists contents =
  let images = V.toList $ V.mapMaybe getImageContent contents
   in if null images then Nothing else Just images
  where
    getImageContent :: OpenAIV1.Content -> Maybe T.Text
    getImageContent (OpenAIV1.Image_URL imgUrl) = Just $ url imgUrl
    getImageContent _ = Nothing

{- | MessageConvertible instance for OpenAIV1.Message (V.Vector OpenAIV1.Content)
This is used for request messages
-}
instance LLM.MessageConvertible (OpenAIV1.Message (V.Vector OpenAIV1.Content)) where
  -- \| Convert LLM.Message to OpenAIV1.Message (V.Vector OpenAIV1.Content)
  to :: LLM.Message -> OpenAIV1.Message (V.Vector OpenAIV1.Content)
  to msg =
    let imagesData = LLM.messageImages $ LLM.messageData msg
        contentVec = makeContentList (LLM.content msg) imagesData
        msgName = LLM.name $ LLM.messageData msg
     in case LLM.role msg of
          LLM.User ->
            OpenAIV1.User
              { OpenAIV1.content = contentVec
              , OpenAIV1.name = msgName
              }
          LLM.System ->
            OpenAIV1.System
              { OpenAIV1.content = contentVec
              , OpenAIV1.name = msgName
              }
          LLM.Assistant ->
            OpenAIV1.Assistant
              { OpenAIV1.assistant_content = Just contentVec
              , OpenAIV1.name = msgName
              , OpenAIV1.refusal = Nothing
              , OpenAIV1.assistant_audio = Nothing
              , OpenAIV1.tool_calls = fmap toOpenAIToolCall <$> toolCalls $ messageData msg
              }
          LLM.Tool ->
            OpenAIV1.Tool
              { OpenAIV1.content = contentVec
              , OpenAIV1.tool_call_id = fromMaybe "" $ fmap getToolId <$> toolCalls $ messageData msg
              }
          -- Fallback to User for unsupported roles (Developer, Function)
          _ ->
            OpenAIV1.User
              { OpenAIV1.content = contentVec
              , OpenAIV1.name = msgName
              }

  -- \| Convert OpenAIV1.Message (V.Vector OpenAIV1.Content) to LLM.Message
  from :: OpenAIV1.Message (V.Vector OpenAIV1.Content) -> LLM.Message
  from msg = case msg of
    OpenAIV1.User {OpenAIV1.content = c, OpenAIV1.name = n} ->
      LLM.Message
        { LLM.role = LLM.User
        , LLM.content = extractTextFromContent c
        , LLM.messageData =
            LLM.MessageData
              { LLM.name = n
              , LLM.toolCalls = Nothing
              , LLM.messageImages = getImageDataIfExists c
              , LLM.thinking = Nothing
              }
        }
    OpenAIV1.System {OpenAIV1.content = c, OpenAIV1.name = n} ->
      LLM.Message
        { LLM.role = LLM.System
        , LLM.content = extractTextFromContent c
        , LLM.messageData =
            LLM.MessageData
              { LLM.name = n
              , LLM.toolCalls = Nothing
              , LLM.messageImages = getImageDataIfExists c
              , LLM.thinking = Nothing
              }
        }
    OpenAIV1.Assistant
      { OpenAIV1.assistant_content = ac
      , OpenAIV1.name = n
      , OpenAIV1.tool_calls = mbToolVector
      } ->
        LLM.Message
          { LLM.role = LLM.Assistant
          , LLM.content = maybe "" extractTextFromContent ac
          , LLM.messageData =
              LLM.MessageData
                { LLM.name = n
                , LLM.toolCalls = fmap (V.toList . V.map fromOpenAIToolCall) mbToolVector
                , LLM.messageImages = getImageDataIfExists =<< ac
                , LLM.thinking = Nothing
                }
          }
    OpenAIV1.Tool {OpenAIV1.content = c, OpenAIV1.tool_call_id = toolCallid} ->
      LLM.Message
        { LLM.role = LLM.Tool
        , LLM.content = extractTextFromContent c
        , LLM.messageData =
            LLM.MessageData
              { LLM.name = Nothing
              , LLM.toolCalls =
                  Just
                    [ ToolCall
                        { toolCallId = toolCallid
                        , toolCallType = "function"
                        , toolCallFunction =
                            ToolFunction
                              { toolFunctionName = ""
                              , toolFunctionArguments = mempty
                              }
                        }
                    ]
              , LLM.messageImages = getImageDataIfExists c
              , LLM.thinking = Nothing
              }
        }

instance LLM.MessageConvertible (OpenAIV1.Message T.Text) where
  to :: LLM.Message -> OpenAIV1.Message T.Text
  to _ = error "Conversion to OpenAIV1.Message T.Text not implemented."

  -- \| Convert OpenAIV1.Message T.Text to LLM.Message
  from :: OpenAIV1.Message T.Text -> LLM.Message
  from msg = case msg of
    OpenAIV1.User {OpenAIV1.content = c, OpenAIV1.name = n} ->
      LLM.Message
        { LLM.role = LLM.User
        , LLM.content = c
        , LLM.messageData =
            LLM.MessageData
              { LLM.name = n
              , LLM.toolCalls = Nothing
              , LLM.messageImages = Nothing
              , LLM.thinking = Nothing
              }
        }
    OpenAIV1.System {OpenAIV1.content = c, OpenAIV1.name = n} ->
      LLM.Message
        { LLM.role = LLM.System
        , LLM.content = c
        , LLM.messageData =
            LLM.MessageData
              { LLM.name = n
              , LLM.toolCalls = Nothing
              , LLM.messageImages = Nothing
              , LLM.thinking = Nothing
              }
        }
    OpenAIV1.Assistant
      { OpenAIV1.assistant_content = ac
      , OpenAIV1.name = n
      , OpenAIV1.tool_calls = mbToolVector
      } ->
        LLM.Message
          { LLM.role = LLM.Assistant
          , LLM.content = fromMaybe "" ac
          , LLM.messageData =
              LLM.MessageData
                { LLM.name = n
                , LLM.toolCalls = fmap (V.toList . V.map fromOpenAIToolCall) mbToolVector
                , LLM.messageImages = Nothing
                , LLM.thinking = Nothing
                }
          }
    OpenAIV1.Tool {OpenAIV1.content = c, OpenAIV1.tool_call_id = toolCallid} ->
      LLM.Message
        { LLM.role = LLM.Tool
        , LLM.content = c
        , LLM.messageData =
            LLM.MessageData
              { LLM.name = Nothing
              , LLM.toolCalls =
                  Just
                    [ ToolCall
                        { toolCallId = toolCallid
                        , toolCallType = "function"
                        , toolCallFunction =
                            ToolFunction
                              { toolFunctionName = ""
                              , toolFunctionArguments = mempty
                              }
                        }
                    ]
              , LLM.messageImages = Nothing
              , LLM.thinking = Nothing
              }
        }

-- | Helper function to convert LLM.Message to OpenAI Message (using MessageConvertible)
toOpenAIMsg :: LLM.Message -> OpenAIV1.Message (V.Vector OpenAIV1.Content)
toOpenAIMsg = LLM.to

-- | Helper function to convert OpenAI Message to LLM.Message (using MessageConvertible)
fromOpenAIMsg :: OpenAIV1.Message T.Text -> LLM.Message
fromOpenAIMsg = LLM.from

instance LLM.LLM OpenAICompatible where
  type LLMParams OpenAICompatible = OpenAIV1.CreateChatCompletion
  type LLMStreamTokenType OpenAICompatible = OpenAIV1.ChatCompletionChunk

  generate OpenAICompatible {..} prompt mbLLMParams = do
    clientEnv <- getClientEnv $ maybe "https://api.openai.com" T.pack baseUrl
    let Methods {createChatCompletion} = makeMethods clientEnv apiKey Nothing Nothing
    eRes <-
      try $
        createChatCompletion
          _CreateChatCompletion
            { OpenAIV1.messages =
                V.fromList
                  [ OpenAIV1.User
                      { OpenAIV1.content = V.fromList [OpenAIV1.Text prompt]
                      , name = Nothing
                      }
                  ]
            , CreateCompletion.model = maybe "gpt-4o-mini" CreateCompletion.model mbLLMParams
            }
    case eRes of
      Left err -> pure $ Left $ Error.fromString $ show (err :: SomeException)
      Right (ChatCompletionObject {choices}) -> do
        let Choice {message} = V.head choices
        pure (Right $ messageToText message)

  chat OpenAICompatible {..} chatHistory mbLLMParams = do
    clientEnv <- getClientEnv $ maybe "https://api.openai.com" T.pack baseUrl
    let Methods {createChatCompletion} = makeMethods clientEnv apiKey Nothing Nothing
    eRes <-
      try $
        createChatCompletion
          _CreateChatCompletion
            { OpenAIV1.messages =
                V.fromList $ map toOpenAIMsg (NE.toList chatHistory)
            , CreateCompletion.model = maybe "gpt-4o-mini" CreateCompletion.model mbLLMParams
            }
    case eRes of
      Left err -> pure $ Left $ Error.fromString $ show (err :: SomeException)
      Right (ChatCompletionObject {choices}) -> do
        let Choice {message} = V.head choices
        pure (Right $ fromOpenAIMsg message)

  stream OpenAICompatible {..} chatHistory streamHandler mbLLMParams = do
    let onEvent (Left _) = pure () -- ignore for now
        onEvent (Right chunk) = onToken streamHandler chunk

    clientEnv <- getClientEnv $ maybe "https://api.openai.com" T.pack baseUrl
    let Methods {createChatCompletionStreamTyped} = makeMethods clientEnv apiKey Nothing Nothing
    let req_ =
          _CreateChatCompletion
            { OpenAIV1.messages =
                V.fromList $ map toOpenAIMsg (NE.toList chatHistory)
            , CreateCompletion.model = maybe "gpt-4o-mini" CreateCompletion.model mbLLMParams
            }
    _ <- createChatCompletionStreamTyped req_ onEvent
    pure $ Right ()

-- | Create an LMStudio instance
mkLMStudio :: [Callback] -> Maybe String -> T.Text -> OpenAICompatible
mkLMStudio callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = apiKey'
    , callbacks = callbacks'
    , baseUrl = Just $ fromMaybe "http://localhost:1234/v1" baseUrl'
    , providerName = "LMStudio"
    }

-- | Create a llama.cpp instance
mkLlamaCpp :: [Callback] -> Maybe String -> T.Text -> OpenAICompatible
mkLlamaCpp callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = apiKey'
    , callbacks = callbacks'
    , baseUrl = Just $ fromMaybe "http://localhost:8080/v1" baseUrl'
    , providerName = "LlamaCpp"
    }

{- | Create an OpenRouter instance
OpenRouter provides access to multiple model providers through a single API
Model name should be in the format "provider/model" (e.g., "anthropic/claude-3-opus")
-}
mkOpenRouter :: [Callback] -> Maybe String -> T.Text -> OpenAICompatible
mkOpenRouter callbacks' baseUrl' apiKey' =
  OpenAICompatible
    { apiKey = apiKey' -- OpenRouter requires an API key
    , callbacks = callbacks'
    , baseUrl = Just $ fromMaybe "https://openrouter.ai" baseUrl'
    , providerName = "OpenRouter"
    }

instance LLM.Runnable OpenAICompatible where
  type RunnableInput OpenAICompatible = (ChatHistory, Maybe OpenAIV1.CreateChatCompletion)
  type RunnableOutput OpenAICompatible = LLM.Message

  invoke = uncurry . chat

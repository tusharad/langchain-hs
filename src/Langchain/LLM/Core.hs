{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      Langchain.LLM.Core
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: Core implementation of langchain chat models
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

This module provides the core types and typeclasses for the Langchain library in Haskell,
which is designed to facilitate interaction with language models (LLMs). It defines a standardized
interface that allows different LLM implementations to be used interchangeably, promoting code reuse
and modularity.

The main components include:

* The 'LLM' typeclass, which defines the interface for language models.

* Data types such as 'Message' for conversation messages,
  and 'StreamHandler' for handling streaming responses.

* Default values like 'defaultParams' and 'defaultMessageData' for convenience.

This module is intended to be used as the foundation for building applications that interact with LLMs,
providing a consistent API across different model implementations.
-}
module Langchain.LLM.Core
  ( -- * LLM Typeclass
    LLM (..)

    -- * Parameters
  , Message (..)
  , Role (..)
  , ChatMessage
  , MessageData (..)
  , ToolCall (..)
  , ToolFunction (..)
  , StreamHandler (..)
  , MessageConvertible (..)

    -- * Default Values
  , defaultMessageData
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty
import qualified Data.Map as HM
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Langchain.Error (LangchainResult)

{- | Callbacks for handling streaming responses from a language model.
This allows real-time processing of tokens as they are generated and an action
upon completion.

@
printHandler :: StreamHandler
printHandler = StreamHandler
  { onToken = putStrLn . ("Token: " ++)
  , onComplete = putStrLn "Streaming complete"
  }
@
-}
data StreamHandler tokenType = StreamHandler
  { onToken :: tokenType -> IO ()
  -- ^ Action to perform for each token received
  , onComplete :: IO ()
  -- ^ Action to perform when streaming is complete
  }

-- | Enumeration of possible roles in a conversation.
data Role
  = -- | System role, typically for instructions or context
    System
  | -- | User role, for user inputs
    User
  | -- | Assistant role, for model responses
    Assistant
  | -- | Tool role, for tool outputs or interactions
    Tool
  | Developer
  | -- | Special role for developer messages. Specific to only some integrations
    Function
  deriving
    ( -- | Function call messages. Specific to only some integrations
      Eq
    , Show
    , Generic
    , ToJSON
    , FromJSON
    )

{- | Represents a message in a conversation, including the sender's role, content,
and additional metadata.
https://python.langchain.com/docs/concepts/messages/

@
userMsg :: Message
userMsg = Message
  { role = User
  , content = "Explain functional programming"
  , messageData = defaultMessageData
  }
@
-}
data Message = Message
  { role :: Role
  -- ^ The role of the message sender
  , content :: Text
  -- ^ The content of the message
  , messageData :: MessageData
  -- ^ Additional data associated with the message
  }
  deriving (Eq, Show)

-- Function call details
data ToolFunction = ToolFunction
  { toolFunctionName :: Text
  , toolFunctionArguments :: HM.Map Text Value
  }
  deriving (Show, Eq)

-- Main tool call structure
data ToolCall = ToolCall
  { toolCallId :: Text
  , toolCallType :: Text
  , toolCallFunction :: ToolFunction
  }
  deriving (Show, Eq)

-- ToJSON instance for ToolFunction
instance ToJSON ToolFunction where
  toJSON (ToolFunction name args) =
    object
      [ "name" .= name
      , "arguments" .= args
      ]

-- FromJSON instance for ToolFunction
instance FromJSON ToolFunction where
  parseJSON = withObject "ToolFunction" $ \obj -> do
    name <- obj .: "name"
    argsVal <- obj .: "arguments"
    args <- case argsVal of
      Object o -> pure $ KM.toMapText o
      String s -> case decodeStrict (encodeUtf8 s) of
        Just (Object o) -> pure $ KM.toMapText o
        _ -> fail "ToolFunction.arguments: expected object or JSON-encoded object string"
      _ -> fail "ToolFunction.arguments: expected object or string"
    return $ ToolFunction name args

-- ToJSON instance for ToolCall
instance ToJSON ToolCall where
  toJSON (ToolCall callId callType func) =
    object
      [ "id" .= callId
      , "type" .= callType
      , "function" .= func
      ]

-- FromJSON instance for ToolCall
instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \obj -> do
    callId <- obj .: "id"
    callType <- obj .: "type"
    func <- obj .: "function"
    return $ ToolCall callId callType func

{- | Additional data for a message, such as a name or tool calls.
This type is designed for extensibility, allowing new fields to be added without
breaking changes. Use 'defaultMessageData' for typical usage.
-}
data MessageData = MessageData
  { name :: Maybe Text
  -- ^ Optional name associated with the message
  , toolCalls :: Maybe [ToolCall]
  -- ^ Optional list of tool calls invoked by the message
  , messageImages :: Maybe [Text]
  -- ^ Base64 encoded image data list
  , thinking :: Maybe Text
  -- ^ Thinking
  }
  deriving (Eq, Show)

-- | JSON serialization for MessageData.
instance ToJSON MessageData where
  toJSON MessageData {..} =
    object
      [ "name" .= name
      , "tool_calls" .= toolCalls
      , "images" .= messageImages
      , "thinking" .= thinking
      -- Add more fields as they are added
      ]

-- | JSON deserialization for MessageData.
instance FromJSON MessageData where
  parseJSON = withObject "MessageData" $ \v ->
    MessageData
      <$> v .:? "name"
      <*> v .:? "tool_calls"
      <*> v .:? "images"
      <*> v .:? "thinking"

-- | Type alias for NonEmpty Message
type ChatMessage = NonEmpty Message

{- | Default message data with all fields set to Nothing.
Use this for standard messages without additional metadata
-}
defaultMessageData :: MessageData
defaultMessageData =
  MessageData
    { name = Nothing
    , toolCalls = Nothing
    , messageImages = Nothing
    , thinking = Nothing
    }

-- | Typeclass that all ChatModels should interface with
class LLM llm where
  -- | Define the Parameter type for your LLM model.
  type LLMStreamTokenType llm

  type LLMParams llm

  {- | Invoke the language model with a single prompt.
       Suitable for simple queries; returns either an error or generated text.
  -}
  generate ::
    -- | The type of the language model instance.
    llm ->
    -- | The prompt to send to the model.
    Text ->
    -- | Optional configuration parameters.
    Maybe (LLMParams llm) ->
    IO (LangchainResult Text)

  {- | Chat with the language model using a sequence of messages.
  Suitable for multi-turn conversations; returns either an error or the response.
  -}
  chat ::
    -- | The type of the language model instance.
    llm ->
    -- | A non-empty list of messages to send to the model.
    ChatMessage ->
    -- | Optional configuration parameters.
    Maybe (LLMParams llm) ->
    -- | The result of the chat, either an error or the response text.
    IO (LangchainResult Message)

  {- | Stream responses from the language model for a sequence of messages.
  Uses callbacks to process tokens in real-time; returns either an error or unit.
  -}
  stream ::
    llm ->
    ChatMessage ->
    StreamHandler (LLMStreamTokenType llm) ->
    Maybe (LLMParams llm) ->
    IO (LangchainResult ())

  -- Default implementations

  -- | MonadIO version of generate
  generateM ::
    MonadIO m =>
    -- | The type of the language model instance.
    llm ->
    -- | The prompt to send to the model.
    Text ->
    -- | Optional configuration parameters.
    Maybe (LLMParams llm) ->
    m (LangchainResult Text)
  generateM llm prompt mbParams = liftIO $ generate llm prompt mbParams

  -- | MonadIO version of chat
  chatM ::
    MonadIO m =>
    -- | The type of the language model instance.
    llm ->
    -- | A non-empty list of messages to send to the model.
    ChatMessage ->
    -- | Optional configuration parameters.
    Maybe (LLMParams llm) ->
    -- | The result of the chat, either an error or the response text.
    m (LangchainResult Message)
  chatM llm chatHistory mbParams = liftIO $ chat llm chatHistory mbParams

  -- | MonadIO version of stream
  streamM ::
    MonadIO m =>
    llm ->
    ChatMessage ->
    StreamHandler (LLMStreamTokenType llm) ->
    Maybe (LLMParams llm) ->
    m (LangchainResult ())
  streamM llm chatHistory sHandler mbParams = liftIO $ stream llm chatHistory sHandler mbParams

class MessageConvertible a where
  to :: Message -> a
  from :: a -> Message

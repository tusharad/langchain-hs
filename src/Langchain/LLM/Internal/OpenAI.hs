{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.LLM.Internal.OpenAI
Description : Internal module for OpenAI chat completion API interactions
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides data types and functions to interact with OpenAI's chat completion API. It includes types for requests, responses, and streaming handlers, as well as functions to create and handle chat completion requests and streams. Designed for internal use within the LangChain library, it can also be used directly for fine-grained control over API interactions.

Key Features

- Data types for chat completion requests and responses
- Support for streaming chat completions with real-time token processing
- Default values and configurations for common use cases
- Comprehensive error handling for API interactions

### Example Usage

@
import Data.Text (Text)
import Langchain.LLM.Internal.OpenAI

-- Create a simple message
let message = defaultMessage { role = User, content = Just (StringContent "Hello, how are you?") }

-- Create a chat completion request
let request = defaultChatCompletionRequest
      { messages = [message]
      , model = "gpt-3.5-turbo"
      , temperature = Just 0.7
      }

-- Send the request
response <- createChatCompletion "your-api-key" request
case response of
  Right res -> print (choices res)
  Left err -> putStrLn $ "Error: " ++ err
@

- Streaming Chat Completions:

@
import Langchain.LLM.Internal.OpenAI

-- Define a stream handler
let handler = OpenAIStreamHandler
      { onToken = \chunk -> putStr (maybe "" id $ contentForDelta $ delta $ head $ chunkChoices chunk)
      , onComplete = putStrLn "Stream complete"
      }

-- Create a streaming request
let streamRequest = request { stream = Just True }

-- Start streaming
result <- createChatCompletionStream "your-api-key" streamRequest handler
case result of
  Right () -> putStrLn "Streaming completed successfully"
  Left err -> putStrLn $ "Error: " ++ err
@

-}
module Langchain.LLM.Internal.OpenAI
  ( 
    -- * Types
    ChatCompletionChunk (..)
  , ChunkChoice (..)
  , Delta (..)
  , OpenAIStreamHandler (..)
  , ChatCompletionRequest (..)
  , ChatCompletionResponse (..)
  , Message (..)
  , Role (..)
  , MessageContent (..)
  , TextContent (..)
  , Tool_ (..)
  , Function_ (..)
  , ToolCall (..)
  , FunctionCall_ (..)
  , Usage (..)
  , Choice (..)
  , FinishReason (..)
  , LogProbs (..)
  , LogProbContent (..)
  , TopLogProb (..)
  , AudioConfig (..)
  , AudioResponse (..)
  , Modality (..)
  , ToolChoice (..)
  , SpecificToolChoice (..)
  , ReasoningEffort (..)
  , PredictionOutput (..)
  , PredictionContent (..)
  , ResponseFormat (..)
  , StreamOptions (..)
  , WebSearchOptions (..)
  , UserLocation (..)
  , ApproximateLocation (..)
  , CompletionTokensDetails (..)
  , PromptTokensDetails (..)
  , OpenAIParams (..)
  -- * Default values for types
  , defaultChatCompletionRequest
  , createChatCompletion
  , createChatCompletionStream
  , defaultMessage
  , defaultOpenAIParams
  , defaultPredictionOutput
  , defaultResponseFormat
  , defaultStreamOptions
  , defaultWebSearchOptions
  , defaultUserLocation
  , defaultAudioConfig
  , defaultToolChoice
  , defaultSpecificToolChoice
  , defaultReasoningEffort
  , defaultFunction
  ) where

import Conduit
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusCode)

{- | Represents a chunk of the chat completion response in a streaming context.
Contains a list of possible choices for the completion.
-}
data ChatCompletionChunk = ChatCompletionChunk
  { chunkChoices :: [ChunkChoice]
  -- ^ List of choices in this chunk of the response
  }
  deriving (Show)

instance FromJSON ChatCompletionChunk where
  parseJSON = withObject "ChatCompletionChunk" $ \v ->
    ChatCompletionChunk <$> v .: "choices"

{- | Represents a single choice in a chat completion chunk.
Includes the incremental content and an optional reason for finishing.
-}
data ChunkChoice = ChunkChoice
  { delta :: Delta
  -- ^ Incremental content added in this chunk
  , finishReason :: Maybe FinishReason
  -- ^ Reason why the completion stopped, if applicable
  }
  deriving (Show)

instance FromJSON ChunkChoice where
  parseJSON = withObject "ChunkChoice" $ \v ->
    ChunkChoice <$> v .: "delta" <*> v .:? "finish_reason"

-- | Represents the incremental content added in a chat completion chunk.
data Delta = Delta
  { contentForDelta :: Maybe Text
  -- ^ Optional text content added in this chunk
  }
  deriving (Show)

instance FromJSON Delta where
  parseJSON = withObject "Delta" $ \v ->
    Delta <$> v .:? "content"

{- | Handler for streaming chat completion responses.
Provides callbacks for processing each token and handling stream completion.
-}
data OpenAIStreamHandler = OpenAIStreamHandler
  { onToken :: ChatCompletionChunk -> IO ()
  -- ^ Callback for each token (chunk) received
  , onComplete :: IO ()
  -- ^ Callback when the stream is complete
  }

{- | Represents the main request for chat completions.
Contains all parameters for configuring the OpenAI chat completion API call.
-}
data ChatCompletionRequest = ChatCompletionRequest
  { messages :: [Message]
  -- ^ List of messages in the conversation history
  , model :: Text
  -- ^ The model to use for completion (e.g., "gpt-3.5-turbo")
  , frequencyPenalty :: Maybe Double
  -- ^ Penalty for frequent tokens (range: -2.0 to 2.0)
  , logitBias :: Maybe (Map Text Double)
  -- ^ Bias for specific tokens
  , logprobs :: Maybe Bool
  -- ^ Whether to return log probabilities
  , maxCompletionTokens :: Maybe Int
  -- ^ Maximum tokens to generate in the completion
  , maxTokens :: Maybe Int
  -- ^ Maximum tokens in the response
  , metadata :: Maybe (Map Text Text)
  -- ^ Metadata to attach to the request
  , modalities :: Maybe [Modality]
  -- ^ Modalities to use (e.g., text, audio)
  , n :: Maybe Int
  -- ^ Number of completions to generate
  , parallelToolCalls :: Maybe Bool
  -- ^ Whether to allow parallel tool calls
  , prediction :: Maybe PredictionOutput
  -- ^ Prediction output configuration
  , presencePenalty :: Maybe Double
  -- ^ Penalty for new tokens (range: -2.0 to 2.0)
  , reasoningEffort :: Maybe ReasoningEffort
  -- ^ Level of reasoning effort
  , responseFormat :: Maybe ResponseFormat
  -- ^ Format of the response (e.g., JSON)
  , seed :: Maybe Int
  -- ^ Seed for deterministic outputs
  , serviceTier :: Maybe Text
  -- ^ Service tier to use
  , stop :: Maybe (Either Text [Text])
  -- ^ Stop sequences to end the completion
  , store :: Maybe Bool
  -- ^ Whether to store the conversation
  , stream :: Maybe Bool
  -- ^ Whether to stream the response
  , streamOptions :: Maybe StreamOptions
  -- ^ Options for streaming
  , temperature :: Maybe Double
  -- ^ Sampling temperature (range: 0.0 to 2.0)
  , toolChoice :: Maybe ToolChoice
  -- ^ How to choose tools for the model
  , tools :: Maybe [Tool_]
  -- ^ Tools available to the model
  , topLogprobs :: Maybe Int
  -- ^ Number of top log probabilities to return
  , topP :: Maybe Double
  -- ^ Nucleus sampling parameter (range: 0.0 to 1.0)
  , user :: Maybe Text
  -- ^ User identifier
  , webSearchOptions :: Maybe WebSearchOptions
  -- ^ Options for web search features
  , audio :: Maybe AudioConfig
  -- ^ Configuration for audio processing
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChatCompletionRequest where
  toJSON ChatCompletionRequest {..} =
    object $
      [ "messages" .= messages
      , "model" .= model
      ]
        ++ maybe [] (\fp -> ["frequency_penalty" .= fp]) frequencyPenalty
        ++ maybe [] (\lb -> ["logit_bias" .= lb]) logitBias
        ++ maybe [] (\lp -> ["logprobs" .= lp]) logprobs
        ++ maybe [] (\mt -> ["max_completion_tokens" .= mt]) maxCompletionTokens
        ++ maybe [] (\mt -> ["max_tokens" .= mt]) maxTokens
        ++ maybe [] (\md -> ["metadata" .= md]) metadata
        ++ maybe [] (\m -> ["modalities" .= m]) modalities
        ++ maybe [] (\n' -> ["n" .= n']) n
        ++ maybe [] (\ptc -> ["parallel_tool_calls" .= ptc]) parallelToolCalls
        ++ maybe [] (\p -> ["prediction" .= p]) prediction
        ++ maybe [] (\pp -> ["presence_penalty" .= pp]) presencePenalty
        ++ maybe [] (\re -> ["reasoning_effort" .= re]) reasoningEffort
        ++ maybe [] (\rf -> ["response_format" .= rf]) responseFormat
        ++ maybe [] (\s -> ["seed" .= s]) seed
        ++ maybe [] (\st -> ["service_tier" .= st]) serviceTier
        ++ maybe [] (\s -> ["stop" .= s]) stop
        ++ maybe [] (\s -> ["store" .= s]) store
        ++ maybe [] (\s -> ["stream" .= s]) stream
        ++ maybe [] (\so -> ["stream_options" .= so]) streamOptions
        ++ maybe [] (\t -> ["temperature" .= t]) temperature
        ++ maybe [] (\tc -> ["tool_choice" .= tc]) toolChoice
        ++ maybe [] (\t -> ["tools" .= t]) tools
        ++ maybe [] (\tlp -> ["top_logprobs" .= tlp]) topLogprobs
        ++ maybe [] (\tp -> ["top_p" .= tp]) topP
        ++ maybe [] (\u -> ["user" .= u]) user
        ++ maybe [] (\wso -> ["web_search_options" .= wso]) webSearchOptions
        ++ maybe [] (\a -> ["audio" .= a]) audio

{- | Represents the response from a chat completion request.
Contains the generated choices, metadata, and usage information.
-}
data ChatCompletionResponse = ChatCompletionResponse
  { choices :: [Choice]
  -- ^ List of completion choices
  , created :: Integer
  -- ^ Timestamp of when the response was created
  , id_ :: Text
  -- ^ Unique identifier for the response
  , responseModel :: Text
  -- ^ The model used for the completion
  , object_ :: Text
  -- ^ Type of the response object
  , serviceTier :: Maybe Text
  -- ^ Service tier used
  , systemFingerprint :: Text
  -- ^ System fingerprint
  , usage :: Usage
  -- ^ Token usage information
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChatCompletionResponse where
  parseJSON = withObject "ChatCompletionResponse" $ \v ->
    ChatCompletionResponse
      <$> v .: "choices"
      <*> v .: "created"
      <*> v .: "id"
      <*> v .: "model"
      <*> v .: "object"
      <*> v .:? "service_tier"
      <*> v .: "system_fingerprint"
      <*> v .: "usage"

{- | Represents a single message in a conversation.
Contains the role, content, and optional metadata like function calls or audio responses.
-}
data Message = Message
  { role :: Role
  -- ^ The role of the message sender
  , content :: Maybe MessageContent
  -- ^ The content of the message
  , name :: Maybe Text
  -- ^ Optional name of the sender
  , functionCall :: Maybe FunctionCall_
  -- ^ Optional function call information
  , toolCalls :: Maybe [ToolCall]
  -- ^ Optional tool call information
  , toolCallId :: Maybe Text
  -- ^ Optional tool call ID
  , audio :: Maybe AudioResponse
  -- ^ Optional audio response
  , refusal :: Maybe Text
  -- ^ Optional refusal reason
  }
  deriving (Show, Eq, Generic)

-- | Default message with User role and no content.
defaultMessage :: Message
defaultMessage =
  Message
    { role = User
    , content = Nothing
    , name = Nothing
    , functionCall = Nothing
    , toolCalls = Nothing
    , toolCallId = Nothing
    , audio = Nothing
    , refusal = Nothing
    }

instance ToJSON Message where
  toJSON Message {..} =
    object $
      ["role" .= role]
        ++ maybe [] (\c -> ["content" .= c]) content
        ++ maybe [] (\n -> ["name" .= n]) name
        ++ maybe [] (\fc -> ["function_call" .= fc]) functionCall
        ++ maybe [] (\tc -> ["tool_calls" .= tc]) toolCalls
        ++ maybe [] (\tcid -> ["tool_call_id" .= tcid]) toolCallId
        ++ maybe [] (\a -> ["audio" .= a]) audio
        ++ maybe [] (\r -> ["refusal" .= r]) refusal

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "role"
      <*> v .:? "content"
      <*> v .:? "name"
      <*> v .:? "function_call"
      <*> v .:? "tool_calls"
      <*> v .:? "tool_call_id"
      <*> v .:? "audio"
      <*> v .:? "refusal"

{- | Represents different roles in a conversation.
Each role has a specific meaning in the context of the chat.
-}
data Role
  = -- | Human user input
    User
  | -- | AI-generated response
    Assistant
  | -- | System-level instructions
    System
  | -- | Special role for developer messages
    Developer
  | -- | Tool interaction messages
    Tool
  | -- | Function call messages
    Function
  deriving (Show, Eq, Generic)

instance ToJSON Role where
  toJSON User = String "user"
  toJSON Assistant = String "assistant"
  toJSON System = String "system"
  toJSON Developer = String "developer"
  toJSON Tool = String "tool"
  toJSON Function = String "function"

instance FromJSON Role where
  parseJSON (String "user") = return User
  parseJSON (String "assistant") = return Assistant
  parseJSON (String "system") = return System
  parseJSON (String "developer") = return Developer
  parseJSON (String "tool") = return Tool
  parseJSON (String "function") = return Function
  parseJSON invalid = fail $ "Invalid role: " ++ show invalid

-- | Represents the content of a message, which can be a simple string or structured parts.
data MessageContent
  = -- | Simple text content
    StringContent Text
  | -- | Structured content parts
    ContentParts [TextContent]
  deriving (Show, Eq, Generic)

instance ToJSON MessageContent where
  toJSON (StringContent text) = String text
  toJSON (ContentParts parts) = toJSON parts

instance FromJSON MessageContent where
  parseJSON (String s) = return $ StringContent s
  parseJSON (Array arr) = ContentParts <$> parseJSON (Array arr)
  parseJSON invalid = fail $ "Invalid message content: " ++ show invalid

-- | Represents a piece of text content with a type.
data TextContent = TextContent
  { text_ :: Text
  -- ^ The text content
  , contentType :: Text
  -- ^ The type of the content
  }
  deriving (Show, Eq, Generic)

instance ToJSON TextContent where
  toJSON TextContent {..} =
    object
      [ "text" .= text_
      , "type" .= contentType
      ]

instance FromJSON TextContent where
  parseJSON = withObject "TextContent" $ \v ->
    TextContent
      <$> v .: "text"
      <*> v .: "type"

-- | Represents a tool that can be used in the conversation.
data Tool_ = Tool_
  { toolType :: Text
  -- ^ The type of the tool
  , function :: Function_
  -- ^ The function associated with the tool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Tool_ where
  toJSON Tool_ {..} =
    object
      [ "type" .= toolType
      , "function" .= function
      ]

instance FromJSON Tool_ where
  parseJSON = withObject "Tool" $ \v ->
    Tool_
      <$> v .: "type"
      <*> v .: "function"

-- | Represents a function that can be called by the model.
data Function_ = Function_
  { name :: Text
  -- ^ The name of the function
  , description :: Maybe Text
  -- ^ Optional description of the function
  , parameters :: Maybe Value
  -- ^ Optional parameters for the function
  , strict :: Maybe Bool
  -- ^ Optional strictness flag
  }
  deriving (Show, Eq, Generic)

instance ToJSON Function_ where
  toJSON Function_ {..} =
    object $
      [ "name" .= name
      ]
        ++ maybe [] (\d -> ["description" .= d]) description
        ++ maybe [] (\p -> ["parameters" .= p]) parameters
        ++ maybe [] (\s -> ["strict" .= s]) strict

instance FromJSON Function_ where
  parseJSON = withObject "Function" $ \v ->
    Function_
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .:? "parameters"
      <*> v .:? "strict"

-- | Represents a call to a tool.
data ToolCall = ToolCall
  { id_ :: Text
  -- ^ The ID of the tool call
  , toolType :: Text
  -- ^ The type of the tool
  , function :: FunctionCall_
  -- ^ The function call
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON ToolCall {..} =
    object
      [ "id" .= id_
      , "type" .= toolType
      , "function" .= function
      ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v ->
    ToolCall
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "function"

-- | Represents a call to a function.
data FunctionCall_ = FunctionCall_
  { name :: Text
  -- ^ The name of the function
  , arguments :: Text
  -- ^ The arguments for the function
  }
  deriving (Show, Eq, Generic)

instance ToJSON FunctionCall_ where
  toJSON FunctionCall_ {..} =
    object
      [ "name" .= name
      , "arguments" .= arguments
      ]

instance FromJSON FunctionCall_ where
  parseJSON = withObject "FunctionCall" $ \v ->
    FunctionCall_
      <$> v .: "name"
      <*> v .: "arguments"

-- | Represents token usage information.
data Usage = Usage
  { completionTokens :: Int
  -- ^ Tokens used in completion
  , promptTokens :: Int
  -- ^ Tokens in the prompt
  , totalTokens :: Int
  -- ^ Total tokens used
  , completionTokensDetails :: Maybe CompletionTokensDetails
  -- ^ Detailed completion token info
  , promptTokensDetails :: Maybe PromptTokensDetails
  -- ^ Detailed prompt token info
  }
  deriving (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    Usage
      <$> v .: "completion_tokens"
      <*> v .: "prompt_tokens"
      <*> v .: "total_tokens"
      <*> v .:? "completion_tokens_details"
      <*> v .:? "prompt_tokens_details"

-- | Represents a single choice in the chat completion response.
data Choice = Choice
  { finishReason :: Maybe FinishReason
  -- ^ Reason why the completion stopped
  , index :: Int
  -- ^ Index of the choice
  , logprobs :: Maybe LogProbs
  -- ^ Log probabilities, if requested
  , message :: Message
  -- ^ The generated message
  }
  deriving (Show, Eq, Generic)

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \v ->
    Choice
      <$> v .: "finish_reason"
      <*> v .: "index"
      <*> v .:? "logprobs"
      <*> v .: "message"

-- | Represents the reason why the completion stopped.
data FinishReason = Stop | Length | ContentFilter | ToolCalls | FunctionCall
  deriving (Show, Eq, Generic)

instance FromJSON FinishReason where
  parseJSON (String "stop") = return Stop
  parseJSON (String "length") = return Length
  parseJSON (String "content_filter") = return ContentFilter
  parseJSON (String "tool_calls") = return ToolCalls
  parseJSON (String "function_call") = return FunctionCall
  parseJSON invalid = fail $ "Invalid finish reason: " ++ show invalid

-- | Represents log probability information for the completion.
data LogProbs = LogProbs
  { contentForLogProbs :: Maybe [LogProbContent]
  -- ^ Log probs for content
  , refusal :: Maybe [LogProbContent]
  -- ^ Log probs for refusal
  }
  deriving (Show, Eq, Generic)

instance FromJSON LogProbs where
  parseJSON = withObject "LogProbs" $ \v ->
    LogProbs
      <$> v .:? "content"
      <*> v .:? "refusal"

-- | Represents log probability content for a token.
data LogProbContent = LogProbContent
  { bytes :: Maybe [Int]
  -- ^ Optional byte representation
  , logprob :: Double
  -- ^ Log probability of the token
  , token :: Text
  -- ^ The token
  , topLogprobs :: [TopLogProb]
  -- ^ Top log probabilities
  }
  deriving (Show, Eq, Generic)

instance FromJSON LogProbContent where
  parseJSON = withObject "LogProbContent" $ \v ->
    LogProbContent
      <$> v .:? "bytes"
      <*> v .: "logprob"
      <*> v .: "token"
      <*> v .: "top_logprobs"

-- | Represents a top log probability for a token.
data TopLogProb = TopLogProb
  { bytes :: Maybe [Int]
  -- ^ Optional byte representation
  , logprob :: Double
  -- ^ Log probability
  , token :: Text
  -- ^ The token
  }
  deriving (Show, Eq, Generic)

instance FromJSON TopLogProb where
  parseJSON = withObject "TopLogProb" $ \v ->
    TopLogProb
      <$> v .:? "bytes"
      <*> v .: "logprob"
      <*> v .: "token"

{- | Configuration for audio processing.
Specifies format and voice preferences for text-to-speech.
-}
data AudioConfig = AudioConfig
  { format :: Text
  -- ^ Audio format (e.g., "mp3")
  , voice :: Text
  -- ^ Voice to use (e.g., "en-US")
  }
  deriving (Show, Eq, Generic)

instance ToJSON AudioConfig where
  toJSON AudioConfig {..} =
    object
      [ "format" .= format
      , "voice" .= voice
      ]

instance FromJSON AudioConfig where
  parseJSON = withObject "AudioConfig" $ \v ->
    AudioConfig
      <$> v .: "format"
      <*> v .: "voice"

-- | Represents an audio response.
data AudioResponse = AudioResponse
  { data_ :: Text
  -- ^ Audio data
  , expiresAt :: Integer
  -- ^ Expiration time
  , id_ :: Text
  -- ^ Unique ID
  , transcript :: Text
  -- ^ Transcript of the audio
  }
  deriving (Show, Eq, Generic)

instance ToJSON AudioResponse where
  toJSON AudioResponse {..} =
    object
      [ "data" .= data_
      , "expires_at" .= expiresAt
      , "id" .= id_
      , "transcript" .= transcript
      ]

instance FromJSON AudioResponse where
  parseJSON = withObject "AudioResponse" $ \v ->
    AudioResponse
      <$> v .: "data"
      <*> v .: "expires_at"
      <*> v .: "id"
      <*> v .: "transcript"

-- | Represents different modalities for the conversation.
data Modality = TextModality | AudioModality
  deriving (Show, Eq, Generic)

instance ToJSON Modality where
  toJSON TextModality = String "text"
  toJSON AudioModality = String "audio"

instance FromJSON Modality where
  parseJSON (String "text") = return TextModality
  parseJSON (String "audio") = return AudioModality
  parseJSON invalid = fail $ "Invalid modality: " ++ show invalid

-- | Specifies how the model should choose tools.
data ToolChoice = None | Auto | Required | SpecificTool SpecificToolChoice
  deriving (Show, Eq, Generic)

instance ToJSON ToolChoice where
  toJSON None = String "none"
  toJSON Auto = String "auto"
  toJSON Required = String "required"
  toJSON (SpecificTool choice) = toJSON choice

instance FromJSON ToolChoice where
  parseJSON (String "none") = return None
  parseJSON (String "auto") = return Auto
  parseJSON (String "required") = return Required
  parseJSON o@(Object _) = SpecificTool <$> parseJSON o
  parseJSON invalid = fail $ "Invalid tool choice: " ++ show invalid

-- | Provides details for a specific tool choice.
data SpecificToolChoice = SpecificToolChoice
  { toolType :: Text
  -- ^ Type of the tool
  , function :: Value
  -- ^ Function details
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpecificToolChoice where
  toJSON SpecificToolChoice {..} =
    object
      [ "type" .= toolType
      , "function" .= function
      ]

instance FromJSON SpecificToolChoice where
  parseJSON = withObject "SpecificToolChoice" $ \v ->
    SpecificToolChoice
      <$> v .: "type"
      <*> v .: "function"

-- | Indicates the level of reasoning effort.
data ReasoningEffort = Low | Medium | High
  deriving (Show, Eq, Generic)

instance ToJSON ReasoningEffort where
  toJSON Low = String "low"
  toJSON Medium = String "medium"
  toJSON High = String "high"

instance FromJSON ReasoningEffort where
  parseJSON (String "low") = return Low
  parseJSON (String "medium") = return Medium
  parseJSON (String "high") = return High
  parseJSON invalid = fail $ "Invalid reasoning effort: " ++ show invalid

-- | Represents prediction content.
data PredictionContent = PredictionContent
  { contentForPredictionContent :: MessageContent
  -- ^ The content
  , contentType :: Text
  -- ^ Type of the content
  }
  deriving (Show, Eq, Generic)

instance ToJSON PredictionContent where
  toJSON PredictionContent {..} =
    object
      [ "content" .= contentForPredictionContent
      , "type" .= contentType
      ]

instance FromJSON PredictionContent where
  parseJSON = withObject "PredictionContent" $ \v ->
    PredictionContent
      <$> v .: "content"
      <*> v .: "type"

-- | Represents prediction output.
data PredictionOutput = PredictionOutput
  { predictionType :: Text
  -- ^ Type of the prediction
  , contentForPredictionOutput :: MessageContent
  -- ^ The output content
  }
  deriving (Show, Eq, Generic)

instance ToJSON PredictionOutput where
  toJSON PredictionOutput {..} =
    object
      [ "type" .= predictionType
      , "content" .= contentForPredictionOutput
      ]

instance FromJSON PredictionOutput where
  parseJSON = withObject "PredictionOutput" $ \v ->
    PredictionOutput
      <$> v .: "type"
      <*> v .: "content"

-- | Specifies the format of the response.
data ResponseFormat = JsonObjectFormat | JsonSchemaFormat Value
  deriving (Show, Eq, Generic)

instance ToJSON ResponseFormat where
  toJSON JsonObjectFormat = object ["type" .= ("json_object" :: Text)]
  toJSON (JsonSchemaFormat schema) =
    object
      [ "type" .= ("json_schema" :: Text)
      , "json_schema" .= schema
      ]

instance FromJSON ResponseFormat where
  parseJSON = withObject "ResponseFormat" $ \v -> do
    formatType <- v .: "type"
    case formatType of
      String "json_object" -> return JsonObjectFormat
      String "json_schema" -> JsonSchemaFormat <$> v .: "json_schema"
      _ -> fail $ "Invalid response format type: " ++ show formatType

-- | Options for streaming responses.
data StreamOptions = StreamOptions
  { includeUsage :: Bool
  -- ^ Whether to include usage information
  }
  deriving (Show, Eq, Generic)

instance ToJSON StreamOptions where
  toJSON StreamOptions {..} =
    object
      [ "include_usage" .= includeUsage
      ]

instance FromJSON StreamOptions where
  parseJSON = withObject "StreamOptions" $ \v ->
    StreamOptions <$> v .: "include_usage"

-- | Represents web search options.
data WebSearchOptions = WebSearchOptions
  { searchContextSize :: Maybe Text
  -- ^ Size of the search context
  , userLocation :: Maybe UserLocation
  -- ^ User location for context
  }
  deriving (Show, Eq, Generic)

instance ToJSON WebSearchOptions where
  toJSON WebSearchOptions {..} =
    object $
      maybe [] (\s -> ["search_context_size" .= s]) searchContextSize
        ++ maybe [] (\l -> ["user_location" .= l]) userLocation

instance FromJSON WebSearchOptions where
  parseJSON = withObject "WebSearchOptions" $ \v ->
    WebSearchOptions
      <$> v .:? "search_context_size"
      <*> v .:? "user_location"

-- | Represents user location.
data UserLocation = UserLocation
  { approximate :: ApproximateLocation
  -- ^ Approximate location details
  }
  deriving (Show, Eq, Generic)

instance ToJSON UserLocation where
  toJSON UserLocation {..} =
    object
      [ "approximate" .= approximate
      ]

instance FromJSON UserLocation where
  parseJSON = withObject "UserLocation" $ \v ->
    UserLocation <$> v .: "approximate"

-- | Represents approximate location.
data ApproximateLocation = ApproximateLocation
  { locationType :: Text
  -- ^ Type of the location
  }
  deriving (Show, Eq, Generic)

instance ToJSON ApproximateLocation where
  toJSON ApproximateLocation {..} =
    object
      [ "type" .= locationType
      ]

instance FromJSON ApproximateLocation where
  parseJSON = withObject "ApproximateLocation" $ \v ->
    ApproximateLocation <$> v .: "type"

-- | Details about completion tokens.
data CompletionTokensDetails = CompletionTokensDetails
  { acceptedPredictionTokens :: Int
  -- ^ Accepted prediction tokens
  , audioTokens :: Int
  -- ^ Audio tokens
  , reasoningTokens :: Int
  -- ^ Reasoning tokens
  , rejectedPredictionTokens :: Int
  -- ^ Rejected prediction tokens
  }
  deriving (Show, Eq, Generic)

instance FromJSON CompletionTokensDetails where
  parseJSON = withObject "CompletionTokensDetails" $ \v ->
    CompletionTokensDetails
      <$> v .: "accepted_prediction_tokens"
      <*> v .: "audio_tokens"
      <*> v .: "reasoning_tokens"
      <*> v .: "rejected_prediction_tokens"

-- | Details about prompt tokens.
data PromptTokensDetails = PromptTokensDetails
  { audioTokens :: Int
  -- ^ Audio tokens in the prompt
  , cachedTokens :: Int
  -- ^ Cached tokens
  }
  deriving (Show, Eq, Generic)

instance FromJSON PromptTokensDetails where
  parseJSON = withObject "PromptTokensDetails" $ \v ->
    PromptTokensDetails
      <$> v .: "audio_tokens"
      <*> v .: "cached_tokens"

-- | Parameters for customizing OpenAI API calls.
data OpenAIParams = OpenAIParams
  { frequencyPenalty :: Maybe Double
  , logitBias :: Maybe (Map Text Double)
  , logprobs :: Maybe Bool
  , maxCompletionTokens :: Maybe Int
  , maxTokens :: Maybe Int
  , metadata :: Maybe (Map Text Text)
  , modalities :: Maybe [Modality]
  , n :: Maybe Int
  , parallelToolCalls :: Maybe Bool
  , prediction :: Maybe PredictionOutput
  , presencePenalty :: Maybe Double
  , reasoningEffort :: Maybe ReasoningEffort
  , responseFormat :: Maybe ResponseFormat
  , seed :: Maybe Int
  , serviceTier :: Maybe Text
  , stop :: Maybe (Either Text [Text])
  , store :: Maybe Bool
  , temperature :: Maybe Double
  , toolChoice :: Maybe ToolChoice
  , tools :: Maybe [Tool_]
  , topLogprobs :: Maybe Int
  , topP :: Maybe Double
  , user :: Maybe Text
  , webSearchOptions :: Maybe WebSearchOptions
  , audio :: Maybe AudioConfig
  }

-- | Default chat completion request with "gpt-4.1-nano" model.
defaultChatCompletionRequest :: ChatCompletionRequest
defaultChatCompletionRequest =
  ChatCompletionRequest
    { messages = []
    , model = "gpt-4.1-nano"
    , frequencyPenalty = Nothing
    , logitBias = Nothing
    , logprobs = Nothing
    , maxCompletionTokens = Nothing
    , maxTokens = Nothing
    , metadata = Nothing
    , modalities = Nothing
    , n = Nothing
    , parallelToolCalls = Nothing
    , prediction = Nothing
    , presencePenalty = Nothing
    , reasoningEffort = Nothing
    , responseFormat = Nothing
    , seed = Nothing
    , serviceTier = Nothing
    , stop = Nothing
    , store = Nothing
    , stream = Nothing
    , streamOptions = Nothing
    , temperature = Nothing
    , toolChoice = Nothing
    , tools = Nothing
    , topLogprobs = Nothing
    , topP = Nothing
    , user = Nothing
    , webSearchOptions = Nothing
    , audio = Nothing
    }

{- | Creates a chat completion request and returns the response.
Sends the request to OpenAI's API and parses the response.
-}
createChatCompletion :: Text -> ChatCompletionRequest -> IO (Either String ChatCompletionResponse)
createChatCompletion apiKey r = do
  request_ <- parseRequest "https://api.openai.com/v1/chat/completions"
  let req =
        setRequestMethod "POST" $
          setRequestSecure True $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
                setRequestBodyJSON r $
                  request_

  response <- httpLBS req
  let status = statusCode $ getResponseStatus response
  if status >= 200 && status < 300
    then case eitherDecode (getResponseBody response) of
      Left err -> return $ Left $ "JSON parse error: " <> err
      Right completionResponse -> return $ Right completionResponse
    else return $ Left $ "API error: " <> show status <> " " <> show (getResponseBody response)

{- | Creates a streaming chat completion request.
Processes the stream using the provided handler.
-}
createChatCompletionStream ::
  Text -> ChatCompletionRequest -> OpenAIStreamHandler -> IO (Either String ())
createChatCompletionStream apiKey r OpenAIStreamHandler {..} = do
  request_ <- parseRequest "POST https://api.openai.com/v1/chat/completions"
  let httpReq =
        setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
          setRequestHeader "Content-Type" ["application/json"] $
            setRequestBodyJSON r $
              request_

  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http httpReq manager
    bufferRef <- liftIO $ newIORef BS.empty
    runConduit $
      responseBody response
        .| linesUnboundedAsciiC
        .| mapM_C (liftIO . processLine bufferRef)

  onComplete
  return $ Right ()
  where
    processLine bufferRef line = do
      if BS.isPrefixOf "data: " line
        then do
          if line == "data: [DONE]"
            then return () -- Stream is complete
            else do
              let content = BS.drop 6 line -- Remove "data: " prefix
              case decode (LBS.fromStrict content) of
                Just chunk -> onToken chunk
                Nothing -> do
                  -- Handle potential partial JSON by buffering
                  oldBuffer <- readIORef bufferRef
                  let newBuffer = oldBuffer <> content
                  writeIORef bufferRef newBuffer
                  -- Try to parse the combined buffer
                  case decode (LBS.fromStrict newBuffer) of
                    Just chunk -> do
                      onToken chunk
                      writeIORef bufferRef BS.empty -- Clear buffer after successful parse
                    Nothing -> return () -- Keep in buffer for next chunk
        else return () -- Ignore non-data lines

-- | Default parameters for OpenAI API calls.
defaultOpenAIParams :: OpenAIParams
defaultOpenAIParams =
  OpenAIParams
    { frequencyPenalty = Nothing
    , logitBias = Nothing
    , logprobs = Nothing
    , maxCompletionTokens = Nothing
    , maxTokens = Nothing
    , metadata = Nothing
    , modalities = Nothing
    , n = Nothing
    , parallelToolCalls = Nothing
    , prediction = Nothing
    , presencePenalty = Nothing
    , reasoningEffort = Nothing
    , responseFormat = Nothing
    , seed = Nothing
    , serviceTier = Nothing
    , stop = Nothing
    , store = Nothing
    , temperature = Nothing
    , toolChoice = Nothing
    , tools = Nothing
    , topLogprobs = Nothing
    , topP = Nothing
    , user = Nothing
    , webSearchOptions = Nothing
    , audio = Nothing
    }

-- | Default prediction output.
defaultPredictionOutput :: PredictionOutput
defaultPredictionOutput =
  PredictionOutput
    { predictionType = "text"
    , contentForPredictionOutput = StringContent ""
    }

-- | Default response format (JSON object).
defaultResponseFormat :: ResponseFormat
defaultResponseFormat = JsonObjectFormat

-- | Default stream options.
defaultStreamOptions :: StreamOptions
defaultStreamOptions =
  StreamOptions
    { includeUsage = False
    }

-- | Default web search options.
defaultWebSearchOptions :: WebSearchOptions
defaultWebSearchOptions =
  WebSearchOptions
    { searchContextSize = Nothing
    , userLocation = Nothing
    }

-- | Default user location.
defaultUserLocation :: UserLocation
defaultUserLocation =
  UserLocation
    { approximate = ApproximateLocation {locationType = "approximate"}
    }

-- | Default audio configuration.
defaultAudioConfig :: AudioConfig
defaultAudioConfig =
  AudioConfig
    { format = "mp3"
    , voice = "en-US"
    }

-- | Default tool choice (None).
defaultToolChoice :: ToolChoice
defaultToolChoice = None

-- | Default specific tool choice.
defaultSpecificToolChoice :: SpecificToolChoice
defaultSpecificToolChoice =
  SpecificToolChoice
    { toolType = "text"
    , function = Null
    }

-- | Default reasoning effort (Low).
defaultReasoningEffort :: ReasoningEffort
defaultReasoningEffort = Low

-- | Default function.
defaultFunction :: Function_
defaultFunction =
  Function_
    { name = "default_function"
    , description = Nothing
    , parameters = Nothing
    , strict = Nothing
    }

{-# LANGUAGE DeriveGeneric #-}
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

Example Usage

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
  ( -- * Types
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
  , InputTool (..)
  , FunctionDef (..)
  , FunctionParameters (..)
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

    -- * Default values for types
  , defaultChatCompletionRequest
  , createChatCompletion
  , createChatCompletionStream
  , defaultMessage
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
import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Langchain.Error (LangchainError, llmError)
import Langchain.LLM.Core (ToolCall (..))
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.Internal.SchemaBuilder
import Network.HTTP.Conduit
import Network.HTTP.Simple
  ( getResponseBody
  , getResponseStatus
  , setRequestBodyJSON
  , setRequestHeader
  , setRequestMethod
  , setRequestSecure
  )
import Network.HTTP.Types.Status (statusCode)

{- | Represents a chunk of the chat completion response in a streaming context.
Contains a list of possible choices for the completion.
-}
newtype ChatCompletionChunk = ChatCompletionChunk
  { chunkChoices :: [ChunkChoice]
  {- ^ List of choices in this chunk of the response
  TODO: add Usage
  -}
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
newtype Delta = Delta
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
  { baseUrl :: Maybe String
  , messages :: [Message]
  -- ^ List of messages in the conversation history
  , model :: Text
  -- ^ The model to use for completion (e.g., "gpt-3.5-turbo")
  , timeout :: Maybe Int
  -- ^ Override default response timeout in seconds. Default = 60 seconds
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
  , tools :: Maybe [InputTool]
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
  , responseServiceTier :: Maybe Text
  -- ^ Service tier used
  , systemFingerprint :: Maybe Text
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
      <*> v .:? "system_fingerprint"
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
  , toolCalls :: Maybe [ToolCall]
  -- ^ Optional tool call information
  , messageToolCallId :: Maybe Text
  -- ^ Optional tool call ID
  , messageAudio :: Maybe AudioResponse
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
    , toolCalls = Nothing
    , messageToolCallId = Nothing
    , messageAudio = Nothing
    , refusal = Nothing
    }

instance ToJSON Message where
  toJSON Message {..} =
    object $
      ["role" .= role]
        ++ maybe [] (\c -> ["content" .= c]) content
        ++ maybe [] (\n -> ["name" .= n]) name
        ++ maybe [] (\tc -> ["tool_calls" .= tc]) toolCalls
        ++ maybe [] (\tcid -> ["tool_call_id" .= tcid]) messageToolCallId
        ++ maybe [] (\a -> ["audio" .= a]) messageAudio
        ++ maybe [] (\r -> ["refusal" .= r]) refusal

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "role"
      <*> v .:? "content"
      <*> v .:? "name"
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

data InputTool = InputTool
  { toolType :: Text
  -- ^ The type of the tool
  , function :: FunctionDef
  -- ^ The function associated with the tool
  }
  deriving (Show, Eq, Generic)

instance ToJSON InputTool where
  toJSON InputTool {..} =
    object
      [ "type" .= toolType
      , "function" .= function
      ]

instance FromJSON InputTool where
  parseJSON = withObject "Tool" $ \v ->
    InputTool
      <$> v .: "type"
      <*> v .: "function"

-- | Represents a function that can be called by the model.
data FunctionDef = FunctionDef
  { functionName :: Text
  -- ^ The name of the function
  , functionDescription :: Maybe Text
  -- ^ Optional description of the function
  , functionParameters :: Maybe FunctionParameters
  -- ^ Optional parameters for the function
  , functionStrict :: Maybe Bool
  -- ^ Optional strictness flag
  }
  deriving (Show, Eq, Generic)

instance ToJSON FunctionDef where
  toJSON FunctionDef {..} =
    object $
      [ "name" .= functionName
      ]
        ++ maybe [] (\d -> ["description" .= d]) functionDescription
        ++ maybe [] (\p -> ["parameters" .= p]) functionParameters
        ++ maybe [] (\s -> ["strict" .= s]) functionStrict

instance FromJSON FunctionDef where
  parseJSON = withObject "Function" $ \v ->
    FunctionDef
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .:? "parameters"
      <*> v .:? "strict"

-- | Parameters definition for a function call used in structured output or tool calls.
data FunctionParameters = FunctionParameters
  { parameterType :: Text
  -- ^ Type of the parameter (usually "object").
  , parameterProperties :: Maybe (HM.Map Text FunctionParameters)
  -- ^ Optional nested parameters as a property map.
  , requiredParams :: Maybe [Text]
  -- ^ List of required parameter names.
  , additionalProperties :: Maybe Bool
  -- ^ Whether additional (unspecified) parameters are allowed.
  }
  deriving (Show, Eq)

instance ToJSON FunctionParameters where
  toJSON FunctionParameters {..} =
    object
      [ "type" .= parameterType
      , "properties" .= parameterProperties
      , "required" .= requiredParams
      , "additionalProperties" .= additionalProperties
      ]

instance FromJSON FunctionParameters where
  parseJSON = withObject "parameters" $ \v ->
    FunctionParameters
      <$> v .: "type"
      <*> v .: "properties"
      <*> v .: "required"
      <*> v .: "additionalProperties"

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
  { choiceFinishReason :: Maybe FinishReason
  -- ^ Reason why the completion stopped
  , index :: Int
  -- ^ Index of the choice
  , choiceLogprobs :: Maybe LogProbs
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
  , logProbsRefusal :: Maybe [LogProbContent]
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
  , logProbContentTopLogprobs :: [TopLogProb]
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
  { topLogProbBytes :: Maybe [Int]
  -- ^ Optional byte representation
  , topLogProbLogprob :: Double
  -- ^ Log probability
  , topLogProbToken :: Text
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
  { audioResponseData :: Text
  -- ^ Audio data
  , expiresAt :: Integer
  -- ^ Expiration time
  , audioResponseId :: Text
  -- ^ Unique ID
  , transcript :: Text
  -- ^ Transcript of the audio
  }
  deriving (Show, Eq, Generic)

instance ToJSON AudioResponse where
  toJSON AudioResponse {..} =
    object
      [ "data" .= audioResponseData
      , "expires_at" .= expiresAt
      , "id" .= audioResponseId
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
  { specificToolChoiceToolType :: Text
  -- ^ Type of the tool
  , specificToolChoiceFunction :: Value
  -- ^ Function details
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpecificToolChoice where
  toJSON SpecificToolChoice {..} =
    object
      [ "type" .= specificToolChoiceToolType
      , "function" .= specificToolChoiceFunction
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
  , predictionContentType :: Text
  -- ^ Type of the content
  }
  deriving (Show, Eq, Generic)

instance ToJSON PredictionContent where
  toJSON PredictionContent {..} =
    object
      [ "content" .= contentForPredictionContent
      , "type" .= predictionContentType
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
data ResponseFormat = JsonObjectFormat | JsonSchemaFormat Text Schema Bool
  deriving (Show, Eq, Generic)

instance ToJSON ResponseFormat where
  toJSON JsonObjectFormat = object ["type" .= ("json_object" :: Text)]
  toJSON (JsonSchemaFormat name schema strict) =
    object
      [ "type" .= ("json_schema" :: Text)
      , "json_schema"
          .= object
            [ "name" .= name
            , "schema" .= schema
            , "strict" .= strict
            ]
      ]

-- | Options for streaming responses.
newtype StreamOptions = StreamOptions
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
newtype UserLocation = UserLocation
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
newtype ApproximateLocation = ApproximateLocation
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
  { promptTokenDetailsAudioTokens :: Int
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

-- | Default chat completion request with "gpt-4.1-nano" model.
defaultChatCompletionRequest :: ChatCompletionRequest
defaultChatCompletionRequest =
  ChatCompletionRequest
    { baseUrl = Just "https://api.openai.com/v1"
    , messages = []
    , model = "gpt-4.1-nano"
    , timeout = Just 60
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
createChatCompletion ::
  Text -> ChatCompletionRequest -> IO (Either LangchainError ChatCompletionResponse)
createChatCompletion apiKey r = do
  request_ <-
    parseRequest $
      fromMaybe "https://api.openai.com/v1" (baseUrl r)
        <> "/chat/completions"
  manager <-
    newManager
      tlsManagerSettings
        { managerResponseTimeout =
            responseTimeoutMicro (fromMaybe 60 (timeout r) * 1000000)
        }
  let req =
        setRequestMethod "POST" $
          setRequestSecure True $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
                setRequestBodyJSON r request_

  response <- httpLbs req manager
  let status = statusCode $ getResponseStatus response
  if status >= 200 && status < 300
    then case eitherDecode (getResponseBody response) of
      Left err ->
        return $
          Left $
            llmError
              ( T.pack $
                  "JSON parse error: "
                    <> err
                    <> show (getResponseBody response)
              )
              Nothing
              Nothing
      Right completionResponse -> return $ Right completionResponse
    else
      return $
        Left $
          llmError
            ( T.pack $
                "API error: "
                  <> show status
                  <> " "
                  <> show (getResponseBody response)
            )
            Nothing
            Nothing

{- | Creates a streaming chat completion request.
Processes the stream using the provided handler.
-}
createChatCompletionStream ::
  Text -> ChatCompletionRequest -> OpenAIStreamHandler -> IO (Either LangchainError ())
createChatCompletionStream apiKey r OpenAIStreamHandler {..} = do
  request_ <-
    parseRequest $
      "POST "
        <> fromMaybe "https://api.openai.com/v1" (baseUrl r)
        <> "/chat/completions"
  let httpReq =
        setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
          setRequestHeader "Content-Type" ["application/json"] $
            setRequestBodyJSON r request_

  manager <-
    newManager
      tlsManagerSettings
        { managerResponseTimeout =
            responseTimeoutMicro (fromMaybe 60 (timeout r) * 1000000)
        }
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
      when
        (BS.isPrefixOf "data: " line)
        ( do
            unless
              (line == "data: [DONE]")
              ( do
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
                          writeIORef bufferRef BS.empty
                        -- Clear buffer after successful parse
                        Nothing -> return () -- Keep in buffer for next chunk
              )
        )

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
    { specificToolChoiceToolType = "text"
    , specificToolChoiceFunction = Null
    }

-- | Default reasoning effort (Low).
defaultReasoningEffort :: ReasoningEffort
defaultReasoningEffort = Low

-- | Default function.
defaultFunction :: FunctionDef
defaultFunction =
  FunctionDef
    { functionName = "default_function"
    , functionDescription = Nothing
    , functionParameters = Nothing
    , functionStrict = Nothing
    }

instance LLM.MessageConvertible Message where
  to msg =
    defaultMessage
      { role = toRole $ LLM.role msg
      , content = Just $ StringContent (LLM.content msg)
      , name = LLM.name (LLM.messageData msg)
      , toolCalls = LLM.toolCalls (LLM.messageData msg)
      }
    where
      toRole :: LLM.Role -> Role
      toRole r = case r of
        LLM.System -> System
        LLM.User -> User
        LLM.Assistant -> Assistant
        LLM.Tool -> Tool
        LLM.Developer -> Developer
        LLM.Function -> Function

  from msg =
    LLM.Message
      { LLM.role = fromRole $ role msg
      , LLM.content = case content msg of
          Just (StringContent txt) -> txt
          _ -> "" -- Handle other cases like `Nothing` or other content types as needed
      , LLM.messageData =
          LLM.defaultMessageData
            { LLM.name = name msg
            , LLM.toolCalls = toolCalls msg
            }
      }
    where
      fromRole :: Role -> LLM.Role
      fromRole r = case r of
        System -> LLM.System
        User -> LLM.User
        Assistant -> LLM.Assistant
        Tool -> LLM.Tool
        Developer -> LLM.Developer
        Function -> LLM.Function

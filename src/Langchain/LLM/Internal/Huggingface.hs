{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module:      Langchain.LLM.Internal.Huggingface
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

Internal types for interfacing with Huggingface.
https://huggingface.co/docs/inference-providers/providers/cerebras
-}
module Langchain.LLM.Internal.Huggingface
  ( -- * Types
    StreamOptions (..)
  , HuggingfaceChatCompletionRequest (..)
  , Message (..)
  , MessageContent (..)
  , ImageUrl (..)
  , Role (..)
  , ContentObject (..)
  , Tool_ (..)
  , Function_ (..)
  , ToolChoice (..)
  , SpecificToolChoice (..)
  , ResponseFormat (..)
  , ChatCompletionResponse (..)
  , ChatCompletionChunk (..)
  , Choice (..)
  , ChoiceChunk (..)
  , Usage (..)
  , TimeInfo (..)
  , ChunkUsage (..)
  , ChunkTimeInfo (..)
  , Delta (..)
  , Provider (..)
  , HuggingfaceStreamHandler (..)

    -- * Functions
  , providerLinks
  , getProviderLink
  , createChatCompletion
  , defaultHuggingfaceChatCompletionRequest
  , defaultHugginfaceMessage
  , createChatCompletionStream
  , defaultHuggingfaceStreamHandler
  ) where

import Conduit
import Control.Monad (when)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import qualified Langchain.LLM.Core as LLM
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

-- | Specifies the format of the response.
data ResponseFormat = RegexFormat String | JsonSchemaFormat Value
  deriving (Show, Eq, Generic)

instance ToJSON ResponseFormat where
  toJSON (RegexFormat regEx) = object ["type" .= ("regex" :: Text), "value" .= regEx]
  toJSON (JsonSchemaFormat schema) =
    object
      [ "type" .= ("json" :: Text)
      , "value" .= schema
      ]

instance FromJSON ResponseFormat where
  parseJSON = withObject "ResponseFormat" $ \v -> do
    formatType <- v .: "type"
    case formatType of
      String "regex" -> RegexFormat <$> v .: "value"
      String "json" -> JsonSchemaFormat <$> v .: "value"
      _ -> fail $ "Invalid response format type: " ++ show formatType

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
  { functionName :: Text
  -- ^ The name of the function
  , description :: Maybe Text
  -- ^ Optional description of the function
  , arguments :: Maybe Value
  -- ^ Optional parameters for the function
  }
  deriving (Show, Eq, Generic)

instance ToJSON Function_ where
  toJSON Function_ {..} =
    object $
      [ "name" .= functionName
      ]
        ++ maybe [] (\d -> ["description" .= d]) description
        ++ maybe [] (\p -> ["arguments" .= p]) arguments

instance FromJSON Function_ where
  parseJSON = withObject "Function" $ \v ->
    Function_
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .:? "arguments"

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
newtype SpecificToolChoice = SpecificToolChoice
  { specificToolChoiceFunction :: Value
  -- ^ Function details
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpecificToolChoice where
  toJSON SpecificToolChoice {..} =
    object
      [ "function" .= specificToolChoiceFunction
      ]

instance FromJSON SpecificToolChoice where
  parseJSON = withObject "SpecificToolChoice" $ \v ->
    SpecificToolChoice
      <$> v .: "function"

-- | Options for streaming responses.
newtype StreamOptions = StreamOptions
  { includeUsage :: Bool
  -- ^ Whether to include usage information
  }
  deriving (Show, Eq)

instance ToJSON StreamOptions where
  toJSON StreamOptions {..} =
    object
      [ "include_usage" .= includeUsage
      ]

instance FromJSON StreamOptions where
  parseJSON = withObject "StreamOptions" $ \v ->
    StreamOptions <$> v .: "include_usage"

-- | Huggingface supporting Roles
data Role = User | Assistant | Tool | System
  deriving (Eq, Show, Generic)

instance ToJSON Role where
  toJSON User = String "user"
  toJSON Assistant = String "assistant"
  toJSON Tool = String "tool"
  toJSON System = String "system"

instance FromJSON Role where
  parseJSON = withText "Role" $ \t -> case t of
    "user" -> pure User
    "assistant" -> pure Assistant
    "tool" -> pure Tool
    "system" -> pure System
    _ -> fail $ "Unknown role: " ++ T.unpack t

-- | Image url object
newtype ImageUrl = ImageUrl
  { url :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON ImageUrl where
  toJSON (ImageUrl url) = object ["url" .= url]

instance FromJSON ImageUrl where
  parseJSON = withObject "ImageUrl" $ \v ->
    ImageUrl <$> v .: "url"

-- | ContentObject
data ContentObject = ContentObject
  { contentType :: Text
  , contentText :: Maybe Text
  , imageUrl :: Maybe ImageUrl
  }
  deriving (Eq, Show, Generic)

instance ToJSON ContentObject where
  toJSON (ContentObject contentType contentText imageUrl) =
    object $
      ["type" .= contentType]
        ++ maybe [] (\t -> ["text" .= t]) contentText
        ++ maybe [] (\i -> ["image_url" .= i]) imageUrl

instance FromJSON ContentObject where
  parseJSON = withObject "ContentObject" $ \v ->
    ContentObject
      <$> v .: "type"
      <*> v .:? "text"
      <*> v .:? "image_url"

-- | Message could be either simple text or an object
data MessageContent = MessageContent [ContentObject] | TextContent Text
  deriving (Eq, Show)

instance ToJSON MessageContent where
  toJSON (MessageContent contentObjects) = toJSON contentObjects
  toJSON (TextContent text) = String text

instance FromJSON MessageContent where
  parseJSON v@(String _) = TextContent <$> parseJSON v
  parseJSON v = MessageContent <$> parseJSON v

-- | Huggingface's Message type
data Message = Message
  { role :: Role
  , content :: MessageContent
  , name :: Maybe String
  }
  deriving (Eq, Show, Generic)

-- | Default message type
defaultHugginfaceMessage :: Message
defaultHugginfaceMessage =
  Message
    { role = User
    , content = TextContent "What is the meaining of life?"
    , name = Nothing
    }

instance ToJSON Message where
  toJSON (Message role content name) =
    object $
      ["role" .= role, "content" .= content]
        ++ maybe [] (\n -> ["name" .= n]) name

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "role"
      <*> v .: "content"
      <*> v .:? "name"

{- | \$providers
Supported providers and their API endpoints:

- Cerebras: @https://router.huggingface.co/cerebras/...
- Cohere: @https://router.huggingface.co/cohere/...
- Fireworks: @https://router.huggingface.co/fireworks-ai/...
- HFInference: @https://router.huggingface.co/hf-inference/...
-}
getProviderLink :: Provider -> Maybe String
getProviderLink provider = Map.lookup provider providerLinks

-- | Map of Providers to their respective links
providerLinks :: Map.Map Provider String
providerLinks =
  Map.fromList
    [ (Cerebras, "https://router.huggingface.co/cerebras/v1/chat/completions")
    , (Cohere, "https://router.huggingface.co/cohere/compatibility/v1/chat/completions")
    , (FalAI, "https://router.huggingface.co/fal-ai/fal-ai/whisper")
    , (Fireworks, "https://router.huggingface.co/fireworks-ai/inference/v1/chat/completions")
    , (Hyperbolic, "https://router.huggingface.co/hyperbolic/v1/chat/completions")
    , (HFInference, "https://router.huggingface.co/hf-inference/models/Qwen/QwQ-32B/v1/chat/completions")
    , (Nebius, "https://router.huggingface.co/nebius/v1/chat/completions")
    , (Novita, "https://router.huggingface.co/novita/v3/openai/chat/completions")
    , (SambaNova, "https://router.huggingface.co/sambanova/v1/chat/completions")
    , (Together, "https://router.huggingface.co/together/v1/chat/completions")
    ]

{- |
    Providers integrated with Huggingface Inference
    https://huggingface.co/docs/inference-providers/index#partners
-}
data Provider
  = Cerebras
  | Cohere
  | FalAI
  | Fireworks
  | HFInference
  | Hyperbolic
  | Nebius
  | Novita
  | Replicate
  | SambaNova
  | Together
  deriving (Show, Eq, Ord)

-- | Chat completion request body type. Separatly passes provider.
data HuggingfaceChatCompletionRequest = HuggingfaceChatCompletionRequest
  { provider :: Provider
  , timeout :: Maybe Int
  , messages :: [Message]
  , model :: Text
  , stream :: Bool
  , maxTokens :: Maybe Integer
  , frequencyPenalty :: Maybe Double
  , logProbs :: Maybe Bool
  , presencePenalty :: Maybe Double
  , seed :: Maybe Int
  , stop :: Maybe [String]
  , temperature :: Maybe Double
  , toolPrompt :: Maybe String
  , topLogprobs :: Maybe Int
  , topP :: Maybe Double
  , streamOptions :: Maybe StreamOptions
  , responseFormat :: Maybe ResponseFormat
  , tools :: Maybe [Tool_]
  , toolChoice :: Maybe ToolChoice
  }
  deriving (Eq, Show, Generic)

-- | Default values of chat completion request.
defaultHuggingfaceChatCompletionRequest :: HuggingfaceChatCompletionRequest
defaultHuggingfaceChatCompletionRequest =
  HuggingfaceChatCompletionRequest
    { provider = Cerebras
    , timeout = Nothing
    , messages = [defaultHugginfaceMessage]
    , model = "llama-3.3-70b"
    , stream = False
    , maxTokens = Nothing
    , frequencyPenalty = Nothing
    , logProbs = Nothing
    , presencePenalty = Nothing
    , seed = Nothing
    , stop = Nothing
    , temperature = Nothing
    , toolPrompt = Nothing
    , topLogprobs = Nothing
    , topP = Nothing
    , streamOptions = Nothing
    , responseFormat = Nothing
    , tools = Nothing
    , toolChoice = Nothing
    }

instance ToJSON HuggingfaceChatCompletionRequest where
  toJSON
    ( HuggingfaceChatCompletionRequest
        _
        _
        messages
        model
        stream
        maxTokens
        frequencyPenalty
        logProbs
        presencePenalty
        seed
        stop
        temperature
        toolPrompt
        topLogprobs
        topP
        streamOptions
        responseFormat
        tools
        toolChoice
      ) =
      object $
        [ "messages" .= messages
        , "model" .= model
        , "stream" .= stream
        ]
          ++ optionalField "max_tokens" maxTokens
          ++ optionalField "frequency_penalty" frequencyPenalty
          ++ optionalField "logprobs" logProbs
          ++ optionalField "presence_penalty" presencePenalty
          ++ optionalField "seed" seed
          ++ optionalField "stop" stop
          ++ optionalField "temperature" temperature
          ++ optionalField "tool_prompt" toolPrompt
          ++ optionalField "top_logprobs" topLogprobs
          ++ optionalField "top_p" topP
          ++ optionalField "stream_options" streamOptions
          ++ optionalField "response_format" responseFormat
          ++ optionalField "tools" tools
          ++ optionalField "tool_choice" toolChoice
      where
        optionalField _ Nothing = []
        optionalField key (Just value) = [(key, toJSON value)]

-- | Choice options
data Choice = Choice
  { finish_reason :: Text
  , index :: Int
  , message :: Message
  }
  deriving (Eq, Show, Generic)

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \v ->
    Choice
      <$> v .: "finish_reason"
      <*> v .: "index"
      <*> v .: "message"

-- | Token usage
data Usage = Usage
  { prompt_tokens :: Int
  , completion_tokens :: Int
  , total_tokens :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    Usage
      <$> v .: "prompt_tokens"
      <*> v .: "completion_tokens"
      <*> v .: "total_tokens"

-- | Timeinfo
data TimeInfo = TimeInfo
  { queue_time :: Double
  , prompt_time :: Double
  , completion_time :: Double
  , total_time :: Double
  , timeInfoCreated :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON TimeInfo where
  parseJSON = withObject "TimeInfo" $ \v ->
    TimeInfo
      <$> v .: "queue_time"
      <*> v .: "prompt_time"
      <*> v .: "completion_time"
      <*> v .: "total_time"
      <*> v .: "created"

-- | Response type for chat completion
data ChatCompletionResponse = ChatCompletionResponse
  { responseId :: Text
  , choices :: [Choice]
  , created :: Int
  , chatCompletionModel :: Text
  , system_fingerprint :: Text
  , chatCompletionObject :: Text
  , usage :: Usage
  , time_info :: TimeInfo
  }
  deriving (Eq, Show, Generic)

instance FromJSON ChatCompletionResponse where
  parseJSON = withObject "ChatCompletion" $ \v ->
    ChatCompletionResponse
      <$> v .: "id"
      <*> v .: "choices"
      <*> v .: "created"
      <*> v .: "model"
      <*> v .: "system_fingerprint"
      <*> v .: "object"
      <*> v .: "usage"
      <*> v .: "time_info"

-- | Response for stream
newtype Delta = Delta
  { deltaContent :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Delta where
  parseJSON = withObject "Delta" $ \v ->
    Delta
      <$> v .:? "content"

-- | Represents type for choice object from stream response
data ChoiceChunk = ChoiceChunk
  { delta :: Delta
  , choiceFinishReason :: Maybe Text
  , choiceIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON ChoiceChunk where
  parseJSON = withObject "ChoiceChunk" $ \v ->
    ChoiceChunk
      <$> v .: "delta"
      <*> v .:? "finish_reason"
      <*> v .: "index"

-- | Represent type for usage object from stream response
data ChunkUsage = ChunkUsage
  { promptTokens :: Int
  , usageCompletionTokens :: Int
  , usageTotalTokens :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON ChunkUsage where
  parseJSON = withObject "Usage" $ \v ->
    ChunkUsage
      <$> v .: "prompt_tokens"
      <*> v .: "completion_tokens"
      <*> v .: "total_tokens"

-- | Represents type for timeinfo object from stream reponse
data ChunkTimeInfo = ChunkTimeInfo
  { timeInfoQueueTime :: Double
  , timeInfoPromptTime :: Double
  , timeInfoCompletionTime :: Double
  , timeInfoTotalTime :: Double
  , chunkTimeInfoCreated :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON ChunkTimeInfo where
  parseJSON = withObject "TimeInfo" $ \v ->
    ChunkTimeInfo
      <$> v .: "queue_time"
      <*> v .: "prompt_time"
      <*> v .: "completion_time"
      <*> v .: "total_time"
      <*> v .: "created"

-- | Type that represents stream response
data ChatCompletionChunk = ChatCompletionChunk
  { chatCompletionChunkId :: Text
  , chunkChoices :: [ChoiceChunk]
  , chunkCreated :: Int
  , chunkModel :: Text
  , chunkSystemFingerprint :: Text
  , chunkObject :: Text
  , chunkUsage :: Maybe Usage
  , chunkTimeInfo :: Maybe ChunkTimeInfo
  }
  deriving (Eq, Show, Generic)

instance FromJSON ChatCompletionChunk where
  parseJSON = withObject "ChatCompletionChunk" $ \v ->
    ChatCompletionChunk
      <$> v .: "id"
      <*> v .: "choices"
      <*> v .: "created"
      <*> v .: "model"
      <*> v .: "system_fingerprint"
      <*> v .: "object"
      <*> v .:? "usage"
      <*> v .:? "time_info"

-- | Chat completion function
createChatCompletion ::
  Text -> HuggingfaceChatCompletionRequest -> IO (Either String ChatCompletionResponse)
createChatCompletion apiKey r = do
  case getProviderLink (provider r) of
    Nothing -> pure $ Left "Incompatible provider"
    Just link -> do
      request_ <- parseRequest link
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
          Left err -> return $ Left $ "JSON parse error: " <> err
          Right completionResponse -> return $ Right completionResponse
        else
          return $
            Left $
              "API error: "
                <> show status
                <> " "
                <> show (getResponseBody response)

{- | Handler for streaming chat completion responses.
Provides callbacks for processing each token and handling stream completion.
-}
data HuggingfaceStreamHandler = HuggingfaceStreamHandler
  { onToken :: ChatCompletionChunk -> IO ()
  -- ^ Callback for each token (chunk) received
  , onComplete :: IO ()
  -- ^ Callback when the stream is complete
  }

-- | Default values for stream handling in Huggingface LLM
defaultHuggingfaceStreamHandler :: HuggingfaceStreamHandler
defaultHuggingfaceStreamHandler =
  HuggingfaceStreamHandler
    { onToken = print
    , onComplete = pure ()
    }

-- | Streaming function for huggingface
createChatCompletionStream ::
  Text ->
  HuggingfaceChatCompletionRequest ->
  HuggingfaceStreamHandler ->
  IO (Either String ())
createChatCompletionStream apiKey r HuggingfaceStreamHandler {..} = do
  case getProviderLink (provider r) of
    Nothing -> pure $ Left "Incompatible provider"
    Just link -> do
      request_ <- parseRequest link
      let httpReq =
            setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
              setRequestMethod "POST" $
                setRequestSecure True $
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
                do
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
            )

instance LLM.MessageConvertible Message where
  -- to :: LLM.Message -> Message
  to msg =
    defaultHugginfaceMessage
      { role = toRole $ LLM.role msg
      , content = TextContent (LLM.content msg)
      }
    where
      toRole :: LLM.Role -> Role
      toRole r = case r of
        LLM.System -> System
        LLM.User -> User
        LLM.Assistant -> Assistant
        LLM.Tool -> Tool
        _ -> User

  -- from :: Message -> LLM.Message
  from msg =
    LLM.Message
      { LLM.role = toRole (role msg)
      , LLM.content = case content msg of
          TextContent txt -> txt
          _ -> ""
      , LLM.messageData = LLM.defaultMessageData
      }
    where
      toRole :: Role -> LLM.Role
      toRole r = case r of
        System -> LLM.System
        User -> LLM.User
        Assistant -> LLM.Assistant
        Tool -> LLM.Tool

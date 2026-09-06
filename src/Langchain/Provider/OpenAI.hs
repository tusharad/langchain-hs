{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Langchain.Provider.OpenAI
Description : OpenAI provider implementing effect-polymorphic ChatModel
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

OpenAI and OpenAICompatible provider using the @openai@ Haskell package
for typed API calls. Multi-modal content and streaming support.
-}
module Langchain.Provider.OpenAI
  ( OpenAI (..)
  , OpenAIConfig (..)
  , defaultConfig
  , defaultOpenAIConfig
  , OpenAIToolChoice (..)
  , openAITools
  , newOpenAI
  , openAICompatible
  , normalizeBaseUrl
  , parseOpenAIResponse
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (AsyncCancelled (..), async, cancel)
import Control.Concurrent.STM
  ( atomically
  , newEmptyTMVarIO
  , newTBQueueIO
  , orElse
  , putTMVar
  , readTBQueue
  , readTMVar
  , writeTBQueue
  )
import Control.Exception (SomeException, finally, fromException, throwIO, try)
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Conduit.Combinators as C

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Generics (Generic)

import qualified OpenAI.V1 as OAI
import qualified OpenAI.V1.Chat.Completions as CC
import qualified OpenAI.V1.Models as OM
import qualified OpenAI.V1.ToolCall as OTC
import qualified OpenAI.V1.Usage as OU

import Langchain.Core.Error (LangchainError, llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), StreamM, TokenUsage (..))
import Langchain.Core.Tool (Tool, toolToValue)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (Header, JSON, ReqBody, (:>))
import Servant.API.EventStream
  ( FromServerEvent (fromServerEvent)
  , PostServerSentEvents
  , ServerEvent (eventData)
  , jsonData
  )
import Servant.Client.Core.BaseUrl (parseBaseUrl)
import Servant.Client.Streaming (ClientM, client, mkClientEnv, withClientM)
import Servant.Conduit ()

-- | Configuration for OpenAI provider
data OpenAIConfig = OpenAIConfig
  { configApiKey :: Text
  , configModel :: Text
  , configBaseUrl :: Maybe Text
  , configTemperature :: Maybe Double
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

defaultConfig :: Text -> OpenAIConfig
defaultConfig key = OpenAIConfig key "gpt-4o" Nothing (Just 0.7)

defaultOpenAIConfig :: Text -> OpenAIConfig
defaultOpenAIConfig = defaultConfig

-- | OpenAI ChatModel implementation
data OpenAI = OpenAI
  { apiKey :: Text
  , model :: Text
  , baseUrl :: Text
  {- ^ Base URL (e.g. @"https://api.openai.com"@). The @openai@ package
  automatically appends @\/v1\/chat\/completions@.
  -}
  , temperature :: Maybe Double
  }
  deriving (Eq, Show)

-- | Controls how OpenAI chooses among request tool definitions.
data OpenAIToolChoice
  = OpenAIToolAuto
  | OpenAIToolNone
  | OpenAIToolRequired
  | OpenAIToolFunction Text
  deriving (Eq, Show)

-- | Build stream request options from langchain tools.
openAITools :: [Tool m] -> OpenAIToolChoice -> Value
openAITools tools choice =
  object
    [ "tools" .= map toolToValue tools
    , "tool_choice" .= toolChoiceValue choice
    ]

toolChoiceValue :: OpenAIToolChoice -> Value
toolChoiceValue OpenAIToolAuto = String "auto"
toolChoiceValue OpenAIToolNone = String "none"
toolChoiceValue OpenAIToolRequired = String "required"
toolChoiceValue (OpenAIToolFunction name) =
  object
    [ "type" .= ("function" :: Text)
    , "function" .= object ["name" .= name]
    ]

data OpenAIStreamEvent
  = OpenAIChunk OpenAIStreamChunk
  | OpenAIDone

instance FromServerEvent OpenAIStreamEvent where
  fromServerEvent event
    | eventData event == "[DONE]" = Right OpenAIDone
    | otherwise = OpenAIChunk <$> jsonData event

data OpenAIStreamChunk = OpenAIStreamChunk
  { streamChoices :: [OpenAIStreamChoice]
  , streamUsage :: Maybe TokenUsage
  }

instance Aeson.FromJSON OpenAIStreamChunk where
  parseJSON = Aeson.withObject "OpenAIStreamChunk" $ \obj ->
    OpenAIStreamChunk
      <$> obj Aeson..:? "choices" Aeson..!= []
      <*> (obj Aeson..:? "usage" >>= traverse parseOpenAIStreamUsage)

parseOpenAIStreamUsage :: Value -> Parser TokenUsage
parseOpenAIStreamUsage = Aeson.withObject "OpenAIStreamUsage" $ \obj ->
  TokenUsage
    <$> obj Aeson..: "prompt_tokens"
    <*> obj Aeson..: "completion_tokens"
    <*> obj Aeson..: "total_tokens"

data OpenAIStreamChoice = OpenAIStreamChoice
  { streamChoiceIndex :: Int
  , streamChoiceDelta :: OpenAIStreamDelta
  }

instance Aeson.FromJSON OpenAIStreamChoice where
  parseJSON = Aeson.withObject "OpenAIStreamChoice" $ \obj ->
    OpenAIStreamChoice
      <$> obj Aeson..: "index"
      <*> obj Aeson..: "delta"

data OpenAIStreamDelta = OpenAIStreamDelta
  { streamContent :: Maybe Text
  , streamToolCalls :: [OpenAIStreamToolCall]
  }

instance Aeson.FromJSON OpenAIStreamDelta where
  parseJSON = Aeson.withObject "OpenAIStreamDelta" $ \obj ->
    OpenAIStreamDelta
      <$> obj Aeson..:? "content"
      <*> obj Aeson..:? "tool_calls" Aeson..!= []

data OpenAIStreamToolCall = OpenAIStreamToolCall
  { streamToolCallIndex :: Int
  , streamToolCallId :: Maybe Text
  , streamToolCallName :: Maybe Text
  , streamToolCallArguments :: Maybe Text
  }

instance Aeson.FromJSON OpenAIStreamToolCall where
  parseJSON = Aeson.withObject "OpenAIStreamToolCall" $ \obj -> do
    streamToolCallIndex <- obj Aeson..: "index"
    streamToolCallId <- obj Aeson..:? "id"
    streamFunction <- obj Aeson..:? "function"
    let streamToolCallName = streamFunction >>= parseMaybe (Aeson..: "name")
        streamToolCallArguments = streamFunction >>= parseMaybe (Aeson..: "arguments")
    pure
      OpenAIStreamToolCall
        { streamToolCallIndex
        , streamToolCallId
        , streamToolCallName
        , streamToolCallArguments
        }

data PartialToolCall = PartialToolCall
  { partialToolCallId :: Maybe Text
  , partialToolCallName :: Maybe Text
  , partialToolCallArguments :: Text
  }

type OpenAIStreamApi =
  "v1"
    :> "chat"
    :> "completions"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Value
    :> PostServerSentEvents (ConduitT () OpenAIStreamEvent IO ())

openAIStreamClient ::
  Maybe Text -> Value -> ClientM (ConduitT () OpenAIStreamEvent IO ())
openAIStreamClient = client (Proxy :: Proxy OpenAIStreamApi)

streamRequestBody :: CC.CreateChatCompletion -> Maybe Value -> Value
streamRequestBody request options = case Aeson.toJSON request of
  Object fields ->
    Object $
      KeyMap.insert "stream_options" (object ["include_usage" Aeson..= True]) $
        KeyMap.insert "stream" (Bool True) $
          KeyMap.union fields optionFields
  value -> value
  where
    optionFields = case options of
      Just (Object fields) -> fields
      _ -> mempty

-- | Create standard OpenAI provider instance
newOpenAI :: Text -> Text -> OpenAI
newOpenAI key mName =
  OpenAI
    { apiKey = key
    , model = mName
    , baseUrl = "https://api.openai.com"
    , temperature = Just 0.7
    }

{- | Create OpenAICompatible provider instance for OpenRouter/Fireworks/Together.

The @endpoint@ should be the __base URL__ only (e.g.
@"https://openrouter.ai/api"@), not the full chat completions path.
The @openai@ package appends @\/v1\/chat\/completions@ automatically.
-}
openAICompatible :: Text -> Text -> Text -> OpenAI
openAICompatible key mName endpoint =
  OpenAI
    { apiKey = key
    , model = mName
    , baseUrl = endpoint
    , temperature = Just 0.7
    }

-- ---------------------------------------------------------------------------
-- Conversion: langchain-hs Message -> openai package Message
-- ---------------------------------------------------------------------------

-- | Convert a langchain 'ContentBlock' to an openai 'CC.Content'.
contentBlockToOAI :: ContentBlock -> CC.Content
contentBlockToOAI (TextBlock t) = CC.Text {CC.text = t}
contentBlockToOAI (ImageBlock ImageContent {imageSource = ImageUrl url}) =
  CC.Image_URL {CC.image_url = CC.ImageURL {CC.url = url, CC.detail = Nothing}}
contentBlockToOAI (ImageBlock ImageContent {imageSource = ImageBase64 (Just mime) b64}) =
  CC.Image_URL
    { CC.image_url =
        CC.ImageURL
          { CC.url = "data:" <> mime <> ";base64," <> b64
          , CC.detail = Nothing
          }
    }
contentBlockToOAI (ImageBlock ImageContent {imageSource = ImageBase64 Nothing b64}) =
  CC.Image_URL
    { CC.image_url =
        CC.ImageURL
          { CC.url = "data:application/octet-stream;base64," <> b64
          , CC.detail = Nothing
          }
    }
contentBlockToOAI (AudioBlock _mime _b64) =
  -- Audio blocks are represented as text placeholders in the request
  CC.Text {CC.text = "[Audio content]"}
contentBlockToOAI (DataBlock _) =
  CC.Text {CC.text = "[Data block]"}

-- | Convert a langchain 'Message' to an openai package 'CC.Message'.
toLangchainOAIMessage :: Message -> CC.Message (V.Vector CC.Content)
toLangchainOAIMessage msg =
  let contents = V.fromList $ map contentBlockToOAI (NonEmpty.toList (messageContents msg))
   in case messageRole msg of
        System ->
          CC.System {CC.content = contents, CC.name = messageName msg}
        User ->
          CC.User {CC.content = contents, CC.name = messageName msg}
        Assistant ->
          CC.Assistant
            { CC.assistant_content = Just contents
            , CC.refusal = Nothing
            , CC.name = messageName msg
            , CC.assistant_audio = Nothing
            , CC.tool_calls = V.fromList . map toOAIToolCall <$> messageToolCalls msg
            }
        Tool ->
          CC.Tool
            { CC.content = contents
            , CC.tool_call_id = fromMaybe "" (messageToolId msg)
            }
        -- Developer and Function map to System for the openai package
        Developer ->
          CC.System {CC.content = contents, CC.name = messageName msg}
        Function ->
          CC.System {CC.content = contents, CC.name = messageName msg}
  where
    toOAIToolCall toolCall =
      OTC.ToolCall_Function
        { OTC.id = toolCallId toolCall
        , OTC.function = OTC.Function {OTC.name = toolCallName toolCall, OTC.arguments = arguments toolCall}
        }

    arguments = TE.decodeUtf8 . LBS.toStrict . Aeson.encode . toolCallArguments

-- ---------------------------------------------------------------------------
-- Conversion: openai package response -> langchain-hs Message
-- ---------------------------------------------------------------------------

-- | Convert an openai package 'CC.Choice' response message to a langchain 'Message'.
fromOAIMessage :: CC.Message Text -> Message
fromOAIMessage oaiMsg = case oaiMsg of
  CC.Assistant {CC.assistant_content, CC.tool_calls = oaiToolCalls, CC.name = nm} ->
    let contentText = fromMaybe "" assistant_content
        baseMsg = (assistantMessage contentText) {messageName = nm}
        tcList = case oaiToolCalls of
          Nothing -> Nothing
          Just tcs ->
            Just $
              map
                ( \(OTC.ToolCall_Function {OTC.id = tcId, OTC.function = fn}) ->
                    let argVal = case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 (OTC.arguments fn))) of
                          Just v -> v
                          Nothing -> object []
                     in ToolCall tcId "function" (OTC.name fn) argVal
                )
                (V.toList tcs)
     in baseMsg {messageToolCalls = tcList}
  CC.System {CC.content = c} -> systemMessage c
  CC.User {CC.content = c} -> userMessage c
  CC.Tool {CC.content = c} ->
    textMessage Tool c

-- | Convert openai 'OU.Usage' to langchain 'TokenUsage'.
fromOAIUsage :: OU.Usage ctd ptd -> TokenUsage
fromOAIUsage u =
  TokenUsage
    { promptTokens = fromIntegral (OU.prompt_tokens u)
    , completionTokens = fromIntegral (OU.completion_tokens u)
    , totalTokens = fromIntegral (OU.total_tokens u)
    }

-- ---------------------------------------------------------------------------
-- ChatModel instance
-- ---------------------------------------------------------------------------

instance ChatModel OpenAI where
  type ModelConfig OpenAI = Value

  invoke provider inputMsgs _ = do
    resp <- liftIO $ first asText <$> try createComplention
    case resp of
      Left err -> throwError $ llmError' err
      Right (CC.ChatCompletionObject {CC.choices = choicesVec, CC.usage = oaiUsage}) -> do
        case V.toList choicesVec of
          [] -> throwError $ llmError "Empty choices array in OpenAI response" Nothing Nothing
          (choice : _) -> do
            let respMsg = fromOAIMessage $ CC.message choice
                _usage = fromOAIUsage oaiUsage
            pure respMsg {messageToolCalls = messageToolCalls respMsg}
    where
      createComplention = do
        methods <- getMethods
        let body = reqBody provider inputMsgs
        OAI.createChatCompletion methods body
      getMethods = do
        clientEnv <- OAI.getClientEnv (normalizeBaseUrl (baseUrl provider))
        pure $ OAI.makeMethods clientEnv (apiKey provider) Nothing Nothing

  stream provider inputMsgs options = do
    yield $ LLMStart rId (model provider) inputMsgs

    (accumulated, toolCalls, usage) <-
      callbackSource openAIEvents
        .| receiveChunks "" Map.empty Nothing

    yield $ LLMEnd rId ((assistantMessage accumulated) {messageToolCalls = toolCalls}) usage
    where
      rId = "openai-stream-run"
      receiveChunks accumulated toolCalls usage =
        await >>= \case
          Nothing -> finishStream
          Just (Left err) -> throwError $ llmError' err
          Just (Right OpenAIDone) -> finishStream
          Just (Right (OpenAIChunk OpenAIStreamChunk {streamChoices, streamUsage})) -> do
            let (texts, nextToolCalls) = handleChoice $ List.find choice0 streamChoices
                nextUsage = streamUsage <|> usage
            mapM_ (\text -> yield $ LLMChunk rId text Nothing) texts
            receiveChunks (accumulated <> mconcat texts) nextToolCalls nextUsage
        where
          choice0 = (== 0) . streamChoiceIndex

          handleChoice Nothing = ([], toolCalls)
          handleChoice (Just OpenAIStreamChoice {streamChoiceDelta = OpenAIStreamDelta {streamContent, streamToolCalls}}) =
            let nextToolCalls = List.foldl' addToolCall toolCalls streamToolCalls
             in (maybe [] pure streamContent, nextToolCalls)

          addToolCall
            toolCalls'
            OpenAIStreamToolCall
              { streamToolCallIndex
              , streamToolCallId
              , streamToolCallName
              , streamToolCallArguments
              } =
              Map.alter (Just . update) streamToolCallIndex toolCalls'
              where
                update curr =
                  let prevId = curr >>= partialToolCallId
                      prevName = curr >>= partialToolCallName
                      prevArgs = maybe "" partialToolCallArguments curr
                      nextArgs = fromMaybe "" streamToolCallArguments
                   in PartialToolCall
                        { partialToolCallId = streamToolCallId <|> prevId
                        , partialToolCallName = streamToolCallName <|> prevName
                        , partialToolCallArguments = prevArgs <> nextArgs
                        }

          finishStream = do
            finalToolCalls <- lift $ traverse toToolCall $ Map.elems toolCalls
            mapM_ (yield . LLMChunk rId "" . Just) finalToolCalls
            pure (accumulated, nonEmpty finalToolCalls, usage)
            where
              nonEmpty [] = Nothing
              nonEmpty xs = Just xs

              toToolCall :: PartialToolCall -> StreamM ToolCall
              toToolCall PartialToolCall {partialToolCallId, partialToolCallName, partialToolCallArguments} = do
                toolCallId <- mb partialToolCallId "OpenAI stream ended with a tool call missing an id"
                toolCallName <-
                  mb partialToolCallName "OpenAI stream ended with a tool call missing a function name"
                toolCallArguments <- case decode partialToolCallArguments of
                  Left err -> throw $ "Invalid JSON arguments in OpenAI tool call: " <> T.pack err
                  Right arguments -> pure arguments
                pure ToolCall {toolCallId, toolCallType = "function", toolCallName, toolCallArguments}

              throw = throwError . llmError'
              mb x errMsg = maybe (throw errMsg) pure x
              decode = Aeson.eitherDecode . LBS.fromStrict . TE.encodeUtf8

      -- \| Internal function to handle streaming events from OpenAI.
      openAIEvents emit = do
        result <- try $ do
          manager <- newManager tlsManagerSettings
          let baseUrl' = T.unpack $ normalizeBaseUrl $ baseUrl provider
          clientEnv <- mkClientEnv manager <$> parseBaseUrl baseUrl'
          let body = reqBody provider inputMsgs
              bearerToken = Just $ "Bearer " <> apiKey provider
              request = openAIStreamClient bearerToken $ streamRequestBody body options

          withClientM request clientEnv $ \case
            Left err ->
              emit $ Left $ T.pack $ show err
            Right source ->
              runConduit $
                source .| C.mapM_ (emit . Right)

        case result of
          Left err
            | Just AsyncCancelled <- fromException err -> throwIO err
            | otherwise -> emit $ Left $ asText err
          Right () -> pure ()

-- | A type alias for a streaming callback function that produces values of type @a@.
type StreamCallback a = (a -> IO ()) -> IO ()

-- | A type alias for a Conduit source that produces values of type @a@ in the 'StreamM' monad.
type StreamSource a = ConduitT () a StreamM ()

-- | Convert a callback-based streaming function into a Conduit source.
callbackSource :: StreamCallback a -> StreamSource a
callbackSource produce = bracketP start (cancel . third) consume
  where
    start = do
      queue <- newTBQueueIO 64
      finished <- newEmptyTMVarIO
      worker <-
        async $ produce (atomically . writeTBQueue queue) `finally` atomically (putTMVar finished ())
      pure (queue, finished, worker)

    consume (queue, finished, _worker) = loop
      where
        loop = do
          let waitForFinished = Nothing <$ readTMVar finished
              readEvent = Just <$> readTBQueue queue
          next <- liftIO . atomically $ readEvent `orElse` waitForFinished
          case next of
            Just item -> yield item >> loop
            Nothing -> pure ()

    third (_, _, worker) = worker

-- | Convert a 'SomeException' to 'Text' for error reporting.
asText :: SomeException -> Text
asText ex = T.pack $ show (ex :: SomeException)

-- | Construct a 'LangchainError' for LLM errors with optional details.
llmError' :: Text -> LangchainError
llmError' msg = llmError msg Nothing Nothing

-- | Construct the request body for OpenAI chat completion.
reqBody :: OpenAI -> [Message] -> CC.CreateChatCompletion
reqBody provider inputMsgs =
  CC._CreateChatCompletion
    { CC.messages = toVec inputMsgs
    , CC.model = OM.Model $ model provider
    , CC.temperature = temperature provider
    }
  where
    toVec = V.fromList . map toLangchainOAIMessage

{- | Normalize base URL to ensure compatibility with the @openai@ package.
Strips any trailing @/v1/chat/completions@, @/chat/completions@, or @/v1@
so that Servant's route constructs the expected URL path.
-}
normalizeBaseUrl :: Text -> Text
normalizeBaseUrl rawUrl =
  let u0 = T.dropWhileEnd (== '/') rawUrl
      u1
        | "/v1/chat/completions" `T.isSuffixOf` u0 =
            T.dropEnd (T.length "/v1/chat/completions") u0
        | "/chat/completions" `T.isSuffixOf` u0 =
            T.dropEnd (T.length "/chat/completions") u0
        | "/v1" `T.isSuffixOf` u0 =
            T.dropEnd (T.length "/v1") u0
        | otherwise =
            u0
   in T.dropWhileEnd (== '/') u1

-- ---------------------------------------------------------------------------
-- Backward-compatible parseOpenAIResponse
-- ---------------------------------------------------------------------------

{- | Parse a raw OpenAI JSON response 'Value' into a langchain 'Message'
and optional 'TokenUsage'.

This function is provided for backward compatibility. New code should use
the typed @openai@ package types directly.
-}
parseOpenAIResponse :: Value -> Either String (Message, Maybe TokenUsage)
parseOpenAIResponse = parseEither $ Aeson.withObject "OpenAIResponse" $ \o -> do
  choices <- o Aeson..: "choices"
  usageVal <- o Aeson..:? "usage"
  mbUsage <- case usageVal of
    Nothing -> pure Nothing
    Just u -> flip (Aeson.withObject "Usage") u $ \uo -> do
      pTok <- uo Aeson..:? "prompt_tokens" Aeson..!= 0
      cTok <- uo Aeson..:? "completion_tokens" Aeson..!= 0
      tTok <- uo Aeson..:? "total_tokens" Aeson..!= 0
      pure $ Just $ TokenUsage pTok cTok tTok
  case choices of
    [] -> fail "Empty choices array in OpenAI response"
    (c : _) -> flip (Aeson.withObject "Choice") c $ \ch -> do
      msgObj <- ch Aeson..: "message"
      contentTxt <- msgObj Aeson..:? "content" Aeson..!= ""
      mbToolCalls <- msgObj Aeson..:? "tool_calls"
      cToolCalls <- case mbToolCalls of
        Nothing -> pure Nothing
        Just tcs -> do
          calls <- forM (tcs :: [Value]) $ Aeson.withObject "ToolCall" $ \tcObj -> do
            tcId <- tcObj Aeson..:? "id" Aeson..!= ""
            fnObj <- tcObj Aeson..: "function"
            fnName <- fnObj Aeson..: "name"
            fnArgsVal <- fnObj Aeson..:? "arguments"
            let fnArgs = case fnArgsVal of
                  Just (String s) -> case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 s)) of
                    Just val -> val
                    Nothing -> object []
                  Just obj@(Object _) -> obj
                  _ -> object []
            pure $ ToolCall tcId "function" fnName fnArgs
          pure (Just calls)
      let msg = (assistantMessage contentTxt) {messageToolCalls = cToolCalls}
      pure (msg, mbUsage)

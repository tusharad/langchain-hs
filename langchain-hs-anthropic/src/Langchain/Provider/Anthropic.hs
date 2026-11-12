{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Provider.Anthropic
Description : Native Anthropic Claude Messages API provider for langchain-hs
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Anthropic Claude Messages API (@/v1/messages@) integration implementing
'ChatModel', 'ToolBinder', and 'CacheableChatModel' with full support for
top-level @system@ prompts, multi-modal images, @tool_use@/@tool_result@ blocks,
prompt caching, extended thinking budgets, and SSE incremental tool streaming.
-}
module Langchain.Provider.Anthropic
  ( -- * Provider & Construction
    Anthropic (..)
  , newAnthropic
  , newAnthropicWithBaseUrl
  , withBaseUrl
  , withAnthropicVersion

    -- * Configuration & Combinators
  , AnthropicTool (..)
  , AnthropicConfig (..)
  , defaultAnthropicConfig
  , withMaxTokens
  , withThinkingBudget
  , withPromptCaching
  , withSystem
  , withTemperature

    -- * Request / Response Conversion & Streaming Helpers
  , messagesToAnthropic
  , redactApiKey
  , splitSseFrames
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (bracketP, yield)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( BodyReader
  , HttpException
  , Request
  , RequestBody (RequestBodyLBS)
  , Response
  , brConsume
  , brRead
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseClose
  , responseOpen
  , responseStatus
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusIsSuccessful, statusMessage)

import Langchain.Cache.Core (CacheableChatModel (..))
import Langchain.Error (LangchainError, llmError)
import Langchain.Model
import Langchain.Stream (ChatStream, StreamEvent (..), TokenUsage (..))
import qualified Langchain.Tool as CoreTool
import Langchain.Tool.Binding (ToolBinder (..))

-- | Anthropic provider configuration for the @/v1/messages@ API.
data Anthropic = Anthropic
  { anthropicApiKey :: !Text
  -- ^ Anthropic API key sent via @x-api-key@ header
  , anthropicModel :: !Text
  -- ^ Target Claude model identifier (e.g. @"claude-sonnet-4-5"@)
  , anthropicBaseUrl :: !Text
  -- ^ API base URL (default: @"https://api.anthropic.com"@)
  , anthropicVersion :: !Text
  -- ^ Value for the @anthropic-version@ header (default: @"2023-06-01"@)
  }
  deriving (Eq, Show, Generic)

-- | Tool schema formatted for the Anthropic Messages API.
data AnthropicTool = AnthropicTool
  { anthropicToolName :: !Text
  , anthropicToolDescription :: !Text
  , anthropicToolInputSchema :: !Value
  }
  deriving (Eq, Show, Generic)

instance ToJSON AnthropicTool where
  toJSON AnthropicTool {..} =
    object
      [ "name" .= anthropicToolName
      , "description" .= anthropicToolDescription
      , "input_schema" .= anthropicToolInputSchema
      ]

instance FromJSON AnthropicTool where
  parseJSON v = case v of
    Object o ->
      AnthropicTool
        <$> o .: "name"
        <*> o .: "description"
        <*> o .: "input_schema"
    _ -> fail "Expected AnthropicTool JSON object"

-- | Runtime request parameters for Anthropic Messages API calls.
data AnthropicConfig = AnthropicConfig
  { configMaxTokens :: !Int
  -- ^ Maximum output tokens (required by Anthropic API, default: 4096)
  , configTemperature :: !(Maybe Double)
  , configTopP :: !(Maybe Double)
  , configTopK :: !(Maybe Int)
  , configStopSequences :: !(Maybe [Text])
  , configSystem :: !(Maybe Text)
  , configTools :: !(Maybe [AnthropicTool])
  , configToolChoice :: !(Maybe Value)
  , configThinkingBudget :: !(Maybe Int)
  -- ^ Optional extended thinking token budget
  , configPromptCaching :: !Bool
  -- ^ Enable ephemeral @cache_control@ markers on system/tools
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default configuration with @max_tokens = 4096@ and prompt caching disabled.
defaultAnthropicConfig :: AnthropicConfig
defaultAnthropicConfig =
  AnthropicConfig
    { configMaxTokens = 4096
    , configTemperature = Nothing
    , configTopP = Nothing
    , configTopK = Nothing
    , configStopSequences = Nothing
    , configSystem = Nothing
    , configTools = Nothing
    , configToolChoice = Nothing
    , configThinkingBudget = Nothing
    , configPromptCaching = False
    }

-- | Construct an 'Anthropic' provider using the default Anthropic API base URL.
newAnthropic :: Text -> Text -> Anthropic
newAnthropic key mdl =
  Anthropic
    { anthropicApiKey = key
    , anthropicModel = mdl
    , anthropicBaseUrl = "https://api.anthropic.com"
    , anthropicVersion = "2023-06-01"
    }

-- | Construct an 'Anthropic' provider targeting a custom base URL (e.g. proxy or local mock server).
newAnthropicWithBaseUrl :: Text -> Text -> Text -> Anthropic
newAnthropicWithBaseUrl key mdl url =
  (newAnthropic key mdl) {anthropicBaseUrl = url}

-- | Override the API base URL on an 'Anthropic' instance.
withBaseUrl :: Text -> Anthropic -> Anthropic
withBaseUrl url a = a {anthropicBaseUrl = url}

-- | Override the @anthropic-version@ header on an 'Anthropic' instance.
withAnthropicVersion :: Text -> Anthropic -> Anthropic
withAnthropicVersion ver a = a {anthropicVersion = ver}

-- | Configure @max_tokens@.
withMaxTokens :: Int -> AnthropicConfig -> AnthropicConfig
withMaxTokens n cfg = cfg {configMaxTokens = n}

-- | Configure extended thinking token budget (@thinking.budget_tokens@).
withThinkingBudget :: Int -> AnthropicConfig -> AnthropicConfig
withThinkingBudget n cfg = cfg {configThinkingBudget = Just n}

-- | Enable or disable ephemeral prompt caching (@cache_control@).
withPromptCaching :: Bool -> AnthropicConfig -> AnthropicConfig
withPromptCaching enabled cfg = cfg {configPromptCaching = enabled}

-- | Set explicit system instruction override.
withSystem :: Text -> AnthropicConfig -> AnthropicConfig
withSystem sys cfg = cfg {configSystem = Just sys}

-- | Set sampling temperature.
withTemperature :: Double -> AnthropicConfig -> AnthropicConfig
withTemperature t cfg = cfg {configTemperature = Just t}

-- | Redact every occurrence of the API key from error text.
redactApiKey :: Text -> Text -> Text
redactApiKey key txt
  | T.null key = txt
  | otherwise = T.replace key "[REDACTED]" txt

encodeContentBlock :: ContentBlock -> Value
encodeContentBlock (TextBlock txt) =
  object ["type" .= ("text" :: Text), "text" .= txt]
encodeContentBlock (ImageBlock ImageContent {imageSource = ImageBase64 mMime b64}) =
  object
    [ "type" .= ("image" :: Text)
    , "source"
        .= object
          [ "type" .= ("base64" :: Text)
          , "media_type" .= fromMaybe "image/png" mMime
          , "data" .= b64
          ]
    ]
encodeContentBlock (ImageBlock ImageContent {imageSource = ImageUrl url}) =
  object
    [ "type" .= ("image" :: Text)
    , "source"
        .= object
          [ "type" .= ("url" :: Text)
          , "url" .= url
          ]
    ]
encodeContentBlock (AudioBlock mime b64) =
  object
    [ "type" .= ("text" :: Text)
    , "text" .= ("[audio:" <> mime <> " len=" <> T.pack (show (T.length b64)) <> "]")
    ]
encodeContentBlock (DataBlock bs) =
  object
    ["type" .= ("text" :: Text), "text" .= ("[data:" <> T.pack (show (BS.length bs)) <> " bytes]")]

toRoleAndBlocks :: Message -> Maybe (Text, [Value])
toRoleAndBlocks msg = case messageRole msg of
  System -> Nothing
  Developer -> Nothing
  User -> Just ("user", map encodeContentBlock (NonEmpty.toList (messageContents msg)))
  Assistant ->
    let txt = extractMessageText msg
        txtBlocks = [object ["type" .= ("text" :: Text), "text" .= txt] | not (T.null txt)]
        toolBlocks =
          [ object
              [ "type" .= ("tool_use" :: Text)
              , "id" .= toolCallId tc
              , "name" .= toolCallName tc
              , "input" .= toolCallArguments tc
              ]
          | tc <- fromMaybe [] (messageToolCalls msg)
          ]
        allBlocks = txtBlocks ++ toolBlocks
        nonEmptyBlocks =
          if null allBlocks then [object ["type" .= ("text" :: Text), "text" .= ("" :: Text)]] else allBlocks
     in Just ("assistant", nonEmptyBlocks)
  Tool -> toolResultMsg
  Function -> toolResultMsg
  where
    toolResultMsg =
      Just
        ( "user"
        ,
          [ object
              [ "type" .= ("tool_result" :: Text)
              , "tool_use_id" .= fromMaybe "" (messageToolId msg)
              , "content" .= extractMessageText msg
              ]
          ]
        )

coalesceRoleBlocks :: [(Text, [Value])] -> [(Text, [Value])]
coalesceRoleBlocks = foldr step []
  where
    step (r, bs) ((r', bs') : rest)
      | r == r' = (r, bs ++ bs') : rest
    step item acc = item : acc

-- | Convert Langchain 'Message's into Anthropic top-level @system@ and @messages@ JSON values.
messagesToAnthropic :: Maybe Text -> [Message] -> (Maybe Text, [Value])
messagesToAnthropic explicitSys msgs =
  let msgSystems =
        [ extractMessageText m
        | m <- msgs
        , messageRole m == System || messageRole m == Developer
        , not (T.null (extractMessageText m))
        ]
      sysParts = maybe [] pure explicitSys ++ msgSystems
      combinedSys = if null sysParts then Nothing else Just (T.intercalate "\n\n" sysParts)
      bodyMsgs =
        [ object ["role" .= r, "content" .= bs]
        | (r, bs) <- coalesceRoleBlocks (foldr (\m acc -> maybe acc (: acc) (toRoleAndBlocks m)) [] msgs)
        ]
   in (combinedSys, bodyMsgs)

toolWithOptionalCache :: Bool -> Bool -> AnthropicTool -> Value
toolWithOptionalCache True True AnthropicTool {..} =
  object
    [ "name" .= anthropicToolName
    , "description" .= anthropicToolDescription
    , "input_schema" .= anthropicToolInputSchema
    , "cache_control" .= object ["type" .= ("ephemeral" :: Text)]
    ]
toolWithOptionalCache _ _ tool = toJSON tool

encodeToolsWithCaching :: Bool -> [AnthropicTool] -> Value
encodeToolsWithCaching _ [] = toJSON ([] :: [AnthropicTool])
encodeToolsWithCaching enableCache tools =
  let len = length tools
      encoded = zipWith (\idx t -> toolWithOptionalCache enableCache (idx == len) t) [1 ..] tools
   in toJSON encoded

buildAnthropicBody :: Anthropic -> Bool -> Maybe AnthropicConfig -> [Message] -> Value
buildAnthropicBody Anthropic {..} isStream mbCfg msgs =
  let AnthropicConfig {..} = fromMaybe defaultAnthropicConfig mbCfg
      (mbSys, encodedMsgs) = messagesToAnthropic configSystem msgs
      sysVal = case mbSys of
        Nothing -> Nothing
        Just sTxt
          | configPromptCaching ->
              Just $
                toJSON
                  [ object
                      [ "type" .= ("text" :: Text)
                      , "text" .= sTxt
                      , "cache_control" .= object ["type" .= ("ephemeral" :: Text)]
                      ]
                  ]
          | otherwise -> Just (String sTxt)
      basePairs =
        [ "model" .= anthropicModel
        , "max_tokens" .= configMaxTokens
        , "messages" .= encodedMsgs
        , "stream" .= isStream
        ]
      optPairs =
        catMaybes
          [ ("system" .=) <$> sysVal
          , ("temperature" .=) <$> configTemperature
          , ("top_p" .=) <$> configTopP
          , ("top_k" .=) <$> configTopK
          , ("stop_sequences" .=) <$> configStopSequences
          , (\ts -> "tools" .= encodeToolsWithCaching configPromptCaching ts) <$> configTools
          , ("tool_choice" .=) <$> configToolChoice
          , (\b -> "thinking" .= object ["type" .= ("enabled" :: Text), "budget_tokens" .= b])
              <$> configThinkingBudget
          ]
   in object (basePairs ++ optPairs)

messagesEndpoint :: Text -> Text
messagesEndpoint raw =
  let trimmed = T.dropWhileEnd (== '/') raw
   in if "/v1/messages" `T.isSuffixOf` trimmed
        then trimmed
        else
          if "/v1" `T.isSuffixOf` trimmed
            then trimmed <> "/messages"
            else trimmed <> "/v1/messages"

mkHttpRequest :: Anthropic -> Bool -> Value -> IO (Either LangchainError Request)
mkHttpRequest a@Anthropic {..} isStream body = do
  let urlStr = T.unpack (messagesEndpoint anthropicBaseUrl)
  eBaseReq <- try @SomeException (parseRequest urlStr)
  pure $ case eBaseReq of
    Left ex ->
      Left $
        llmError
          (redactApiKey anthropicApiKey $ "Invalid Anthropic URL: " <> T.pack (displayException ex))
          (Just "Anthropic")
          Nothing
    Right baseReq ->
      let _ = a
       in Right
            baseReq
              { method = "POST"
              , requestHeaders =
                  [ ("x-api-key", TE.encodeUtf8 anthropicApiKey)
                  , ("anthropic-version", TE.encodeUtf8 anthropicVersion)
                  , ("content-type", "application/json")
                  , ("accept", if isStream then "text/event-stream" else "application/json")
                  ]
              , requestBody = RequestBodyLBS (Aeson.encode body)
              }

parseResponseBlock :: Value -> Parser (Maybe Text, Maybe ToolCall)
parseResponseBlock = Aeson.withObject "AnthropicContentBlock" $ \o -> do
  typ :: Text <- o .: "type"
  case typ of
    "text" -> do
      txt <- o .: "text"
      pure (Just txt, Nothing)
    "tool_use" -> do
      cid <- o .: "id"
      cname <- o .: "name"
      cinput <- o .:? "input"
      let tc = ToolCall cid "function" cname (fromMaybe (object []) cinput)
      pure (Nothing, Just tc)
    _ -> pure (Nothing, Nothing)

parseAnthropicResponse :: Value -> Either Text Message
parseAnthropicResponse =
  first T.pack
    . parseEither
      ( Aeson.withObject "AnthropicResponse" $ \o -> do
          blocks :: [Value] <- o .: "content"
          parsed <- mapM parseResponseBlock blocks
          let txts = [t | (Just t, _) <- parsed]
              tcs = [tc | (_, Just tc) <- parsed]
              fullText = T.concat txts
              mbTools = if null tcs then Nothing else Just tcs
          pure $ (assistantMessage fullText) {messageToolCalls = mbTools}
      )

httpFailureError :: Anthropic -> Response a -> LBS.ByteString -> LangchainError
httpFailureError Anthropic {..} resp bodyBytes =
  let code = statusCode (responseStatus resp)
      reason = TE.decodeUtf8 (statusMessage (responseStatus resp))
      bodyTxt = TE.decodeUtf8 (LBS.toStrict bodyBytes)
      rawMsg =
        "Anthropic HTTP "
          <> T.pack (show code)
          <> " "
          <> reason
          <> ": "
          <> bodyTxt
   in llmError (redactApiKey anthropicApiKey rawMsg) (Just "Anthropic") Nothing

-- | Split a buffered byte chunk into complete SSE frame payloads (delimited by blank lines) and trailing remainder.
splitSseFrames :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitSseFrames raw =
  let normalized = BSC.pack (stripCr (BSC.unpack raw))
   in go normalized []
  where
    stripCr [] = []
    stripCr ('\r' : '\n' : xs) = '\n' : stripCr xs
    stripCr (x : xs) = x : stripCr xs

    go buf acc
      | BS.null buf = (reverse acc, BS.empty)
      | otherwise =
          let (frame, restWithSep) = BS.breakSubstring "\n\n" buf
           in if BS.null restWithSep
                then (reverse acc, buf)
                else
                  let nextBuf = BS.drop 2 restWithSep
                      nextAcc = if BS.null frame then acc else frame : acc
                   in go nextBuf nextAcc

data ActiveBlock
  = ActiveText
  | ActiveTool !Text !Text ![Text]

data AnthropicSseState = AnthropicSseState
  { sseRunId :: !Text
  , sseModel :: !Text
  , sseStarted :: !Bool
  , sseEnded :: !Bool
  , sseAccText :: ![Text]
  , sseActiveBlocks :: !(Map Int ActiveBlock)
  , sseTools :: ![ToolCall]
  , sseInputTokens :: !Int
  , sseOutputTokens :: !Int
  }

initialSseState :: Text -> AnthropicSseState
initialSseState mdl =
  AnthropicSseState
    { sseRunId = "anthropic-stream"
    , sseModel = mdl
    , sseStarted = False
    , sseEnded = False
    , sseAccText = []
    , sseActiveBlocks = Map.empty
    , sseTools = []
    , sseInputTokens = 0
    , sseOutputTokens = 0
    }

parseRawSseFrame :: BS.ByteString -> (Maybe Text, Maybe BS.ByteString)
parseRawSseFrame frame =
  let ls = BSC.lines frame
   in List.foldl' step (Nothing, Nothing) ls
  where
    step (mEv, mData) line
      | "event:" `BS.isPrefixOf` line =
          let val = T.strip (TE.decodeUtf8 (BS.drop 6 line))
           in (Just val, mData)
      | "data:" `BS.isPrefixOf` line =
          let chunk = BS.dropWhile (== 32) (BS.drop 5 line)
              nextData = case mData of
                Nothing -> Just chunk
                Just prev -> Just (prev <> "\n" <> chunk)
           in (mEv, nextData)
      | otherwise = (mEv, mData)

buildFinalStreamMessage :: AnthropicSseState -> Message
buildFinalStreamMessage AnthropicSseState {..} =
  let fullTxt = T.concat (reverse sseAccText)
      toolsRev = reverse sseTools
      mbTools = if null toolsRev then Nothing else Just toolsRev
   in (assistantMessage fullTxt) {messageToolCalls = mbTools}

currentTokenUsage :: AnthropicSseState -> TokenUsage
currentTokenUsage AnthropicSseState {..} =
  TokenUsage
    { promptTokens = sseInputTokens
    , completionTokens = sseOutputTokens
    , totalTokens = sseInputTokens + sseOutputTokens
    }

ensureStarted :: AnthropicSseState -> [Message] -> ([StreamEvent], AnthropicSseState)
ensureStarted st@AnthropicSseState {..} inputMsgs
  | sseStarted = ([], st)
  | otherwise =
      ( [LLMStart sseRunId sseModel inputMsgs]
      , st {sseStarted = True}
      )

processSseFrame ::
  Text ->
  [Message] ->
  AnthropicSseState ->
  BS.ByteString ->
  Either LangchainError ([StreamEvent], AnthropicSseState)
processSseFrame apiKey inputMsgs st frame =
  case parseRawSseFrame frame of
    (_, Nothing) -> Right ([], st)
    (mbEvName, Just rawJson) ->
      case Aeson.eitherDecodeStrict rawJson of
        Left err ->
          Left $
            llmError
              (redactApiKey apiKey $ "Invalid Anthropic SSE JSON: " <> T.pack err)
              (Just "Anthropic")
              Nothing
        Right (Object obj) ->
          let evType =
                case parseEither (.: "type") obj of
                  Right t -> t
                  Left _ -> fromMaybe "" mbEvName
           in handleSseObject apiKey inputMsgs st evType obj
        Right _ ->
          Left $ llmError "Expected JSON object in Anthropic SSE event" (Just "Anthropic") Nothing

handleSseObject ::
  Text ->
  [Message] ->
  AnthropicSseState ->
  Text ->
  Aeson.Object ->
  Either LangchainError ([StreamEvent], AnthropicSseState)
handleSseObject apiKey inputMsgs st evType obj = case evType of
  "ping" -> Right ([], st)
  "error" ->
    let errTxt = case parseEither (\o -> (o .: "error") >>= (.: "message")) obj of
          Right m -> m
          Left _ -> TE.decodeUtf8 (LBS.toStrict (Aeson.encode obj))
     in Left $ llmError (redactApiKey apiKey errTxt) (Just "Anthropic") Nothing
  "message_start" ->
    case parseEither parseMsgStart obj of
      Left err ->
        Left $
          llmError ("Failed to parse Anthropic message_start: " <> T.pack err) (Just "Anthropic") Nothing
      Right (mbId, mbMdl, mbInTok) ->
        let st1 =
              st
                { sseRunId = fromMaybe (sseRunId st) mbId
                , sseModel = fromMaybe (sseModel st) mbMdl
                , sseInputTokens = fromMaybe (sseInputTokens st) mbInTok
                }
            (startEvs, st2) = ensureStarted st1 inputMsgs
         in Right (startEvs, st2)
  "content_block_start" ->
    let (startEvs, st1) = ensureStarted st inputMsgs
     in case parseEither parseBlockStart obj of
          Left err ->
            Left $
              llmError
                ("Failed to parse Anthropic content_block_start: " <> T.pack err)
                (Just "Anthropic")
                Nothing
          Right (idx, bTyp, mbInitText, mbToolMeta) ->
            case bTyp of
              "tool_use" ->
                let (tid, tname) = fromMaybe ("", "") mbToolMeta
                    st2 = st1 {sseActiveBlocks = Map.insert idx (ActiveTool tid tname []) (sseActiveBlocks st1)}
                 in Right (startEvs, st2)
              _ ->
                let st2 = st1 {sseActiveBlocks = Map.insert idx ActiveText (sseActiveBlocks st1)}
                    initEvs = case mbInitText of
                      Just txt | not (T.null txt) -> [LLMChunk (sseRunId st2) txt Nothing]
                      _ -> []
                    st3 = case mbInitText of
                      Just txt | not (T.null txt) -> st2 {sseAccText = txt : sseAccText st2}
                      _ -> st2
                 in Right (startEvs ++ initEvs, st3)
  "content_block_delta" ->
    let (startEvs, st1) = ensureStarted st inputMsgs
     in case parseEither parseBlockDelta obj of
          Left err ->
            Left $
              llmError
                ("Failed to parse Anthropic content_block_delta: " <> T.pack err)
                (Just "Anthropic")
                Nothing
          Right (idx, dTyp, mbTxt, mbJsonFrag) -> case dTyp of
            "text_delta" ->
              let txt = fromMaybe "" mbTxt
                  st2 = st1 {sseAccText = txt : sseAccText st1}
               in Right (startEvs ++ [LLMChunk (sseRunId st2) txt Nothing], st2)
            "input_json_delta" ->
              let frag = fromMaybe "" mbJsonFrag
                  upd (ActiveTool tid tname fs) = ActiveTool tid tname (frag : fs)
                  upd other = other
                  st2 = st1 {sseActiveBlocks = Map.adjust upd idx (sseActiveBlocks st1)}
               in Right (startEvs, st2)
            _ -> Right (startEvs, st1)
  "content_block_stop" ->
    let (startEvs, st1) = ensureStarted st inputMsgs
     in case parseEither (.: "index") obj of
          Left err ->
            Left $
              llmError ("Failed to parse Anthropic content_block_stop: " <> T.pack err) (Just "Anthropic") Nothing
          Right idx -> case Map.lookup idx (sseActiveBlocks st1) of
            Just (ActiveTool tid tname frags) ->
              let rawToolJson = T.concat (reverse frags)
                  parsedArgs
                    | T.null (T.strip rawToolJson) = Right (object [])
                    | otherwise = Aeson.eitherDecodeStrict (TE.encodeUtf8 rawToolJson)
               in case parsedArgs of
                    Left jErr ->
                      Left $
                        llmError
                          ( redactApiKey apiKey $
                              "Invalid JSON in completed tool_use block '" <> tname <> "': " <> T.pack jErr
                          )
                          (Just "Anthropic")
                          Nothing
                    Right argsVal ->
                      let tc = ToolCall tid "function" tname argsVal
                          st2 =
                            st1
                              { sseActiveBlocks = Map.delete idx (sseActiveBlocks st1)
                              , sseTools = tc : sseTools st1
                              }
                       in Right (startEvs ++ [LLMChunk (sseRunId st2) "" (Just tc)], st2)
            _ ->
              let st2 = st1 {sseActiveBlocks = Map.delete idx (sseActiveBlocks st1)}
               in Right (startEvs, st2)
  "message_delta" ->
    case parseEither parseMsgDelta obj of
      Left err ->
        Left $
          llmError ("Failed to parse Anthropic message_delta: " <> T.pack err) (Just "Anthropic") Nothing
      Right (mbInTok, mbOutTok) ->
        let st1 =
              st
                { sseInputTokens = fromMaybe (sseInputTokens st) mbInTok
                , sseOutputTokens = fromMaybe (sseOutputTokens st) mbOutTok
                }
         in Right ([], st1)
  "message_stop" ->
    let (startEvs, st1) = ensureStarted st inputMsgs
     in if sseEnded st1
          then Right (startEvs, st1)
          else
            let endEv = LLMEnd (sseRunId st1) (buildFinalStreamMessage st1) (Just (currentTokenUsage st1))
             in Right (startEvs ++ [endEv], st1 {sseEnded = True})
  _ -> Right ([], st)
  where
    parseMsgStart o = do
      msg <- o .: "message"
      mbId <- msg .:? "id"
      mbMdl <- msg .:? "model"
      mbUsage <- msg .:? "usage"
      mbInTok <- maybe (pure Nothing) (.:? "input_tokens") mbUsage
      pure (mbId, mbMdl, mbInTok)

    parseBlockStart o = do
      idx <- o .: "index"
      cb <- o .: "content_block"
      bTyp <- cb .: "type"
      mbText <- cb .:? "text"
      mbTool <- case (bTyp :: Text) of
        "tool_use" -> do
          tid <- cb .: "id"
          tname <- cb .: "name"
          pure (Just (tid, tname))
        _ -> pure Nothing
      pure (idx, bTyp, mbText, mbTool)

    parseBlockDelta o = do
      idx <- o .: "index"
      d <- o .: "delta"
      dTyp :: Text <- d .: "type"
      mbTxt <- d .:? "text"
      mbJson <- d .:? "partial_json"
      pure (idx, dTyp, mbTxt, mbJson)

    parseMsgDelta o = do
      mbUsage <- o .:? "usage"
      case mbUsage of
        Nothing -> pure (Nothing, Nothing)
        Just u -> (,) <$> u .:? "input_tokens" <*> u .:? "output_tokens"

streamSseBody ::
  Anthropic ->
  [Message] ->
  Response BodyReader ->
  ChatStream
streamSseBody provider inputMsgs resp = do
  if not (statusIsSuccessful (responseStatus resp))
    then do
      errBody <- LBS.fromChunks <$> liftIO (brConsume (responseBody resp))
      throwError $ httpFailureError provider resp errBody
    else loop BS.empty (initialSseState (anthropicModel provider))
  where
    apiKey = anthropicApiKey provider
    reader = responseBody resp

    emitFrames _ st [] = pure (Right st)
    emitFrames _ st (f : fs) =
      case processSseFrame apiKey inputMsgs st f of
        Left err -> pure (Left err)
        Right (evs, st') -> do
          mapM_ yield evs
          emitFrames apiKey st' fs

    loop buf st = do
      eChunk <- liftIO $ try @SomeException (brRead reader)
      case eChunk of
        Left ex ->
          throwError $
            llmError
              (redactApiKey apiKey $ "Anthropic stream transport error: " <> T.pack (displayException ex))
              (Just "Anthropic")
              Nothing
        Right chunk
          | BS.null chunk -> do
              let (finalFrames, _) = splitSseFrames (buf <> "\n\n")
              eFinalSt <- emitFrames apiKey st finalFrames
              case eFinalSt of
                Left err -> throwError err
                Right st' ->
                  if sseEnded st'
                    then pure ()
                    else do
                      let (startEvs, stStarted) = ensureStarted st' inputMsgs
                      mapM_ yield startEvs
                      yield $
                        LLMEnd
                          (sseRunId stStarted)
                          (buildFinalStreamMessage stStarted)
                          (Just (currentTokenUsage stStarted))
          | otherwise -> do
              let (frames, nextBuf) = splitSseFrames (buf <> chunk)
              eNextSt <- emitFrames apiKey st frames
              case eNextSt of
                Left err -> throwError err
                Right st' -> loop nextBuf st'

instance ChatModel Anthropic where
  type ModelConfig Anthropic = AnthropicConfig

  invoke provider msgs mbCfg = do
    let reqJson = buildAnthropicBody provider False mbCfg msgs
        apiKey = anthropicApiKey provider
    eReq <- liftIO $ mkHttpRequest provider False reqJson
    req <- case eReq of
      Left err -> throwError err
      Right r -> pure r
    eResp <- liftIO $ try @HttpException $ do
      mgr <- newManager tlsManagerSettings
      httpLbs req mgr
    case eResp of
      Left ex ->
        throwError $
          llmError
            (redactApiKey apiKey $ "Anthropic request error: " <> T.pack (displayException ex))
            (Just "Anthropic")
            Nothing
      Right resp ->
        if not (statusIsSuccessful (responseStatus resp))
          then throwError $ httpFailureError provider resp (responseBody resp)
          else case Aeson.eitherDecode (responseBody resp) of
            Left err ->
              throwError $
                llmError
                  (redactApiKey apiKey $ "Invalid Anthropic response JSON: " <> T.pack err)
                  (Just "Anthropic")
                  Nothing
            Right val -> case parseAnthropicResponse val of
              Left err ->
                throwError $
                  llmError
                    (redactApiKey apiKey $ "Failed to decode Anthropic message: " <> err)
                    (Just "Anthropic")
                    Nothing
              Right msg -> pure msg

  stream provider msgs mbCfg = do
    let reqJson = buildAnthropicBody provider True mbCfg msgs
    eReq <- liftIO $ mkHttpRequest provider True reqJson
    req <- case eReq of
      Left err -> throwError err
      Right r -> pure r
    mgr <- liftIO $ newManager tlsManagerSettings
    bracketP (responseOpen req mgr) responseClose (streamSseBody provider msgs)

instance ToolBinder Anthropic m where
  bindToolsConfig tools mbCfg =
    let cfg = fromMaybe defaultAnthropicConfig mbCfg
        converted =
          [ AnthropicTool
              { anthropicToolName = CoreTool.toolName t
              , anthropicToolDescription = CoreTool.toolDescription t
              , anthropicToolInputSchema = CoreTool.toolSchema t
              }
          | t <- tools
          ]
        existing = fromMaybe [] (configTools cfg)
        existingNames = map anthropicToolName existing
        merged = existing ++ filter (\t -> anthropicToolName t `notElem` existingNames) converted
     in Just $
          if null merged
            then cfg
            else cfg {configTools = Just merged}

instance CacheableChatModel Anthropic where
  cacheModelIdentity Anthropic {..} mbCfg =
    let cfg = fromMaybe defaultAnthropicConfig mbCfg
     in object
          [ "provider" .= ("anthropic" :: Text)
          , "model" .= anthropicModel
          , "baseUrl" .= anthropicBaseUrl
          , "version" .= anthropicVersion
          , "maxTokens" .= configMaxTokens cfg
          , "system" .= configSystem cfg
          , "temperature" .= configTemperature cfg
          , "thinkingBudget" .= configThinkingBudget cfg
          , "promptCaching" .= configPromptCaching cfg
          , "tools" .= configTools cfg
          ]

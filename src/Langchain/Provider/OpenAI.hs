{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
  , newOpenAI
  , openAICompatible
  , normalizeBaseUrl
  , parseOpenAIResponse
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object)
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (yield)
import qualified Data.List.NonEmpty as NonEmpty
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

import Langchain.Core.Error (llmError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..))

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
  -- ^ Base URL (e.g. @"https://api.openai.com"@). The @openai@ package
  -- automatically appends @\/v1\/chat\/completions@.
  , temperature :: Maybe Double
  }
  deriving (Eq, Show)

-- | Create standard OpenAI provider instance
newOpenAI :: Text -> Text -> OpenAI
newOpenAI key mName =
  OpenAI
    { apiKey = key
    , model = mName
    , baseUrl = "https://api.openai.com"
    , temperature = Just 0.7
    }

-- | Create OpenAICompatible provider instance for OpenRouter/Fireworks/Together.
--
-- The @endpoint@ should be the __base URL__ only (e.g.
-- @"https://openrouter.ai/api"@), not the full chat completions path.
-- The @openai@ package appends @\/v1\/chat\/completions@ automatically.
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
contentBlockToOAI (TextBlock t) = CC.Text{CC.text = t}
contentBlockToOAI (ImageBlock ImageContent{imageSource = ImageUrl url}) =
  CC.Image_URL{CC.image_url = CC.ImageURL{CC.url = url, CC.detail = Nothing}}
contentBlockToOAI (ImageBlock ImageContent{imageSource = ImageBase64 (Just mime) b64}) =
  CC.Image_URL
    { CC.image_url =
        CC.ImageURL
          { CC.url = "data:" <> mime <> ";base64," <> b64
          , CC.detail = Nothing
          }
    }
contentBlockToOAI (ImageBlock ImageContent{imageSource = ImageBase64 Nothing b64}) =
  CC.Image_URL
    { CC.image_url =
        CC.ImageURL
          { CC.url = "data:application/octet-stream;base64," <> b64
          , CC.detail = Nothing
          }
    }
contentBlockToOAI (AudioBlock _mime _b64) =
  -- Audio blocks are represented as text placeholders in the request
  CC.Text{CC.text = "[Audio content]"}
contentBlockToOAI (DataBlock _) =
  CC.Text{CC.text = "[Data block]"}

-- | Convert a langchain 'Message' to an openai package 'CC.Message'.
toLangchainOAIMessage :: Message -> CC.Message (V.Vector CC.Content)
toLangchainOAIMessage msg =
  let contents = V.fromList $ map contentBlockToOAI (NonEmpty.toList (messageContents msg))
   in case messageRole msg of
        System ->
          CC.System{CC.content = contents, CC.name = messageName msg}
        User ->
          CC.User{CC.content = contents, CC.name = messageName msg}
        Assistant ->
          CC.Assistant
            { CC.assistant_content = Just contents
            , CC.refusal = Nothing
            , CC.name = messageName msg
            , CC.assistant_audio = Nothing
            , CC.tool_calls = Nothing
            }
        Tool ->
          CC.Tool
            { CC.content = contents
            , CC.tool_call_id = maybe "" Prelude.id (messageToolId msg)
            }
        -- Developer and Function map to System for the openai package
        Developer ->
          CC.System{CC.content = contents, CC.name = messageName msg}
        Function ->
          CC.System{CC.content = contents, CC.name = messageName msg}

-- ---------------------------------------------------------------------------
-- Conversion: openai package response -> langchain-hs Message
-- ---------------------------------------------------------------------------

-- | Convert an openai package 'CC.Choice' response message to a langchain 'Message'.
fromOAIMessage :: CC.Message Text -> Message
fromOAIMessage oaiMsg = case oaiMsg of
  CC.Assistant{CC.assistant_content, CC.tool_calls = oaiToolCalls, CC.name = nm} ->
    let contentText = maybe "" Prelude.id assistant_content
        baseMsg = (assistantMessage contentText){messageName = nm}
        tcList = case oaiToolCalls of
          Nothing -> Nothing
          Just tcs ->
            Just $
              map
                ( \(OTC.ToolCall_Function{OTC.id = tcId, OTC.function = fn}) ->
                    let argVal = case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 (OTC.arguments fn))) of
                          Just v -> v
                          Nothing -> object []
                     in ToolCall tcId "function" (OTC.name fn) argVal
                )
                (V.toList tcs)
     in baseMsg{messageToolCalls = tcList}
  CC.System{CC.content = c} -> systemMessage c
  CC.User{CC.content = c} -> userMessage c
  CC.Tool{CC.content = c} ->
    (textMessage Tool c)

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
    let oaiMsgs = V.fromList $ map toLangchainOAIMessage inputMsgs
        reqBody =
          CC._CreateChatCompletion
            { CC.messages = oaiMsgs
            , CC.model = OM.Model (model provider)
            , CC.temperature = temperature provider
            }

    eRes <- liftIO $ callOpenAI provider reqBody
    case eRes of
      Left err -> throwError $ llmError err Nothing Nothing
      Right (CC.ChatCompletionObject{CC.choices = choicesVec, CC.usage = oaiUsage}) -> do
        case V.toList choicesVec of
          [] -> throwError $ llmError "Empty choices array in OpenAI response" Nothing Nothing
          (choice : _) -> do
            let respMsg = fromOAIMessage (CC.message choice)
                _usage = fromOAIUsage oaiUsage
            pure respMsg{messageToolCalls = messageToolCalls respMsg}

  stream provider inputMsgs _ = do
    let rId = "openai-stream-run"
    yield $ LLMStart rId (model provider) inputMsgs
    let oaiMsgs = V.fromList $ map toLangchainOAIMessage inputMsgs
        reqBody =
          CC._CreateChatCompletion
            { CC.messages = oaiMsgs
            , CC.model = OM.Model (model provider)
            , CC.temperature = temperature provider
            }

    eRes <- liftIO $ callOpenAI provider reqBody
    case eRes of
      Left err -> yield $ LLMChunk rId err Nothing
      Right (CC.ChatCompletionObject{CC.choices = choicesVec, CC.usage = oaiUsage}) -> do
        case V.toList choicesVec of
          [] -> yield $ LLMChunk rId "Empty choices array in OpenAI response" Nothing
          (choice : _) -> do
            let respMsg = fromOAIMessage (CC.message choice)
                mbUsage = Just $ fromOAIUsage oaiUsage
            yield $ LLMChunk rId (extractMessageText respMsg) Nothing
            yield $ LLMEnd rId respMsg mbUsage

-- | Normalize base URL to ensure compatibility with the @openai@ package.
-- Strips any trailing @/v1/chat/completions@, @/chat/completions@, or @/v1@
-- so that Servant's route constructs the expected URL path.
normalizeBaseUrl :: Text -> Text
normalizeBaseUrl rawUrl =
  let u0 = T.dropWhileEnd (== '/') rawUrl
      u1 = if "/v1/chat/completions" `T.isSuffixOf` u0
             then T.dropEnd (T.length "/v1/chat/completions") u0
             else if "/chat/completions" `T.isSuffixOf` u0
               then T.dropEnd (T.length "/chat/completions") u0
               else if "/v1" `T.isSuffixOf` u0
                 then T.dropEnd (T.length "/v1") u0
                 else u0
   in T.dropWhileEnd (== '/') u1

-- | Internal helper to perform the chat completion call via the @openai@ package.
callOpenAI :: OpenAI -> CC.CreateChatCompletion -> IO (Either Text CC.ChatCompletionObject)
callOpenAI provider reqBody = do
  eRes <- try go :: IO (Either SomeException CC.ChatCompletionObject)
  case eRes of
    Left ex -> pure $ Left (T.pack $ show ex)
    Right obj -> pure $ Right obj
  where
    go = do
      clientEnv <- OAI.getClientEnv (normalizeBaseUrl (baseUrl provider))
      let OAI.Methods{OAI.createChatCompletion} =
            OAI.makeMethods clientEnv (apiKey provider) Nothing Nothing
      createChatCompletion reqBody

-- ---------------------------------------------------------------------------
-- Backward-compatible parseOpenAIResponse
-- ---------------------------------------------------------------------------

-- | Parse a raw OpenAI JSON response 'Value' into a langchain 'Message'
-- and optional 'TokenUsage'.
--
-- This function is provided for backward compatibility. New code should use
-- the typed @openai@ package types directly.
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
      let msg = (assistantMessage contentTxt){messageToolCalls = cToolCalls}
      pure (msg, mbUsage)

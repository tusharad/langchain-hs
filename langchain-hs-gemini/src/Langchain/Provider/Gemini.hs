{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Provider.Gemini
Description : Google Gemini provider implementing ChatModel via Servant Interactions v1 API
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Gemini provider built on the Google Gemini Interactions v1 API using Servant client bindings.
Supports multi-modal content, multi-turn dialogues, tool calling, and SSE streaming.
-}
module Langchain.Provider.Gemini
  ( Gemini (..)
  , GeminiConfig (..)
  , defaultGeminiConfig
  , newGemini
  , messagesToInteractionsInput
  , interactionToMessage
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decodeStrict, object, toJSON, (.=))
import Data.Conduit (await, yield, (.|))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import Gemini.Client
  ( CreateInteractionRequest (..)
  , GenerationConfig (..)
  , Interaction (..)
  , InteractionSseEvent (..)
  , InteractionsInput (..)
  , ResponseFormat (..)
  , Step (..)
  , StepDeltaData (..)
  , Usage (..)
  , createInteraction
  , defaultCreateInteractionRequest
  , newGeminiClientEnv
  , newGeminiClientEnvWithBaseUrl
  , runGeminiClient
  , streamGeminiInteraction
  )
import qualified Gemini.Client.Types as GTypes
import Langchain.Cache.Core (CacheableChatModel (..))
import Langchain.Error (llmError)
import Langchain.Model
import Langchain.Stream (StreamEvent (..), TokenUsage (..))
import qualified Langchain.Tool as CoreTool
import Langchain.Tool.Binding (ToolBinder (..))

-- | Runtime configuration for Gemini interactions.
data GeminiConfig = GeminiConfig
  { configTools :: !(Maybe [GTypes.Tool])
  -- ^ Tools (functions, Google Search, Code Execution, etc.) available to the model.
  , configGenerationConfig :: !(Maybe GenerationConfig)
  -- ^ Generation parameters such as token limits, stop sequences, and thinking level.
  , configResponseFormat :: !(Maybe ResponseFormat)
  -- ^ Output format constraints, e.g. JSON schema enforcement.
  , configSystemInstruction :: !(Maybe Text)
  -- ^ System instructions directing the model's persona or constraints.
  , configPreviousInteractionId :: !(Maybe Text)
  -- ^ Interaction ID to continue an ongoing session.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default empty Gemini configuration.
defaultGeminiConfig :: GeminiConfig
defaultGeminiConfig =
  GeminiConfig
    { configTools = Nothing
    , configGenerationConfig = Nothing
    , configResponseFormat = Nothing
    , configSystemInstruction = Nothing
    , configPreviousInteractionId = Nothing
    }

-- | Gemini ChatModel provider instance.
data Gemini = Gemini
  { apiKey :: !Text
  , model :: !Text
  , baseUrl :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

-- | Create a new Gemini provider instance.
newGemini ::
  -- | Google Gemini API key
  Text ->
  -- | Model name (e.g. @"gemini-3.6-flash"@)
  Text ->
  -- | Optional custom base URL
  Maybe Text ->
  Gemini
newGemini = Gemini

-- | Convert Langchain ContentBlock to Gemini Content.
contentBlockToGemini :: ContentBlock -> GTypes.Content
contentBlockToGemini (TextBlock t) = GTypes.TextContent t
contentBlockToGemini (ImageBlock ImageContent {imageSource = ImageBase64 (Just mime) b64}) =
  GTypes.ImageContent mime b64
contentBlockToGemini (ImageBlock ImageContent {imageSource = ImageBase64 Nothing b64}) =
  GTypes.ImageContent "image/jpeg" b64
contentBlockToGemini (ImageBlock ImageContent {imageSource = ImageUrl url}) =
  GTypes.TextContent ("[Image URL: " <> url <> "]")
contentBlockToGemini (AudioBlock mime b64) = GTypes.AudioContent mime b64
contentBlockToGemini (DataBlock _) = GTypes.TextContent "[Data block]"

-- | Convert a Langchain Message to Gemini Step(s).
messageToSteps :: [ToolCall] -> Message -> [Step]
messageToSteps priorToolCalls msg = case messageRole msg of
  User ->
    let contents = map contentBlockToGemini (NonEmpty.toList (messageContents msg))
     in [UserInputStep contents]
  Assistant ->
    let contents = map contentBlockToGemini (NonEmpty.toList (messageContents msg))
        textContent = [c | c@(GTypes.TextContent t) <- contents, not (T.null t)]
        outputSteps = [ModelOutputStep textContent | not (null textContent)]
        toolSteps = case messageToolCalls msg of
          Nothing -> []
          Just tcs ->
            [ FunctionCallStep
                { callName = toolCallName tc
                , callArguments = toolCallArguments tc
                , callId = toolCallId tc
                }
            | tc <- tcs
            ]
     in outputSteps ++ toolSteps
  Tool ->
    let payload = parseOrWrap (extractMessageText msg)
        tId = fromMaybe "" (messageToolId msg)
        tName = messageName msg <|> lookupToolName tId priorToolCalls
     in [FunctionResultStep tId tName payload Nothing]
  Function ->
    let payload = parseOrWrap (extractMessageText msg)
        tId = fromMaybe "" (messageToolId msg)
        tName = messageName msg <|> lookupToolName tId priorToolCalls
     in [FunctionResultStep tId tName payload Nothing]
  System ->
    -- System instructions are extracted separately when possible
    let contents = map contentBlockToGemini (NonEmpty.toList (messageContents msg))
     in [UserInputStep contents]
  Developer ->
    let contents = map contentBlockToGemini (NonEmpty.toList (messageContents msg))
     in [UserInputStep contents]
  where
    lookupToolName toolId calls =
      toolCallName <$> List.find ((== toolId) . toolCallId) calls

    parseOrWrap rawTxt =
      case decodeStrict (TE.encodeUtf8 rawTxt) of
        Just val -> val
        Nothing -> object ["result" .= rawTxt]

-- | Convert a list of Langchain Messages to an 'InteractionsInput'.
messagesToInteractionsInput :: [Message] -> (Maybe Text, InteractionsInput)
messagesToInteractionsInput msgs =
  case msgs of
    [singleMsg]
      | messageRole singleMsg == User
      , [TextBlock t] <- NonEmpty.toList (messageContents singleMsg) ->
          (Nothing, TextInput t)
    _ ->
      let (sysMsgs, nonSysMsgs) = List.partition isSystem msgs
          extractedSys = case sysMsgs of
            [] -> Nothing
            _ -> Just $ T.intercalate "\n" (map extractMessageText sysMsgs)
          priorToolCalls = concatMap (fromMaybe [] . messageToolCalls) msgs
          allSteps = concatMap (messageToSteps priorToolCalls) nonSysMsgs
       in (extractedSys, StepListInput allSteps)
  where
    isSystem m = messageRole m `elem` [System, Developer]

-- | Convert an 'Interaction' result to a Langchain 'Message'.
interactionToMessage :: Interaction -> Message
interactionToMessage Interaction {..} =
  let outputTexts =
        [ t
        | ModelOutputStep cs <- interactionSteps
        , GTypes.TextContent t <- cs
        ]
      toolCalls =
        [ ToolCall
            { toolCallId = callId
            , toolCallType = "function"
            , toolCallName = callName
            , toolCallArguments = callArguments
            }
        | FunctionCallStep {..} <- interactionSteps
        ]
      thoughtSigs =
        [ sig
        | ThoughtStep {thoughtSignature = Just sig} <- interactionSteps
        ]
      base = assistantMessage (T.intercalate "\n" outputTexts)
      withCalls =
        if null toolCalls
          then base
          else base {messageToolCalls = Just toolCalls}
      withMeta =
        if null thoughtSigs
          then withCalls
          else
            withCalls
              { messageMetadata =
                  Map.insert
                    "langchain.gemini.thoughtSignatures"
                    (toJSON thoughtSigs)
                    (messageMetadata withCalls)
              }
   in withMeta

-- | Build a 'CreateInteractionRequest' from Gemini configuration and messages.
buildCreateRequest :: Gemini -> [Message] -> Maybe GeminiConfig -> CreateInteractionRequest
buildCreateRequest Gemini {..} msgs mbConfig =
  let (mbExtractedSys, input) = messagesToInteractionsInput msgs
      cfg = fromMaybe defaultGeminiConfig mbConfig
      finalSys = configSystemInstruction cfg <|> mbExtractedSys
      baseReq = defaultCreateInteractionRequest input
   in baseReq
        { reqModel = Just model
        , reqSystemInstruction = finalSys
        , reqTools = configTools cfg
        , reqGenerationConfig = configGenerationConfig cfg
        , reqResponseFormat = configResponseFormat cfg
        , reqPreviousInteractionId = configPreviousInteractionId cfg
        }

instance ChatModel Gemini where
  type ModelConfig Gemini = GeminiConfig

  invoke provider msgs mbConfig = do
    let req = buildCreateRequest provider msgs mbConfig
    env <- liftIO $ case baseUrl provider of
      Just u -> newGeminiClientEnvWithBaseUrl u (apiKey provider)
      Nothing -> newGeminiClientEnv (apiKey provider)
    eRes <- liftIO $ runGeminiClient env (createInteraction (apiKey provider) req)
    case eRes of
      Left err ->
        throwError $
          llmError (redactKey (apiKey provider) (T.pack (show err))) Nothing Nothing
      Right interaction -> pure $ interactionToMessage interaction

  stream provider msgs mbConfig = do
    let rId = "gemini-stream-run"
        mName = model provider
    yield $ LLMStart rId mName msgs

    env <- liftIO $ case baseUrl provider of
      Just u -> newGeminiClientEnvWithBaseUrl u (apiKey provider)
      Nothing -> newGeminiClientEnv (apiKey provider)
    let req = buildCreateRequest provider msgs mbConfig

    (accumulated, mbCompletedInter) <-
      streamGeminiInteraction env req .| consumeStream rId "" Nothing

    case mbCompletedInter of
      Just completedInter -> do
        let finalMsg = interactionToMessage completedInter
            usage = toTokenUsage (interactionUsage completedInter)
        yield $ LLMEnd rId finalMsg usage
      Nothing -> do
        let finalMsg = assistantMessage accumulated
        yield $ LLMEnd rId finalMsg Nothing
    where
      consumeStream rId acc mbCompleted =
        await >>= \case
          Nothing -> pure (acc, mbCompleted)
          Just (SseStepDelta _ (TextDelta txt)) -> do
            yield $ LLMChunk rId txt Nothing
            consumeStream rId (acc <> txt) mbCompleted
          Just (SseInteractionCompleted inter) ->
            consumeStream rId acc (Just inter)
          Just (SseError code msg) ->
            throwError $
              llmError (code <> ": " <> msg) Nothing Nothing
          Just _ ->
            consumeStream rId acc mbCompleted

      toTokenUsage Nothing = Nothing
      toTokenUsage (Just Usage {..}) =
        Just $
          TokenUsage
            { promptTokens = fromMaybe 0 totalInputTokens
            , completionTokens = fromMaybe 0 totalOutputTokens
            , totalTokens = fromMaybe 0 totalTokens
            }

instance ToolBinder Gemini m where
  bindToolsConfig tools mbConfig =
    let cfg = fromMaybe defaultGeminiConfig mbConfig
        geminiTools = map toGeminiTool tools
        mergedTools = case configTools cfg of
          Nothing -> Just geminiTools
          Just existing -> Just (existing ++ geminiTools)
     in Just cfg {configTools = mergedTools}
    where
      toGeminiTool t =
        GTypes.FunctionTool
          (CoreTool.toolName t)
          (Just $ CoreTool.toolDescription t)
          (Just $ CoreTool.toolSchema t)

instance CacheableChatModel Gemini where
  cacheModelIdentity Gemini {model = mName} _ =
    object
      [ "provider" .= ("gemini" :: Text)
      , "model" .= mName
      ]

-- | Redact API key from strings to prevent accidental leakage in errors.
redactKey :: Text -> Text -> Text
redactKey key txt
  | T.null key = txt
  | otherwise = T.replace key "[REDACTED]" txt

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Core.Model.Types
Description : Multi-modal ContentBlock and Message data types
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Data types for multi-modal messages, content blocks, roles, and tool calls.
-}
module Langchain.Core.Model.Types
  ( ContentBlock (..)
  , ImageContent (..)
  , ImageSource (..)
  , Role (..)
  , ToolCall (..)
  , Message (..)
  , textMessage
  , userMessage
  , systemMessage
  , assistantMessage
  , imageMessage
  , extractMessageText
  , countTokensBPE
  , roleLabel
  , formatMessageString
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | A single content block within a multi-modal message.
data ContentBlock
  = TextBlock {blockText :: Text}
  | ImageBlock ImageContent
  | AudioBlock {blockMimeType :: Text, blockBase64 :: Text}
  | DataBlock {blockBytes :: ByteString}
  deriving (Eq, Show, Generic, NFData)

data ImageContent = ImageContent
  { imageSource :: ImageSource
  , imageDetail :: Maybe Text
  , imageMetadata :: Maybe Value
  }
  deriving (Eq, Show, Generic, NFData)

data ImageSource
  = ImageBase64 {imageMimeType :: Maybe Text, imageData :: Text}
  | ImageUrl {imageUrl :: Text}
  deriving (Eq, Show, Generic, NFData)

instance ToJSON ContentBlock where
  toJSON (TextBlock t) = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ImageBlock ImageContent {imageSource = ImageUrl url, imageDetail = detail}) =
    object
      [ "type" .= ("image_url" :: Text)
      , "image_url" .= object (maybe id ((:) . ("detail" .=)) detail ["url" .= url])
      ]
  toJSON (ImageBlock ImageContent {imageSource = ImageBase64 (Just mime) imageData, imageMetadata = metadata}) =
    object $
      ["type" .= ("image" :: Text), "mime_type" .= mime, "data" .= imageData]
        <> maybe [] (pure . ("metadata" .=)) metadata
  toJSON (ImageBlock ImageContent {imageSource = ImageBase64 _ imageData, imageMetadata = metadata}) =
    object $
      ["type" .= ("image" :: Text), "source_type" .= ("base64" :: Text), "data" .= imageData]
        <> maybe [] (pure . ("metadata" .=)) metadata
  toJSON (AudioBlock mime b64) = object ["type" .= ("audio" :: Text), "mime_type" .= mime, "data" .= b64]
  toJSON (DataBlock bs) = object ["type" .= ("data" :: Text), "data" .= TE.decodeUtf8 (Base64.encode bs)]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v -> do
    typ <- v .: "type"
    case (typ :: Text) of
      "text" -> TextBlock <$> v .: "text"
      "image" -> do
        mimeType <- v .:? "mime_type"
        case mimeType of
          Just mime -> do
            imageData <- v .: "data"
            metadata <- v .:? "metadata"
            pure $ ImageBlock $ ImageContent (ImageBase64 (Just mime) imageData) Nothing metadata
          Nothing -> do
            sourceType <- v .: "source_type"
            imageData <- v .: "data"
            metadata <- v .:? "metadata"
            let source =
                  case (sourceType :: Text) of
                    "url" -> ImageUrl imageData
                    _ -> ImageBase64 Nothing imageData
            pure $ ImageBlock $ ImageContent source Nothing metadata
      "image_url" -> do
        imageUrl <- v .: "image_url"
        url <- imageUrl .: "url"
        detail <- imageUrl .:? "detail"
        pure $ ImageBlock $ ImageContent (ImageUrl url) detail Nothing
      "audio" -> AudioBlock <$> v .: "mime_type" <*> v .: "data"
      "data" -> do
        b64Text <- v .: "data"
        case Base64.decode (TE.encodeUtf8 b64Text) of
          Left err -> fail $ "Invalid base64 data block: " ++ err
          Right bs -> pure $ DataBlock bs
      other -> fail $ "Unknown ContentBlock type: " ++ show other

-- | Complete set of conversation roles supported across LLM providers.
data Role
  = System
  | User
  | Assistant
  | Tool
  | Developer
  | Function
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, ToJSON, FromJSON, NFData)

-- | Structured tool call from an LLM response.
data ToolCall = ToolCall
  { toolCallId :: Text
  , toolCallType :: Text
  -- ^ Always "function" for current providers
  , toolCallName :: Text
  , toolCallArguments :: Value
  -- ^ Parsed JSON Value arguments
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, NFData)

-- | Structured chat message supporting multi-modal content blocks.
data Message = Message
  { messageRole :: Role
  , messageContents :: NonEmpty ContentBlock
  , messageName :: Maybe Text
  , messageToolCalls :: Maybe [ToolCall]
  , messageToolId :: Maybe Text
  -- ^ Associated tool call ID for Tool role
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, NFData)

-- | Create a message with a single text content block.
textMessage :: Role -> Text -> Message
textMessage r t = Message r (TextBlock t :| []) Nothing Nothing Nothing

-- | Create a User role text message.
userMessage :: Text -> Message
userMessage = textMessage User

-- | Create a System role text message.
systemMessage :: Text -> Message
systemMessage = textMessage System

-- | Create an Assistant role text message.
assistantMessage :: Text -> Message
assistantMessage = textMessage Assistant

-- | Create an Image content block message.
imageMessage :: Role -> Text -> Text -> Message
imageMessage r mime b64 =
  Message
    r
    (ImageBlock (ImageContent (ImageBase64 (Just mime) b64) Nothing Nothing) :| [])
    Nothing
    Nothing
    Nothing

-- | Extract all text content blocks concatenated into a single Text string.
extractMessageText :: Message -> Text
extractMessageText msg = T.intercalate "\n" [t | TextBlock t <- NonEmpty.toList (messageContents msg)]

-- | Label used when rendering a chat message role as plain text.
roleLabel :: Role -> Text
roleLabel System = "System"
roleLabel User = "Human"
roleLabel Assistant = "AI"
roleLabel Tool = "Tool"
roleLabel Developer = "Developer"
roleLabel Function = "Function"

-- | Render a message as a single plain-text line with its role label.
formatMessageString :: Message -> Text
formatMessageString chatMessage =
  roleLabel (messageRole chatMessage) <> ": " <> extractMessageText chatMessage

-- | Count approximate BPE tokens for a Text string (roughly 4 characters per token or word-based heuristic).
countTokensBPE :: Text -> Int
countTokensBPE t
  | T.null t = 0
  | otherwise = max 1 (ceiling (fromIntegral (T.length t) / (4.0 :: Double)))

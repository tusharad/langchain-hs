{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.PromptTemplate.Chat.ChatPromptTemplate
Description : ChatPromptTemplate prompt template
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Langchain.PromptTemplate.Chat.ChatPromptTemplate
  ( ChatPromptTemplate (..)
  , ChatPromptMessage
  , ContentPromptBlock (..)
  , ChatPromptInput (..)
  , ChatPromptValue (..)
  , PartialValue (..)
  , fromTemplate
  , fromTemplateWithOptions
  , fromMessages
  , message
  , templateMessage
  , templateMessageWithFormat
  , contentMessage
  , messagesPlaceholder
  , messagesPlaceholderWithOptions
  , append
  , extend
  , partial
  , invoke
  , formatPrompt
  , format
  , toMessages
  , toString
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Either (fromRight)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError, validationError)
import Langchain.Core.Model.Types
  ( ContentBlock (..)
  , ImageContent (..)
  , ImageSource (..)
  , Message (..)
  , Role (..)
  , formatMessageString
  , textMessage
  , userMessage
  )
import Langchain.PromptTemplate.Chat (BaseMessagePromptTemplate (formatMessages))
import Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate (HumanMessagePromptTemplate (..))
import Langchain.PromptTemplate.Chat.MessagesPlaceholder
  ( MessagesPlaceholder (..)
  , messagesPlaceholderVariableName
  )
import qualified Langchain.PromptTemplate.Chat.MessagesPlaceholder as MessagesPlaceholder
import Langchain.PromptTemplate.Prompt (PromptTemplateOptions)
import qualified Langchain.PromptTemplate.Prompt as Prompt
import Langchain.PromptTemplate.String (TemplateFormat (..))
import qualified Langchain.PromptTemplate.String as String

-- | A single chat message template inside a chat prompt.
data ChatPromptMessage
  = HumanMessagePrompt HumanMessagePromptTemplate
  | SystemMessagePrompt Prompt.PromptTemplate
  | AIMessagePrompt Prompt.PromptTemplate
  | ChatMessagePrompt Role Prompt.PromptTemplate
  | ContentMessagePrompt Role [ContentPromptBlock]
  | MessagesPlaceholderPrompt MessagesPlaceholder (Maybe [Message])
  | StaticMessage Message
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A templated block inside a multipart chat message.
data ContentPromptBlock
  = TextPromptBlock TemplateFormat Text
  | ImagePromptBlock TemplateFormat ImageContent
  deriving (Show, Eq, Generic)

instance ToJSON ContentPromptBlock where
  toJSON (TextPromptBlock templateFormat template) =
    object
      [ "type" .= ("text_prompt" :: Text)
      , "templateFormat" .= templateFormat
      , "template" .= template
      ]
  toJSON (ImagePromptBlock templateFormat imageContent) =
    object
      [ "type" .= ("image_prompt" :: Text)
      , "templateFormat" .= templateFormat
      , "imageContent" .= imageContentToJSON imageContent
      ]

instance FromJSON ContentPromptBlock where
  parseJSON = withObject "ContentPromptBlock" $ \value -> do
    blockType <- value .: "type"
    case (blockType :: Text) of
      "text_prompt" -> TextPromptBlock <$> value .: "templateFormat" <*> value .: "template"
      "image_prompt" ->
        ImagePromptBlock <$> value .: "templateFormat" <*> (value .: "imageContent" >>= parseImageContent)
      other -> fail $ "Unknown ContentPromptBlock type: " ++ show other

imageContentToJSON :: ImageContent -> Value
imageContentToJSON ImageContent {imageSource = source, imageDetail = detail, imageMetadata = metadata} =
  object
    [ "source" .= imageSourceToJSON source
    , "detail" .= detail
    , "metadata" .= metadata
    ]

imageSourceToJSON :: ImageSource -> Value
imageSourceToJSON (ImageBase64 mime sourceData) =
  object
    [ "type" .= ("base64" :: Text)
    , "mimeType" .= mime
    , "data" .= sourceData
    ]
imageSourceToJSON (ImageUrl url) =
  object
    [ "type" .= ("url" :: Text)
    , "url" .= url
    ]

parseImageContent :: Value -> Parser ImageContent
parseImageContent = withObject "ImageContent" $ \value ->
  ImageContent
    <$> (value .: "source" >>= parseImageSource)
    <*> value .:? "detail"
    <*> value .:? "metadata"

parseImageSource :: Value -> Parser ImageSource
parseImageSource = withObject "ImageSource" $ \value -> do
  sourceType <- value .: "type"
  case (sourceType :: Text) of
    "base64" -> ImageBase64 <$> value .:? "mimeType" <*> value .: "data"
    "url" -> ImageUrl <$> value .: "url"
    other -> fail $ "Unknown ImageSource type: " ++ show other

-- | A chat prompt template made of ordered message templates.
data ChatPromptTemplate = ChatPromptTemplate
  { messages :: [ChatPromptMessage]
  , inputVariables :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A rendered chat prompt as concrete messages.
newtype ChatPromptValue = ChatPromptValue
  { messages :: [Message]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Inputs accepted by 'invoke' for chat prompts.
data ChatPromptInput
  = ChatPromptVariables (Map.Map Text Text)
  | ChatPromptMessageList [Message]
  | ChatPromptInputs (Map.Map Text Text) (Map.Map Text [Message])
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Partial values that can pre-bind text or message placeholders.
data PartialValue
  = PartialText Text
  | PartialMessages [Message]
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Create a single-message user chat prompt from raw text.
fromTemplate :: Text -> ChatPromptTemplate
fromTemplate template = fromTemplateWithOptions template Prompt.defaultPromptTemplateOptions

-- | Create a single-message user chat prompt with partial variables.
fromTemplateWithOptions :: Text -> PromptTemplateOptions -> ChatPromptTemplate
fromTemplateWithOptions template options =
  let promptTemplate = Prompt.fromTemplateWithOptions template options
   in ChatPromptTemplate
        { messages = [ChatMessagePrompt User promptTemplate]
        , inputVariables = Prompt.inputVariables promptTemplate
        }

-- | Create a chat prompt from an explicit list of message templates.
fromMessages :: [ChatPromptMessage] -> ChatPromptTemplate
fromMessages promptMessages =
  ChatPromptTemplate
    { messages = promptMessages
    , inputVariables = unique $ concatMap messageInputVariables promptMessages
    }

-- | Wrap a concrete message as part of a chat prompt.
message :: Message -> ChatPromptMessage
message = StaticMessage

-- | Create a templated message for a specific role.
templateMessage :: Role -> Text -> ChatPromptMessage
templateMessage role = ChatMessagePrompt role . Prompt.fromTemplate

-- | Create a templated message for a specific role and template format.
templateMessageWithFormat :: Role -> TemplateFormat -> Text -> ChatPromptMessage
templateMessageWithFormat role templateFormat template =
  ChatMessagePrompt role $
    Prompt.fromTemplateWithFormat template templateFormat Map.empty

-- | Create a multipart content message for a specific role.
contentMessage :: Role -> [ContentPromptBlock] -> ChatPromptMessage
contentMessage = ContentMessagePrompt

-- | Create a placeholder for an injected message list.
messagesPlaceholder :: Text -> ChatPromptMessage
messagesPlaceholder name = messagesPlaceholderWithOptions $ MessagesPlaceholder.messagesPlaceholderOptions name

-- | Create a message-list placeholder with explicit options.
messagesPlaceholderWithOptions ::
  MessagesPlaceholder.MessagesPlaceholderOptions -> ChatPromptMessage
messagesPlaceholderWithOptions options =
  MessagesPlaceholderPrompt (MessagesPlaceholder.messagesPlaceholderWithOptions options) Nothing

-- | Append one message template to the end of a chat prompt.
append :: ChatPromptTemplate -> ChatPromptMessage -> ChatPromptTemplate
append chatPromptTemplate promptMessage = extend chatPromptTemplate [promptMessage]

-- | Append multiple message templates to the end of a chat prompt.
extend :: ChatPromptTemplate -> [ChatPromptMessage] -> ChatPromptTemplate
extend ChatPromptTemplate {messages = promptMessages} newMessages =
  fromMessages $ promptMessages <> newMessages

-- | Apply partial text and message bindings to a chat prompt.
partial :: ChatPromptTemplate -> Map.Map Text PartialValue -> ChatPromptTemplate
partial ChatPromptTemplate {messages = promptMessages} partialVariables =
  fromMessages $ map (`partialMessage` partialVariables) promptMessages

-- | Render a chat prompt to concrete messages.
formatPrompt :: ChatPromptTemplate -> Map.Map Text Text -> Either LangchainError ChatPromptValue
formatPrompt ChatPromptTemplate {messages = promptMessages} variables =
  formatPromptWithMessages promptMessages variables Map.empty

-- | Render a chat prompt with either variables or message-list inputs.
invoke :: ChatPromptTemplate -> ChatPromptInput -> Either LangchainError ChatPromptValue
invoke chatPromptTemplate (ChatPromptVariables variables) = formatPrompt chatPromptTemplate variables
invoke ChatPromptTemplate {messages = [MessagesPlaceholderPrompt placeholder _]} (ChatPromptMessageList promptMessages) =
  ChatPromptValue
    <$> formatMessages
      placeholder
      (Map.singleton (messagesPlaceholderVariableName placeholder) promptMessages)
invoke ChatPromptTemplate {messages = promptMessages} (ChatPromptInputs variables messageVariables) =
  formatPromptWithMessages promptMessages variables messageVariables
invoke _ (ChatPromptMessageList _) =
  Left $
    validationError
      "List input is only supported for a single MessagesPlaceholder"
      (Just "ChatPromptTemplate")
      (Just "invoke")

-- | Render a chat prompt to a single formatted text value.
format :: ChatPromptTemplate -> Map.Map Text Text -> Either LangchainError Text
format chatPromptTemplate variables = toString <$> formatPrompt chatPromptTemplate variables

-- | Extract the concrete messages from a rendered chat prompt.
toMessages :: ChatPromptValue -> [Message]
toMessages (ChatPromptValue promptMessages) = promptMessages

-- | Render a chat prompt as newline-separated message text.
toString :: ChatPromptValue -> Text
toString (ChatPromptValue promptMessages) =
  T.intercalate "\n" $ map formatMessageString promptMessages

messageInputVariables :: ChatPromptMessage -> [Text]
messageInputVariables (HumanMessagePrompt promptMessage) = Prompt.inputVariables . prompt $ promptMessage
messageInputVariables (SystemMessagePrompt promptTemplate) = Prompt.inputVariables promptTemplate
messageInputVariables (AIMessagePrompt promptTemplate) = Prompt.inputVariables promptTemplate
messageInputVariables (ChatMessagePrompt _ promptTemplate) = Prompt.inputVariables promptTemplate
messageInputVariables (ContentMessagePrompt _ blocks) = unique $ concatMap contentBlockInputVariables blocks
messageInputVariables
  ( MessagesPlaceholderPrompt
      MessagesPlaceholder
        { variableName = variableName'
        , optional = optional'
        }
      storedMessages
    )
    | optional' || isJust storedMessages = []
    | otherwise = [variableName']
messageInputVariables (StaticMessage _) = []

partialMessage :: ChatPromptMessage -> Map.Map Text PartialValue -> ChatPromptMessage
partialMessage (HumanMessagePrompt HumanMessagePromptTemplate {prompt = promptTemplate}) partialVariables =
  HumanMessagePrompt $
    HumanMessagePromptTemplate
      { prompt = Prompt.partialPromptTemplate promptTemplate (textPartialVariables partialVariables)
      }
partialMessage (SystemMessagePrompt promptTemplate) partialVariables =
  SystemMessagePrompt $
    Prompt.partialPromptTemplate promptTemplate (textPartialVariables partialVariables)
partialMessage (AIMessagePrompt promptTemplate) partialVariables =
  AIMessagePrompt $
    Prompt.partialPromptTemplate promptTemplate (textPartialVariables partialVariables)
partialMessage (ChatMessagePrompt role promptTemplate) partialVariables =
  ChatMessagePrompt role $
    Prompt.partialPromptTemplate promptTemplate (textPartialVariables partialVariables)
partialMessage (ContentMessagePrompt role blocks) partialVariables =
  ContentMessagePrompt role $
    map (\block -> partialContentBlock block (textPartialVariables partialVariables)) blocks
partialMessage (MessagesPlaceholderPrompt placeholder storedMessages) partialVariables =
  MessagesPlaceholderPrompt placeholder $
    case Map.lookup (messagesPlaceholderVariableName placeholder) partialVariables of
      Just (PartialMessages promptMessages) -> Just promptMessages
      _ -> storedMessages
partialMessage (StaticMessage staticMessage) _ = StaticMessage staticMessage

textPartialVariables :: Map.Map Text PartialValue -> Map.Map Text Text
textPartialVariables = Map.mapMaybe toText
  where
    toText :: PartialValue -> Maybe Text
    toText (PartialText value) = Just value
    toText (PartialMessages _) = Nothing

formatPromptWithMessages ::
  [ChatPromptMessage] ->
  Map.Map Text Text ->
  Map.Map Text [Message] ->
  Either LangchainError ChatPromptValue
formatPromptWithMessages promptMessages variables messageVariables =
  ChatPromptValue . concat
    <$> traverse (\promptMessage -> formatMessage promptMessage variables messageVariables) promptMessages

formatMessage ::
  ChatPromptMessage -> Map.Map Text Text -> Map.Map Text [Message] -> Either LangchainError [Message]
formatMessage (HumanMessagePrompt promptMessage) variables _ =
  (: []) . userMessage <$> Prompt.renderPrompt (prompt promptMessage) variables
formatMessage (SystemMessagePrompt promptTemplate) variables _ =
  (: []) . textMessage System <$> Prompt.renderPrompt promptTemplate variables
formatMessage (AIMessagePrompt promptTemplate) variables _ =
  (: []) . textMessage Assistant <$> Prompt.renderPrompt promptTemplate variables
formatMessage (ChatMessagePrompt role promptTemplate) variables _ =
  (: []) . textMessage role <$> Prompt.renderPrompt promptTemplate variables
formatMessage (ContentMessagePrompt role blocks) variables _ = do
  renderedBlocks <- concat <$> traverse (renderContentBlock variables) blocks
  case NonEmpty.nonEmpty renderedBlocks of
    Nothing -> Right []
    Just nonEmptyBlocks -> Right [Message role nonEmptyBlocks Nothing Nothing Nothing]
formatMessage (MessagesPlaceholderPrompt placeholder storedMessages) _ messageVariables =
  formatMessages placeholder $
    case storedMessages of
      Nothing -> messageVariables
      Just promptMessages ->
        messageVariables
          `Map.union` Map.singleton (messagesPlaceholderVariableName placeholder) promptMessages
formatMessage (StaticMessage staticMessage) _ _ = Right [staticMessage]

contentBlockInputVariables :: ContentPromptBlock -> [Text]
contentBlockInputVariables (TextPromptBlock templateFormat template) =
  String.extractTemplateVariablesWithFormat templateFormat template
contentBlockInputVariables (ImagePromptBlock templateFormat imageContent) =
  imageContentInputVariables templateFormat imageContent

partialContentBlock :: ContentPromptBlock -> Map.Map Text Text -> ContentPromptBlock
partialContentBlock (TextPromptBlock templateFormat template) partials =
  TextPromptBlock templateFormat $ renderPartial templateFormat partials template
partialContentBlock (ImagePromptBlock templateFormat imageContent) partials =
  ImagePromptBlock templateFormat $ partialImageContent templateFormat partials imageContent

renderContentBlock ::
  Map.Map Text Text -> ContentPromptBlock -> Either LangchainError [ContentBlock]
renderContentBlock variables (TextPromptBlock templateFormat template) = do
  rendered <- renderTemplate templateFormat variables template
  pure [TextBlock rendered | not (T.null rendered)]
renderContentBlock variables (ImagePromptBlock templateFormat imageContent) = do
  renderedImage <- renderImageContent templateFormat variables imageContent
  pure [ImageBlock renderedImage]

imageContentInputVariables :: TemplateFormat -> ImageContent -> [Text]
imageContentInputVariables templateFormat ImageContent {imageSource = source, imageDetail = detail, imageMetadata = metadata} =
  imageSourceInputVariables templateFormat source
    <> maybe [] (String.extractTemplateVariablesWithFormat templateFormat) detail
    <> maybe [] (valueInputVariables templateFormat) metadata

imageSourceInputVariables :: TemplateFormat -> ImageSource -> [Text]
imageSourceInputVariables templateFormat (ImageBase64 _ imageTemplate) =
  String.extractTemplateVariablesWithFormat templateFormat imageTemplate
imageSourceInputVariables templateFormat (ImageUrl url) =
  String.extractTemplateVariablesWithFormat templateFormat url

partialImageContent :: TemplateFormat -> Map.Map Text Text -> ImageContent -> ImageContent
partialImageContent templateFormat partials ImageContent {imageSource = source, imageDetail = detail, imageMetadata = metadata} =
  ImageContent
    { imageSource = partialImageSource templateFormat partials source
    , imageDetail = renderPartial templateFormat partials <$> detail
    , imageMetadata = renderPartialValue templateFormat partials <$> metadata
    }

partialImageSource :: TemplateFormat -> Map.Map Text Text -> ImageSource -> ImageSource
partialImageSource templateFormat partials (ImageBase64 mime imageTemplate) =
  ImageBase64 mime $ renderPartial templateFormat partials imageTemplate
partialImageSource templateFormat partials (ImageUrl url) =
  ImageUrl $ renderPartial templateFormat partials url

renderImageContent ::
  TemplateFormat -> Map.Map Text Text -> ImageContent -> Either LangchainError ImageContent
renderImageContent Jinja2 _ ImageContent {imageSource = ImageBase64 _ _} =
  Left $
    validationError
      "Jinja2 is not supported for image data prompt blocks"
      (Just "ChatPromptTemplate")
      (Just "contentMessage")
renderImageContent templateFormat variables ImageContent {imageSource = source, imageDetail = detail, imageMetadata = metadata} = do
  renderedSource <- renderImageSource templateFormat variables source
  renderedDetail <- traverse (renderTemplate templateFormat variables) detail
  renderedMetadata <- traverse (renderValue templateFormat variables) metadata
  pure $ ImageContent renderedSource renderedDetail renderedMetadata

renderImageSource ::
  TemplateFormat -> Map.Map Text Text -> ImageSource -> Either LangchainError ImageSource
renderImageSource templateFormat variables (ImageBase64 mime imageTemplate) =
  ImageBase64 mime <$> renderTemplate templateFormat variables imageTemplate
renderImageSource templateFormat variables (ImageUrl url) =
  ImageUrl <$> renderTemplate templateFormat variables url

renderTemplate :: TemplateFormat -> Map.Map Text Text -> Text -> Either LangchainError Text
renderTemplate templateFormat variables template =
  Prompt.renderPrompt
    (Prompt.fromTemplateWithFormat template templateFormat Map.empty)
    variables

renderPartial :: TemplateFormat -> Map.Map Text Text -> Text -> Text
renderPartial templateFormat partials template =
  fromRight template $ renderTemplate templateFormat partials template

valueInputVariables :: TemplateFormat -> Value -> [Text]
valueInputVariables templateFormat (String value) =
  String.extractTemplateVariablesWithFormat templateFormat value
valueInputVariables templateFormat (Array values) =
  concatMap (valueInputVariables templateFormat) values
valueInputVariables templateFormat (Object objectValue) =
  concatMap (valueInputVariables templateFormat) objectValue
valueInputVariables _ _ = []

renderValue :: TemplateFormat -> Map.Map Text Text -> Value -> Either LangchainError Value
renderValue templateFormat variables (String value) =
  String <$> renderTemplate templateFormat variables value
renderValue templateFormat variables (Array values) =
  Array <$> traverse (renderValue templateFormat variables) values
renderValue templateFormat variables (Object objectValue) =
  Object <$> traverse (renderValue templateFormat variables) objectValue
renderValue _ _ value = Right value

renderPartialValue :: TemplateFormat -> Map.Map Text Text -> Value -> Value
renderPartialValue templateFormat partials value =
  fromRight value $ renderValue templateFormat partials value

unique :: [Text] -> [Text]
unique = foldl addIfMissing []
  where
    addIfMissing :: [Text] -> Text -> [Text]
    addIfMissing variableNames name
      | name `elem` variableNames = variableNames
      | otherwise = variableNames <> [name]

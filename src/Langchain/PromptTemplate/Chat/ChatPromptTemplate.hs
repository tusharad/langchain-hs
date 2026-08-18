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
  , ChatPromptInput (..)
  , ChatPromptValue (..)
  , PartialValue (..)
  , fromTemplate
  , fromTemplateWithOptions
  , fromMessages
  , message
  , templateMessage
  , templateMessageWithFormat
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

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, validationError)
import Langchain.Core.Model.Types
  ( Message (..)
  , Role (..)
  , formatMessageString
  , textMessage
  , userMessage
  )
import Langchain.PromptTemplate (PromptTemplateOptions, TemplateFormat)
import qualified Langchain.PromptTemplate as PromptTemplate
import Langchain.PromptTemplate.Chat (BaseMessagePromptTemplate (formatMessages))
import Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate (HumanMessagePromptTemplate (..))
import Langchain.PromptTemplate.Chat.MessagesPlaceholder
  ( MessagesPlaceholder (..)
  , messagesPlaceholderVariableName
  )
import qualified Langchain.PromptTemplate.Chat.MessagesPlaceholder as MessagesPlaceholder

data ChatPromptMessage
  = HumanMessagePrompt HumanMessagePromptTemplate
  | SystemMessagePrompt PromptTemplate.PromptTemplate
  | AIMessagePrompt PromptTemplate.PromptTemplate
  | ChatMessagePrompt Role PromptTemplate.PromptTemplate
  | MessagesPlaceholderPrompt MessagesPlaceholder (Maybe [Message])
  | StaticMessage Message
  deriving (Show, Eq)

data ChatPromptTemplate = ChatPromptTemplate
  { messages :: [ChatPromptMessage]
  , inputVariables :: [Text]
  }
  deriving (Show, Eq)

newtype ChatPromptValue = ChatPromptValue
  { messages :: [Message]
  }
  deriving (Show, Eq)

data ChatPromptInput
  = ChatPromptVariables (Map.Map Text Text)
  | ChatPromptMessageList [Message]
  | ChatPromptInputs (Map.Map Text Text) (Map.Map Text [Message])
  deriving (Show, Eq)

data PartialValue
  = PartialText Text
  | PartialMessages [Message]
  deriving (Show, Eq)

fromTemplate :: Text -> ChatPromptTemplate
fromTemplate template = fromTemplateWithOptions template PromptTemplate.defaultPromptTemplateOptions

fromTemplateWithOptions :: Text -> PromptTemplateOptions -> ChatPromptTemplate
fromTemplateWithOptions template options =
  let promptTemplate = PromptTemplate.fromTemplateWithOptions template options
   in ChatPromptTemplate
        { messages = [ChatMessagePrompt User promptTemplate]
        , inputVariables = PromptTemplate.inputVariables promptTemplate
        }

fromMessages :: [ChatPromptMessage] -> ChatPromptTemplate
fromMessages promptMessages =
  ChatPromptTemplate
    { messages = promptMessages
    , inputVariables = unique $ concatMap messageInputVariables promptMessages
    }

message :: Message -> ChatPromptMessage
message = StaticMessage

templateMessage :: Role -> Text -> ChatPromptMessage
templateMessage role = ChatMessagePrompt role . PromptTemplate.fromTemplate

templateMessageWithFormat :: Role -> TemplateFormat -> Text -> ChatPromptMessage
templateMessageWithFormat role templateFormat template =
  ChatMessagePrompt role $
    PromptTemplate.fromTemplateWithFormat template templateFormat Map.empty

messagesPlaceholder :: Text -> ChatPromptMessage
messagesPlaceholder name = messagesPlaceholderWithOptions $ MessagesPlaceholder.messagesPlaceholderOptions name

messagesPlaceholderWithOptions ::
  MessagesPlaceholder.MessagesPlaceholderOptions -> ChatPromptMessage
messagesPlaceholderWithOptions options =
  MessagesPlaceholderPrompt (MessagesPlaceholder.messagesPlaceholderWithOptions options) Nothing

append :: ChatPromptTemplate -> ChatPromptMessage -> ChatPromptTemplate
append chatPromptTemplate promptMessage = extend chatPromptTemplate [promptMessage]

extend :: ChatPromptTemplate -> [ChatPromptMessage] -> ChatPromptTemplate
extend ChatPromptTemplate {messages = promptMessages} newMessages =
  fromMessages $ promptMessages <> newMessages

partial :: ChatPromptTemplate -> Map.Map Text PartialValue -> ChatPromptTemplate
partial ChatPromptTemplate {messages = promptMessages} partialVariables =
  fromMessages $ map (`partialMessage` partialVariables) promptMessages

formatPrompt :: ChatPromptTemplate -> Map.Map Text Text -> Either LangchainError ChatPromptValue
formatPrompt ChatPromptTemplate {messages = promptMessages} variables =
  formatPromptWithMessages promptMessages variables Map.empty

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

format :: ChatPromptTemplate -> Map.Map Text Text -> Either LangchainError Text
format chatPromptTemplate variables = toString <$> formatPrompt chatPromptTemplate variables

toMessages :: ChatPromptValue -> [Message]
toMessages (ChatPromptValue promptMessages) = promptMessages

toString :: ChatPromptValue -> Text
toString (ChatPromptValue promptMessages) =
  T.intercalate "\n" $ map formatMessageString promptMessages

messageInputVariables :: ChatPromptMessage -> [Text]
messageInputVariables (HumanMessagePrompt promptMessage) = PromptTemplate.inputVariables . prompt $ promptMessage
messageInputVariables (SystemMessagePrompt promptTemplate) = PromptTemplate.inputVariables promptTemplate
messageInputVariables (AIMessagePrompt promptTemplate) = PromptTemplate.inputVariables promptTemplate
messageInputVariables (ChatMessagePrompt _ promptTemplate) = PromptTemplate.inputVariables promptTemplate
messageInputVariables
  ( MessagesPlaceholderPrompt
      MessagesPlaceholder
        { variableName = variableName'
        , optional = optional'
        }
      storedMessages
    )
    | optional' || maybe False (const True) storedMessages = []
    | otherwise = [variableName']
messageInputVariables (StaticMessage _) = []

partialMessage :: ChatPromptMessage -> Map.Map Text PartialValue -> ChatPromptMessage
partialMessage (HumanMessagePrompt HumanMessagePromptTemplate {prompt = promptTemplate}) partialVariables =
  HumanMessagePrompt $
    HumanMessagePromptTemplate
      { prompt = PromptTemplate.partialPromptTemplate promptTemplate (textPartialVariables partialVariables)
      }
partialMessage (SystemMessagePrompt promptTemplate) partialVariables =
  SystemMessagePrompt $
    PromptTemplate.partialPromptTemplate promptTemplate (textPartialVariables partialVariables)
partialMessage (AIMessagePrompt promptTemplate) partialVariables =
  AIMessagePrompt $
    PromptTemplate.partialPromptTemplate promptTemplate (textPartialVariables partialVariables)
partialMessage (ChatMessagePrompt role promptTemplate) partialVariables =
  ChatMessagePrompt role $
    PromptTemplate.partialPromptTemplate promptTemplate (textPartialVariables partialVariables)
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
  (: []) . userMessage <$> PromptTemplate.renderPrompt (prompt promptMessage) variables
formatMessage (SystemMessagePrompt promptTemplate) variables _ =
  (: []) . textMessage System <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (AIMessagePrompt promptTemplate) variables _ =
  (: []) . textMessage Assistant <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (ChatMessagePrompt role promptTemplate) variables _ =
  (: []) . textMessage role <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (MessagesPlaceholderPrompt placeholder storedMessages) _ messageVariables =
  formatMessages placeholder $
    case storedMessages of
      Nothing -> messageVariables
      Just promptMessages ->
        messageVariables
          `Map.union` Map.singleton (messagesPlaceholderVariableName placeholder) promptMessages
formatMessage (StaticMessage staticMessage) _ _ = Right [staticMessage]

unique :: [Text] -> [Text]
unique = foldl addIfMissing []
  where
    addIfMissing :: [Text] -> Text -> [Text]
    addIfMissing variableNames name
      | name `elem` variableNames = variableNames
      | otherwise = variableNames <> [name]

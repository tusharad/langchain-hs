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
  , fromTemplate
  , fromTemplateWithOptions
  , fromMessages
  , message
  , templateMessage
  , messagesPlaceholder
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
  , extractMessageText
  , textMessage
  , userMessage
  )
import Langchain.PromptTemplate (PromptTemplateOptions)
import qualified Langchain.PromptTemplate as PromptTemplate
import Langchain.PromptTemplate.Chat (BaseMessagePromptTemplate (formatMessages))
import Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate (HumanMessagePromptTemplate (..))
import Langchain.PromptTemplate.Chat.MessagesPlaceholder
  ( MessagesPlaceholder (..)
  )
import qualified Langchain.PromptTemplate.Chat.MessagesPlaceholder as MessagesPlaceholder

data ChatPromptMessage
  = HumanMessagePrompt HumanMessagePromptTemplate
  | SystemMessagePrompt PromptTemplate.PromptTemplate
  | AIMessagePrompt PromptTemplate.PromptTemplate
  | ChatMessagePrompt Role PromptTemplate.PromptTemplate
  | MessagesPlaceholderPrompt MessagesPlaceholder
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

messagesPlaceholder :: Text -> ChatPromptMessage
messagesPlaceholder = MessagesPlaceholderPrompt . MessagesPlaceholder.messagesPlaceholder

append :: ChatPromptTemplate -> ChatPromptMessage -> ChatPromptTemplate
append chatPromptTemplate promptMessage = extend chatPromptTemplate [promptMessage]

extend :: ChatPromptTemplate -> [ChatPromptMessage] -> ChatPromptTemplate
extend ChatPromptTemplate {messages = promptMessages} newMessages =
  fromMessages $ promptMessages <> newMessages

partial :: ChatPromptTemplate -> Map.Map Text Text -> ChatPromptTemplate
partial ChatPromptTemplate {messages = promptMessages} partialVariables =
  fromMessages $ map (`partialMessage` partialVariables) promptMessages

formatPrompt :: ChatPromptTemplate -> Map.Map Text Text -> Either LangchainError ChatPromptValue
formatPrompt ChatPromptTemplate {messages = promptMessages} variables =
  ChatPromptValue . concat <$> traverse (`formatMessage` variables) promptMessages

invoke :: ChatPromptTemplate -> ChatPromptInput -> Either LangchainError ChatPromptValue
invoke chatPromptTemplate (ChatPromptVariables variables) = formatPrompt chatPromptTemplate variables
invoke ChatPromptTemplate {messages = [MessagesPlaceholderPrompt placeholder]} (ChatPromptMessageList promptMessages) =
  ChatPromptValue
    <$> formatMessages
      placeholder
      (Map.singleton (variableName placeholder) promptMessages)
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
    )
    | optional' = []
    | otherwise = [variableName']
messageInputVariables (StaticMessage _) = []

partialMessage :: ChatPromptMessage -> Map.Map Text Text -> ChatPromptMessage
partialMessage (HumanMessagePrompt HumanMessagePromptTemplate {prompt = promptTemplate}) partialVariables =
  HumanMessagePrompt $
    HumanMessagePromptTemplate
      { prompt = partialPromptTemplate promptTemplate partialVariables
      }
partialMessage (SystemMessagePrompt promptTemplate) partialVariables =
  SystemMessagePrompt $ partialPromptTemplate promptTemplate partialVariables
partialMessage (AIMessagePrompt promptTemplate) partialVariables =
  AIMessagePrompt $ partialPromptTemplate promptTemplate partialVariables
partialMessage (ChatMessagePrompt role promptTemplate) partialVariables =
  ChatMessagePrompt role $ partialPromptTemplate promptTemplate partialVariables
partialMessage (MessagesPlaceholderPrompt placeholder) _ = MessagesPlaceholderPrompt placeholder
partialMessage (StaticMessage staticMessage) _ = StaticMessage staticMessage

partialPromptTemplate ::
  PromptTemplate.PromptTemplate -> Map.Map Text Text -> PromptTemplate.PromptTemplate
partialPromptTemplate (PromptTemplate.PromptTemplate template _ existingPartials) partialVariables =
  PromptTemplate.fromTemplateWithOptions template $
    PromptTemplate.PromptTemplateOptions (partialVariables `Map.union` existingPartials)

formatMessage :: ChatPromptMessage -> Map.Map Text Text -> Either LangchainError [Message]
formatMessage (HumanMessagePrompt promptMessage) variables =
  (: []) . userMessage <$> PromptTemplate.renderPrompt (prompt promptMessage) variables
formatMessage (SystemMessagePrompt promptTemplate) variables =
  (: []) . textMessage System <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (AIMessagePrompt promptTemplate) variables =
  (: []) . textMessage Assistant <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (ChatMessagePrompt role promptTemplate) variables =
  (: []) . textMessage role <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (MessagesPlaceholderPrompt placeholder) _ =
  formatMessages placeholder (Map.empty :: Map.Map Text [Message])
formatMessage (StaticMessage staticMessage) _ = Right [staticMessage]

formatMessageString :: Message -> Text
formatMessageString chatMessage =
  roleLabel (messageRole chatMessage) <> ": " <> extractMessageText chatMessage

roleLabel :: Role -> Text
roleLabel System = "System"
roleLabel User = "Human"
roleLabel Assistant = "AI"
roleLabel Tool = "Tool"
roleLabel Developer = "Developer"
roleLabel Function = "Function"

unique :: [Text] -> [Text]
unique = foldl addIfMissing []
  where
    addIfMissing :: [Text] -> Text -> [Text]
    addIfMissing variableNames name
      | name `elem` variableNames = variableNames
      | otherwise = variableNames <> [name]

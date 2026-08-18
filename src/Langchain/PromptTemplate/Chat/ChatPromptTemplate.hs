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
  , ChatPromptMessage (..)
  , ChatPromptValue (..)
  , fromTemplate
  , fromTemplateWithOptions
  , fromMessages
  , partial
  , formatPrompt
  , format
  , toMessages
  , toString
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model.Types
  ( Message (..)
  , Role (..)
  , extractMessageText
  , textMessage
  , userMessage
  )
import Langchain.PromptTemplate (PromptTemplateOptions)
import qualified Langchain.PromptTemplate as PromptTemplate
import Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate (HumanMessagePromptTemplate (..))
import qualified Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate as HumanMessagePromptTemplate

data ChatPromptMessage
  = HumanMessagePrompt HumanMessagePromptTemplate
  | SystemMessagePrompt PromptTemplate.PromptTemplate
  | AIMessagePrompt PromptTemplate.PromptTemplate
  | ChatMessagePrompt Role PromptTemplate.PromptTemplate
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

fromTemplate :: Text -> ChatPromptTemplate
fromTemplate template = fromTemplateWithOptions template PromptTemplate.defaultPromptTemplateOptions

fromTemplateWithOptions :: Text -> PromptTemplateOptions -> ChatPromptTemplate
fromTemplateWithOptions template options =
  let message = HumanMessagePromptTemplate.fromTemplateWithOptions template options
   in ChatPromptTemplate
        { messages = [HumanMessagePrompt message]
        , inputVariables = PromptTemplate.inputVariables . prompt $ message
        }

fromMessages :: [ChatPromptMessage] -> ChatPromptTemplate
fromMessages promptMessages =
  ChatPromptTemplate
    { messages = promptMessages
    , inputVariables = unique $ concatMap messageInputVariables promptMessages
    }

partial :: ChatPromptTemplate -> Map.Map Text Text -> ChatPromptTemplate
partial ChatPromptTemplate {messages = promptMessages} partialVariables =
  fromMessages $ map (`partialMessage` partialVariables) promptMessages

formatPrompt :: ChatPromptTemplate -> Map.Map Text Text -> Either LangchainError ChatPromptValue
formatPrompt ChatPromptTemplate {messages = promptMessages} variables =
  ChatPromptValue <$> traverse (`formatMessage` variables) promptMessages

format :: ChatPromptTemplate -> Map.Map Text Text -> Either LangchainError Text
format chatPromptTemplate variables = toString <$> formatPrompt chatPromptTemplate variables

toMessages :: ChatPromptValue -> [Message]
toMessages (ChatPromptValue promptMessages) = promptMessages

toString :: ChatPromptValue -> Text
toString (ChatPromptValue promptMessages) =
  T.intercalate "\n" $ map formatMessageString promptMessages

messageInputVariables :: ChatPromptMessage -> [Text]
messageInputVariables (HumanMessagePrompt message) = PromptTemplate.inputVariables . prompt $ message
messageInputVariables (SystemMessagePrompt promptTemplate) = PromptTemplate.inputVariables promptTemplate
messageInputVariables (AIMessagePrompt promptTemplate) = PromptTemplate.inputVariables promptTemplate
messageInputVariables (ChatMessagePrompt _ promptTemplate) = PromptTemplate.inputVariables promptTemplate
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
partialMessage (StaticMessage message) _ = StaticMessage message

partialPromptTemplate :: PromptTemplate.PromptTemplate -> Map.Map Text Text -> PromptTemplate.PromptTemplate
partialPromptTemplate (PromptTemplate.PromptTemplate template _ existingPartials) partialVariables =
  PromptTemplate.fromTemplateWithOptions template $
    PromptTemplate.PromptTemplateOptions (partialVariables `Map.union` existingPartials)

formatMessage :: ChatPromptMessage -> Map.Map Text Text -> Either LangchainError Message
formatMessage (HumanMessagePrompt message) variables =
  userMessage <$> PromptTemplate.renderPrompt (prompt message) variables
formatMessage (SystemMessagePrompt promptTemplate) variables =
  textMessage System <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (AIMessagePrompt promptTemplate) variables =
  textMessage Assistant <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (ChatMessagePrompt role promptTemplate) variables =
  textMessage role <$> PromptTemplate.renderPrompt promptTemplate variables
formatMessage (StaticMessage message) _ = Right message

formatMessageString :: Message -> Text
formatMessageString message =
  roleLabel (messageRole message) <> ": " <> extractMessageText message

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
    addIfMissing variableNames variableName
      | variableName `elem` variableNames = variableNames
      | otherwise = variableNames <> [variableName]

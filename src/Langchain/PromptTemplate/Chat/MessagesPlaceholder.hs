{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.PromptTemplate.Chat.MessagesPlaceholder
Description : MessagesPlaceholder prompt template
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Langchain.PromptTemplate.Chat.MessagesPlaceholder
  ( MessagesPlaceholder (..)
  , MessagePlaceholderInput (..)
  , messagesPlaceholder
  , optionalMessagesPlaceholder
  , messagesPlaceholderWithLimit
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Langchain.Core.Error (LangchainError, validationError)
import Langchain.Core.Model.Types (Message, Role (..), textMessage)
import Langchain.PromptTemplate.Chat (BaseMessagePromptTemplate (..))

-- | Prompt template that expects one variable to contain an existing message list.
data MessagesPlaceholder = MessagesPlaceholder
  { messagesPlaceholderVariableName :: Text
  , messagesPlaceholderOptional :: Bool
  , messagesPlaceholderNMessages :: Maybe Int
  }
  deriving (Show, Eq)

-- | Input representations accepted by 'MessagesPlaceholder'.
data MessagePlaceholderInput
  = PlaceholderMessage Message
  | PlaceholderRoleMessage Role Text
  | PlaceholderHumanText Text
  deriving (Show, Eq)

instance BaseMessagePromptTemplate MessagesPlaceholder (Map.Map Text [MessagePlaceholderInput]) where
  formatMessages
    MessagesPlaceholder
      { messagesPlaceholderVariableName = variableName
      , messagesPlaceholderOptional = optional
      , messagesPlaceholderNMessages = nMessages
      }
    inputs = do
    values <-
      case Map.lookup variableName inputs of
        Just values' -> Right values'
        Nothing
          | optional -> Right []
          | otherwise ->
              Left $
                validationError
                  ("Missing variable: " <> variableName)
                  (Just variableName)
                  Nothing
    let messages = map toMessage values
    pure $ maybe messages (`takeLast` messages) nMessages

-- | Create a required messages placeholder.
messagesPlaceholder :: Text -> MessagesPlaceholder
messagesPlaceholder variableName = MessagesPlaceholder variableName False Nothing

-- | Create an optional messages placeholder.
optionalMessagesPlaceholder :: Text -> MessagesPlaceholder
optionalMessagesPlaceholder variableName = MessagesPlaceholder variableName True Nothing

-- | Create a required messages placeholder limited to the last n messages.
messagesPlaceholderWithLimit :: Text -> Int -> Either LangchainError MessagesPlaceholder
messagesPlaceholderWithLimit variableName nMessages
  | nMessages > 0 = Right $ MessagesPlaceholder variableName False (Just nMessages)
  | otherwise = Left $ validationError "n_messages must be positive" (Just variableName) Nothing

toMessage :: MessagePlaceholderInput -> Message
toMessage (PlaceholderMessage message) = message
toMessage (PlaceholderRoleMessage role content) = textMessage role content
toMessage (PlaceholderHumanText content) = textMessage User content

takeLast :: Int -> [a] -> [a]
takeLast n values = drop (max 0 (length values - n)) values

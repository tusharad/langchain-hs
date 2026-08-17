{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.PromptTemplate.Chat
Description : Chat prompt template primitives
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Minimal chat prompt primitives ported from LangChain Python chat prompts.
-}
module Langchain.PromptTemplate.Chat
  ( MessagesPlaceholder (..)
  , MessagePlaceholderInput (..)
  , messagesPlaceholder
  , optionalMessagesPlaceholder
  , messagesPlaceholderWithLimit
  , formatMessagesPlaceholder
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Langchain.Core.Error (LangchainError, validationError)
import Langchain.Core.Model.Types (Message, Role (..), textMessage)

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

-- | Format messages from the variable map, matching Python MessagesPlaceholder behavior.
formatMessagesPlaceholder
  :: MessagesPlaceholder
  -> Map.Map Text [MessagePlaceholderInput]
  -> Either LangchainError [Message]
formatMessagesPlaceholder
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

toMessage :: MessagePlaceholderInput -> Message
toMessage (PlaceholderMessage message) = message
toMessage (PlaceholderRoleMessage role content) = textMessage role content
toMessage (PlaceholderHumanText content) = textMessage User content

takeLast :: Int -> [a] -> [a]
takeLast n values = drop (max 0 (length values - n)) values

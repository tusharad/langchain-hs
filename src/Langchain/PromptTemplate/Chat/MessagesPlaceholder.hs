{-# LANGUAGE DuplicateRecordFields #-}
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
  , messagesPlaceholder
  , optionalMessagesPlaceholder
  , messagesPlaceholderWithLimit
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Langchain.Core.Error (LangchainError, validationError)
import Langchain.Core.Model.Types (Message)
import Langchain.PromptTemplate.Chat (BaseMessagePromptTemplate (..))

-- | Prompt template that expects one variable to contain an existing message list.
data MessagesPlaceholder = MessagesPlaceholder
  { variableName :: Text
  , optional :: Bool
  , nMessages :: Maybe Int
  }
  deriving (Show, Eq)

instance BaseMessagePromptTemplate MessagesPlaceholder (Map.Map Text [Message]) where
  formatMessages
    MessagesPlaceholder
      { variableName = variableName'
      , optional = optional'
      , nMessages = nMessages'
      }
    inputs = do
      values <-
        case Map.lookup variableName' inputs of
          Just values' -> Right values'
          Nothing
            | optional' -> Right []
            | otherwise ->
                Left $
                  validationError
                    ("Missing variable: " <> variableName')
                    (Just variableName')
                    Nothing
      pure $ maybe values (`takeLast` values) nMessages'

-- | Create a required messages placeholder.
messagesPlaceholder :: Text -> MessagesPlaceholder
messagesPlaceholder name = MessagesPlaceholder name False Nothing

-- | Create an optional messages placeholder.
optionalMessagesPlaceholder :: Text -> MessagesPlaceholder
optionalMessagesPlaceholder name = MessagesPlaceholder name True Nothing

-- | Create a required messages placeholder limited to the last n messages.
messagesPlaceholderWithLimit :: Text -> Int -> Either LangchainError MessagesPlaceholder
messagesPlaceholderWithLimit name limit
  | limit > 0 = Right $ MessagesPlaceholder name False (Just limit)
  | otherwise = Left $ validationError "n_messages must be positive" (Just name) Nothing

takeLast :: Int -> [a] -> [a]
takeLast n values = drop (max 0 (length values - n)) values

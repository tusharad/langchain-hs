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
  , MessagesPlaceholderOptions (..)
  , messagesPlaceholder
  , messagesPlaceholderOptions
  , messagesPlaceholderWithOptions
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Langchain.Core.Error (validationError)
import Langchain.Core.Model.Types (Message)
import Langchain.PromptTemplate.Chat (BaseMessagePromptTemplate (..))

-- | Prompt template that expects one variable to contain an existing message list.
data MessagesPlaceholder = MessagesPlaceholder
  { variableName :: Text
  , optional :: Bool
  , nMessages :: Maybe Int
  }
  deriving (Show, Eq)

data MessagesPlaceholderOptions = MessagesPlaceholderOptions
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
      case nMessages' of
        Just limit
          | limit <= 0 ->
              Left $ validationError "n_messages must be positive" (Just variableName') Nothing
        _ -> pure $ maybe values (`takeLast` values) nMessages'

-- | Create a required messages placeholder.
messagesPlaceholder :: Text -> MessagesPlaceholder
messagesPlaceholder name = messagesPlaceholderWithOptions $ messagesPlaceholderOptions name

messagesPlaceholderOptions :: Text -> MessagesPlaceholderOptions
messagesPlaceholderOptions name =
  MessagesPlaceholderOptions
    { variableName = name
    , optional = False
    , nMessages = Nothing
    }

messagesPlaceholderWithOptions :: MessagesPlaceholderOptions -> MessagesPlaceholder
messagesPlaceholderWithOptions MessagesPlaceholderOptions {variableName = name, optional = optional', nMessages = nMessages'} =
  MessagesPlaceholder
    { variableName = name
    , optional = optional'
    , nMessages = nMessages'
    }

takeLast :: Int -> [a] -> [a]
takeLast n values = drop (max 0 (length values - n)) values

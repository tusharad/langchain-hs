{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Langchain.PromptTemplate.Chat.ChatMessagePromptTemplate
Description : ChatMessagePromptTemplate prompt template
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Langchain.PromptTemplate.Chat.ChatMessagePromptTemplate
  ( ChatMessagePromptTemplate (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)

import Langchain.PromptTemplate.Chat (BaseStringMessagePromptTemplate (..))
import Langchain.PromptTemplate.Prompt (PromptTemplate, fromTemplate)
import Langchain.PromptTemplate.String (extractTemplateVariables)

-- | Chat message prompt template contract with a custom role.
data ChatMessagePromptTemplate = ChatMessagePromptTemplate
  { prompt :: PromptTemplate
  , inputVariables :: [Text]
  , role :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance BaseStringMessagePromptTemplate ChatMessagePromptTemplate Text where
  fromTemplateFile templateFile messageRole = do
    template <- T.dropWhileEnd (== '\n') <$> TIO.readFile templateFile
    pure $
      ChatMessagePromptTemplate
        { prompt = fromTemplate template
        , inputVariables = extractTemplateVariables template
        , role = messageRole
        }

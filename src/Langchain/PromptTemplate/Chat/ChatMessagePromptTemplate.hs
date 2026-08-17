{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Langchain.PromptTemplate (PromptTemplate (..))
import Langchain.PromptTemplate.Chat (BaseStringMessagePromptTemplate (..), extractTemplateVariables)

-- | Chat message prompt template contract with a custom role.
data ChatMessagePromptTemplate = ChatMessagePromptTemplate
  { chatMessagePromptTemplatePrompt :: PromptTemplate
  , chatMessagePromptTemplateInputVariables :: [Text]
  , chatMessagePromptTemplateRole :: Text
  }
  deriving (Show, Eq)

instance BaseStringMessagePromptTemplate ChatMessagePromptTemplate Text where
  fromTemplateFile templateFile role = do
    template <- T.dropWhileEnd (== '\n') <$> TIO.readFile templateFile
    pure $
      ChatMessagePromptTemplate
        { chatMessagePromptTemplatePrompt = PromptTemplate template
        , chatMessagePromptTemplateInputVariables = extractTemplateVariables template
        , chatMessagePromptTemplateRole = role
        }

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
  , fromTemplate
  , fromTemplateWithOptions
  ) where

import Data.Text (Text)

import Langchain.PromptTemplate (PromptTemplateOptions)
import qualified Langchain.PromptTemplate as PromptTemplate
import Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate (HumanMessagePromptTemplate (..))
import qualified Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate as HumanMessagePromptTemplate

data ChatPromptTemplate = ChatPromptTemplate
  { messages :: [HumanMessagePromptTemplate]
  , inputVariables :: [Text]
  }
  deriving (Show, Eq)

fromTemplate :: Text -> ChatPromptTemplate
fromTemplate template = fromTemplateWithOptions template PromptTemplate.defaultPromptTemplateOptions

fromTemplateWithOptions :: Text -> PromptTemplateOptions -> ChatPromptTemplate
fromTemplateWithOptions template options =
  let message = HumanMessagePromptTemplate.fromTemplateWithOptions template options
   in ChatPromptTemplate
        { messages = [message]
        , inputVariables = PromptTemplate.inputVariables . prompt $ message
        }

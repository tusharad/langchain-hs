{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate
Description : HumanMessagePromptTemplate prompt template
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate
  ( HumanMessagePromptTemplate (..)
  , fromTemplate
  , fromTemplateWithOptions
  ) where

import Data.Text (Text)

import Langchain.PromptTemplate (PromptTemplate, PromptTemplateOptions)
import qualified Langchain.PromptTemplate as PromptTemplate

newtype HumanMessagePromptTemplate = HumanMessagePromptTemplate
  { prompt :: PromptTemplate
  }
  deriving (Show, Eq)

fromTemplate :: Text -> HumanMessagePromptTemplate
fromTemplate template = fromTemplateWithOptions template PromptTemplate.defaultPromptTemplateOptions

fromTemplateWithOptions :: Text -> PromptTemplateOptions -> HumanMessagePromptTemplate
fromTemplateWithOptions template options =
  HumanMessagePromptTemplate
    { prompt = PromptTemplate.fromTemplateWithOptions template options
    }

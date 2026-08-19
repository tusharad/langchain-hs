{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Langchain.PromptTemplate.Prompt
  ( PromptTemplate
  , PromptTemplateOptions
  , defaultPromptTemplateOptions
  )
import qualified Langchain.PromptTemplate.Prompt as Prompt

newtype HumanMessagePromptTemplate = HumanMessagePromptTemplate
  { prompt :: PromptTemplate
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

fromTemplate :: Text -> HumanMessagePromptTemplate
fromTemplate template = fromTemplateWithOptions template defaultPromptTemplateOptions

fromTemplateWithOptions :: Text -> PromptTemplateOptions -> HumanMessagePromptTemplate
fromTemplateWithOptions template options =
  HumanMessagePromptTemplate
    { prompt = Prompt.fromTemplateWithOptions template options
    }

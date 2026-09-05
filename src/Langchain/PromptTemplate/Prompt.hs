{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- |
Module      : Langchain.PromptTemplate.Prompt
Description : String prompt templates
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Prompt templates backed by string interpolation.
-}
module Langchain.PromptTemplate.Prompt
  ( PromptTemplate (..)
  , PromptTemplateOptions (..)
  , TemplateFormat (..)
  , defaultPromptTemplateOptions
  , fromTemplate
  , fromTemplateWithOptions
  , fromTemplateWithFormat
  , partialPromptTemplate
  , renderPrompt
  , renderTemplateWithFormat
  , renderFStringTemplate
  , extractTemplateVariables
  , extractTemplateVariablesWithFormat
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError)
import Langchain.PromptTemplate.String
  ( TemplateFormat (..)
  , extractTemplateVariables
  , extractTemplateVariablesWithFormat
  , renderFStringTemplate
  , renderTemplateWithFormat
  )

-- | Prompt template container with template string containing {var} placeholders.
data PromptTemplate = PromptTemplate
  { template :: Text
  , inputVariables :: [Text]
  , -- Matches Python partial_variables: pre-bound values reduce required inputs
    -- without changing the original template string.
    partialVariables :: Map.Map Text Text
  , templateFormat :: TemplateFormat
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Options for building a prompt template, currently only partial variables.
newtype PromptTemplateOptions = PromptTemplateOptions
  { partialVariables :: Map.Map Text Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default prompt template options with no partial variables.
defaultPromptTemplateOptions :: PromptTemplateOptions
defaultPromptTemplateOptions = PromptTemplateOptions mempty

-- | Build a string prompt template using the default FString format.
fromTemplate :: Text -> PromptTemplate
fromTemplate source = fromTemplateWithOptions source defaultPromptTemplateOptions

-- | Build a string prompt template with pre-bound partial variables.
fromTemplateWithOptions :: Text -> PromptTemplateOptions -> PromptTemplate
fromTemplateWithOptions source (PromptTemplateOptions partials) =
  fromTemplateWithFormat source FString partials

-- | Build a prompt template from raw text, format, and partial variables.
fromTemplateWithFormat :: Text -> TemplateFormat -> Map.Map Text Text -> PromptTemplate
fromTemplateWithFormat source format partials =
  PromptTemplate
    { template = source
    , inputVariables =
        filter (`Map.notMember` partials) (extractTemplateVariablesWithFormat format source)
    , partialVariables = partials
    , templateFormat = format
    }

-- | Apply additional partial variables to an existing prompt template.
partialPromptTemplate :: PromptTemplate -> Map.Map Text Text -> PromptTemplate
partialPromptTemplate (PromptTemplate source _ existingPartials format) partials =
  fromTemplateWithFormat source format (partials `Map.union` existingPartials)

-- | Render a prompt template with the given variable map.
renderPrompt :: PromptTemplate -> Map.Map Text Text -> Either LangchainError Text
renderPrompt (PromptTemplate source _ partials format) vars =
  renderTemplateWithFormat format (vars `Map.union` partials) source

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.PromptTemplate.FewShot
Description : Few-shot prompt templates
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Langchain.PromptTemplate.FewShot
  ( FewShotPromptTemplate (..)
  , renderFewShotPrompt
  , renderFewShotPromptWithVars
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError)
import Langchain.PromptTemplate.Prompt (fromTemplate, renderPrompt)

-- | Represents a few-shot prompt template with examples
data FewShotPromptTemplate = FewShotPromptTemplate
  { fsPrefix :: Text
  , fsExamples :: [Map.Map Text Text]
  , fsExampleTemplate :: Text
  , fsExampleSeparator :: Text
  , fsSuffix :: Text
  }
  deriving (Show, Eq)

-- | Render a few-shot prompt template
renderFewShotPrompt :: FewShotPromptTemplate -> Either LangchainError Text
renderFewShotPrompt FewShotPromptTemplate {..} = do
  formattedExamples <- traverse (renderPrompt (fromTemplate fsExampleTemplate)) fsExamples
  let examplesText = T.intercalate fsExampleSeparator formattedExamples
  pure $ fsPrefix <> examplesText <> fsSuffix

-- | Render few-shot template with additional variables
renderFewShotPromptWithVars ::
  FewShotPromptTemplate -> Map.Map Text Text -> Either LangchainError Text
renderFewShotPromptWithVars template vars = do
  renderedBase <- renderFewShotPrompt template
  renderPrompt (fromTemplate renderedBase) vars

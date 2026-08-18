{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.PromptTemplate
Description : Prompt templates and variable interpolation for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Pure prompt templates and few-shot prompt templates with variable interpolation.
-}
module Langchain.PromptTemplate
  ( -- * Core Types
    PromptTemplate (..)
  , PromptTemplateOptions (..)
  , FewShotPromptTemplate (..)

    -- * Rendering Functions
  , defaultPromptTemplateOptions
  , fromTemplate
  , fromTemplateWithOptions
  , renderPrompt
  , renderFewShotPrompt
  , renderFewShotPromptWithVars
  , extractTemplateVariables
  ) where

import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, validationError)

-- | Prompt template container with template string containing {var} placeholders.
data PromptTemplate = PromptTemplate
  { template :: Text
  , inputVariables :: [Text]
  -- Matches Python partial_variables: pre-bound values reduce required inputs
  -- without changing the original template string.
  , partialVariables :: HM.Map Text Text
  }
  deriving (Show, Eq)

newtype PromptTemplateOptions = PromptTemplateOptions
  { partialVariables :: HM.Map Text Text
  }
  deriving (Show, Eq)

defaultPromptTemplateOptions :: PromptTemplateOptions
defaultPromptTemplateOptions = PromptTemplateOptions mempty

fromTemplate :: Text -> PromptTemplate
fromTemplate template = fromTemplateWithOptions template defaultPromptTemplateOptions

fromTemplateWithOptions :: Text -> PromptTemplateOptions -> PromptTemplate
fromTemplateWithOptions template (PromptTemplateOptions partials) =
  PromptTemplate
    { template = template
    , inputVariables = filter (`HM.notMember` partials) (extractTemplateVariables template)
    , partialVariables = partials
    }

-- | Render a prompt template with the given variable map
renderPrompt :: PromptTemplate -> HM.Map Text Text -> Either LangchainError Text
renderPrompt (PromptTemplate template _ partials) vars = interpolate (vars `HM.union` partials) template

-- | Represents a few-shot prompt template with examples
data FewShotPromptTemplate = FewShotPromptTemplate
  { fsPrefix :: Text
  , fsExamples :: [HM.Map Text Text]
  , fsExampleTemplate :: Text
  , fsExampleSeparator :: Text
  , fsSuffix :: Text
  }
  deriving (Show, Eq)

-- | Render a few-shot prompt template
renderFewShotPrompt :: FewShotPromptTemplate -> Either LangchainError Text
renderFewShotPrompt FewShotPromptTemplate {..} = do
  formattedExamples <-
    mapM
      (`interpolate` fsExampleTemplate)
      fsExamples
  let examplesText = T.intercalate fsExampleSeparator formattedExamples
  pure $ fsPrefix <> examplesText <> fsSuffix

-- | Interpolate variables into a template string
interpolate :: HM.Map Text Text -> Text -> Either LangchainError Text
interpolate vars = go
  where
    go :: Text -> Either LangchainError Text
    go t =
      case T.breakOn "{" t of
        (before, after) | T.null after -> Right before
        (before, after') ->
          case T.breakOn "}" (T.drop 1 after') of
            (_, after'') | T.null after'' -> Left $ validationError "Unclosed brace in template" (Just "PromptTemplate") Nothing
            (key, after''') ->
              let key' = T.strip key
               in case HM.lookup key' vars of
                    Just val -> do
                      rest <- go (T.drop 1 after''')
                      pure $ before <> val <> rest
                    Nothing -> Left $ validationError ("Missing variable: " <> key') (Just key') Nothing

-- | Render few-shot template with additional variables
renderFewShotPromptWithVars :: FewShotPromptTemplate -> HM.Map Text Text -> Either LangchainError Text
renderFewShotPromptWithVars template vars = do
  renderedBase <- renderFewShotPrompt template
  interpolate vars renderedBase

extractTemplateVariables :: Text -> [Text]
extractTemplateVariables = unique . go
  where
    go :: Text -> [Text]
    go template =
      case T.breakOn "{" template of
        (_, rest) | T.null rest -> []
        (_, rest) ->
          let afterOpen = T.drop 1 rest
           in case T.breakOn "}" afterOpen of
                (_, afterClose) | T.null afterClose -> []
                (variableName, afterClose) ->
                  T.strip variableName : go (T.drop 1 afterClose)

    unique :: [Text] -> [Text]
    unique = foldl addIfMissing []

    addIfMissing :: [Text] -> Text -> [Text]
    addIfMissing variableNames variableName
      | variableName `elem` variableNames = variableNames
      | otherwise = variableNames <> [variableName]

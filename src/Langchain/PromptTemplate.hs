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
  , TemplateFormat (..)
  , FewShotPromptTemplate (..)

    -- * Rendering Functions
  , defaultPromptTemplateOptions
  , fromTemplate
  , fromTemplateWithOptions
  , fromTemplateWithFormat
  , partialPromptTemplate
  , renderPrompt
  , renderFewShotPrompt
  , renderFewShotPromptWithVars
  , extractTemplateVariables
  , extractTemplateVariablesWithFormat
  ) where

import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, validationError)

-- | Prompt template container with template string containing {var} placeholders.
data PromptTemplate = PromptTemplate
  { template :: Text
  , inputVariables :: [Text]
  , -- Matches Python partial_variables: pre-bound values reduce required inputs
    -- without changing the original template string.
    partialVariables :: HM.Map Text Text
  , templateFormat :: TemplateFormat
  }
  deriving (Show, Eq)

data TemplateFormat
  = FString
  | Mustache
  | Jinja2
  deriving (Show, Eq)

newtype PromptTemplateOptions = PromptTemplateOptions
  { partialVariables :: HM.Map Text Text
  }
  deriving (Show, Eq)

defaultPromptTemplateOptions :: PromptTemplateOptions
defaultPromptTemplateOptions = PromptTemplateOptions mempty

data TemplatePart
  = Literal Text
  | Variable Text

parseTemplateWithFormat :: TemplateFormat -> Text -> Either LangchainError [TemplatePart]
parseTemplateWithFormat FString = parseFStringTemplate
parseTemplateWithFormat Mustache = parseDoubleBraceTemplate
parseTemplateWithFormat Jinja2 = parseDoubleBraceTemplate

parseFStringTemplate :: Text -> Either LangchainError [TemplatePart]
parseFStringTemplate = go
  where
    go :: Text -> Either LangchainError [TemplatePart]
    go template =
      case T.breakOn "{" template of
        (literal, rest) | T.null rest -> Right [Literal literal | not (T.null literal)]
        (literal, rest) -> do
          let afterOpen = T.drop 1 rest
          case T.breakOn "}" afterOpen of
            (_, afterClose)
              | T.null afterClose ->
                  Left $ validationError "Unclosed brace in template" (Just "PromptTemplate") Nothing
            (variableName, afterClose) -> do
              remainingParts <- go (T.drop 1 afterClose)
              pure $
                [Literal literal | not (T.null literal)]
                  <> [Variable (T.strip variableName)]
                  <> remainingParts

parseDoubleBraceTemplate :: Text -> Either LangchainError [TemplatePart]
parseDoubleBraceTemplate = go
  where
    go :: Text -> Either LangchainError [TemplatePart]
    go template =
      case T.breakOn "{{" template of
        (literal, rest) | T.null rest -> Right [Literal literal | not (T.null literal)]
        (literal, rest) -> do
          let afterOpen = T.drop 2 rest
          case T.breakOn "}}" afterOpen of
            (_, afterClose)
              | T.null afterClose ->
                  Left $ validationError "Unclosed double brace in template" (Just "PromptTemplate") Nothing
            (variableName, afterClose) -> do
              remainingParts <- go (T.drop 2 afterClose)
              pure $
                [Literal literal | not (T.null literal)]
                  <> [Variable (T.strip variableName)]
                  <> remainingParts

fromTemplate :: Text -> PromptTemplate
fromTemplate template = fromTemplateWithOptions template defaultPromptTemplateOptions

fromTemplateWithOptions :: Text -> PromptTemplateOptions -> PromptTemplate
fromTemplateWithOptions template (PromptTemplateOptions partials) =
  fromTemplateWithFormat template FString partials

fromTemplateWithFormat :: Text -> TemplateFormat -> HM.Map Text Text -> PromptTemplate
fromTemplateWithFormat template templateFormat partials =
  PromptTemplate
    { template = template
    , inputVariables =
        filter (`HM.notMember` partials) (extractTemplateVariablesWithFormat templateFormat template)
    , partialVariables = partials
    , templateFormat = templateFormat
    }

partialPromptTemplate :: PromptTemplate -> HM.Map Text Text -> PromptTemplate
partialPromptTemplate (PromptTemplate template _ existingPartials templateFormat) partials =
  fromTemplateWithFormat template templateFormat (partials `HM.union` existingPartials)

-- | Render a prompt template with the given variable map
renderPrompt :: PromptTemplate -> HM.Map Text Text -> Either LangchainError Text
renderPrompt (PromptTemplate template _ partials templateFormat) vars = interpolate templateFormat (vars `HM.union` partials) template

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
      (\example -> interpolate FString example fsExampleTemplate)
      fsExamples
  let examplesText = T.intercalate fsExampleSeparator formattedExamples
  pure $ fsPrefix <> examplesText <> fsSuffix

-- | Interpolate variables into a template string
interpolate :: TemplateFormat -> HM.Map Text Text -> Text -> Either LangchainError Text
interpolate templateFormat vars template = do
  parts <- parseTemplateWithFormat templateFormat template
  T.concat <$> traverse renderPart parts
  where
    renderPart :: TemplatePart -> Either LangchainError Text
    renderPart (Literal literal) = Right literal
    renderPart (Variable variableName) =
      case HM.lookup variableName vars of
        Just value -> Right value
        Nothing -> Left $ validationError ("Missing variable: " <> variableName) (Just variableName) Nothing

-- | Render few-shot template with additional variables
renderFewShotPromptWithVars ::
  FewShotPromptTemplate -> HM.Map Text Text -> Either LangchainError Text
renderFewShotPromptWithVars template vars = do
  renderedBase <- renderFewShotPrompt template
  interpolate FString vars renderedBase

extractTemplateVariables :: Text -> [Text]
extractTemplateVariables = extractTemplateVariablesWithFormat FString

extractTemplateVariablesWithFormat :: TemplateFormat -> Text -> [Text]
extractTemplateVariablesWithFormat templateFormat template =
  case parseTemplateWithFormat templateFormat template of
    Left _ -> []
    Right parts -> unique [variableName | Variable variableName <- parts]
  where
    unique :: [Text] -> [Text]
    unique = foldl addIfMissing []

    addIfMissing :: [Text] -> Text -> [Text]
    addIfMissing variableNames variableName
      | variableName `elem` variableNames = variableNames
      | otherwise = variableNames <> [variableName]

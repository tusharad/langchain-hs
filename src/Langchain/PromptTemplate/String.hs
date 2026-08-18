{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.PromptTemplate.String
Description : String prompt template formatting helpers
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

String template parsing, variable extraction, and interpolation helpers.
-}
module Langchain.PromptTemplate.String
  ( TemplateFormat (..)
  , renderTemplateWithFormat
  , extractTemplateVariables
  , extractTemplateVariablesWithFormat
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, validationError)

data TemplateFormat
  = FString
  | Mustache
  | Jinja2
  deriving (Show, Eq)

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
    go source =
      case T.breakOn "{" source of
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
    go source =
      case T.breakOn "{{" source of
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

renderTemplateWithFormat ::
  TemplateFormat -> Map.Map Text Text -> Text -> Either LangchainError Text
renderTemplateWithFormat format vars source = do
  let renderedSections = renderSections format vars source
  parts <- parseTemplateWithFormat format renderedSections
  T.concat <$> traverse renderPart parts
  where
    renderPart :: TemplatePart -> Either LangchainError Text
    renderPart (Literal literal) = Right literal
    renderPart (Variable variableName) =
      case Map.lookup variableName vars of
        Just value -> Right value
        Nothing -> Left $ validationError ("Missing variable: " <> variableName) (Just variableName) Nothing

renderSections :: TemplateFormat -> Map.Map Text Text -> Text -> Text
renderSections Mustache vars = renderMustacheSections vars
renderSections _ _ = id

renderMustacheSections :: Map.Map Text Text -> Text -> Text
renderMustacheSections vars = go
  where
    go :: Text -> Text
    go source =
      case T.breakOn "{{#" source of
        (before, rest) | T.null rest -> before
        (before, rest) ->
          let afterOpen = T.drop 3 rest
           in case T.breakOn "}}" afterOpen of
                (_, closeOpen) | T.null closeOpen -> before <> rest
                (nameRaw, afterName) ->
                  let name = T.strip nameRaw
                      closeTag = "{{/" <> name <> "}}"
                      bodyAndRest = T.drop 2 afterName
                   in case T.breakOn closeTag bodyAndRest of
                        (_, closeClose) | T.null closeClose -> before <> rest
                        (body, afterClose) ->
                          let replacement =
                                case Map.lookup name vars of
                                  Just value | not (T.null value) -> go body
                                  _ -> ""
                           in before <> replacement <> go (T.drop (T.length closeTag) afterClose)

extractTemplateVariables :: Text -> [Text]
extractTemplateVariables = extractTemplateVariablesWithFormat FString

extractTemplateVariablesWithFormat :: TemplateFormat -> Text -> [Text]
extractTemplateVariablesWithFormat format source =
  case parseTemplateWithFormat format source of
    Left _ -> []
    Right parts -> unique [variableName | Variable variableName <- parts]
  where
    unique :: [Text] -> [Text]
    unique = foldl addIfMissing []

    addIfMissing :: [Text] -> Text -> [Text]
    addIfMissing variableNames variableName
      | variableName `elem` variableNames = variableNames
      | otherwise = variableNames <> [variableName]

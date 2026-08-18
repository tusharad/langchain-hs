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

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Mustache
  ( Key
  , Node (..)
  , Template (..)
  , compileMustacheText
  , renderMustache
  )
import Text.Mustache.Type (showKey)

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
parseTemplateWithFormat FString = parseDelimitedTemplate "{" "}" "Unclosed brace in template"
parseTemplateWithFormat Mustache = parseDelimitedTemplate "{{" "}}" "Unclosed double brace in template"
parseTemplateWithFormat Jinja2 = parseDelimitedTemplate "{{" "}}" "Unclosed double brace in template"

parseDelimitedTemplate :: Text -> Text -> Text -> Text -> Either LangchainError [TemplatePart]
parseDelimitedTemplate open close unclosed = go
  where
    go :: Text -> Either LangchainError [TemplatePart]
    go source =
      case T.breakOn open source of
        (literal, rest) | T.null rest -> Right [Literal literal | not (T.null literal)]
        (literal, rest) -> do
          let afterOpen = T.drop (T.length open) rest
          case T.breakOn close afterOpen of
            (_, afterClose)
              | T.null afterClose ->
                  Left $ validationError unclosed (Just "PromptTemplate") Nothing
            (variableName, afterClose) -> do
              remainingParts <- go (T.drop (T.length close) afterClose)
              pure $
                [Literal literal | not (T.null literal)]
                  <> [Variable (T.strip variableName)]
                  <> remainingParts

renderTemplateWithFormat ::
  TemplateFormat -> Map.Map Text Text -> Text -> Either LangchainError Text
renderTemplateWithFormat Mustache vars source = renderMustacheTemplate vars source
renderTemplateWithFormat format vars source = do
  parts <- parseTemplateWithFormat format source
  T.concat <$> traverse renderPart parts
  where
    renderPart :: TemplatePart -> Either LangchainError Text
    renderPart (Literal literal) = Right literal
    renderPart (Variable variableName) =
      case Map.lookup variableName vars of
        Just value -> Right value
        Nothing -> Left $ validationError ("Missing variable: " <> variableName) (Just variableName) Nothing

renderMustacheTemplate :: Map.Map Text Text -> Text -> Either LangchainError Text
renderMustacheTemplate vars source =
  case compileMustacheText "PromptTemplate" source of
    Left err -> Left $ validationError (T.pack $ show err) (Just "PromptTemplate") Nothing
    Right template -> do
      traverse_ requireVariable $ requiredMustacheVariables template
      Right . TL.toStrict $ renderMustache template (toMustacheContext vars)
  where
    requireVariable variableName
      | variableName `Map.member` vars = Right ()
      | otherwise =
          Left $ validationError ("Missing variable: " <> variableName) (Just variableName) Nothing

toMustacheContext :: Map.Map Text Text -> Value
toMustacheContext vars =
  object [Key.fromText key .= value | (key, value) <- Map.toList vars]

extractTemplateVariables :: Text -> [Text]
extractTemplateVariables = extractTemplateVariablesWithFormat FString

extractTemplateVariablesWithFormat :: TemplateFormat -> Text -> [Text]
extractTemplateVariablesWithFormat Mustache source =
  case compileMustacheText "PromptTemplate" source of
    Left _ -> []
    Right template -> unique $ templateVariables template
extractTemplateVariablesWithFormat format source =
  case parseTemplateWithFormat format source of
    Left _ -> []
    Right parts -> unique [variableName | Variable variableName <- parts]

unique :: [Text] -> [Text]
unique = foldl addIfMissing []

addIfMissing :: [Text] -> Text -> [Text]
addIfMissing variableNames variableName
  | variableName `elem` variableNames = variableNames
  | otherwise = variableNames <> [variableName]

templateVariables :: Template -> [Text]
templateVariables Template {templateActual = actual, templateCache = cache} =
  maybe [] (concatMap nodeVariables) $ Map.lookup actual cache

requiredMustacheVariables :: Template -> [Text]
requiredMustacheVariables Template {templateActual = actual, templateCache = cache} =
  maybe [] (unique . concatMap requiredNodeVariables) $ Map.lookup actual cache

nodeVariables :: Node -> [Text]
nodeVariables (TextBlock _) = []
nodeVariables (EscapedVar key) = keyVariables key
nodeVariables (UnescapedVar key) = keyVariables key
nodeVariables (Section key nodes) = keyVariables key <> concatMap nodeVariables nodes
nodeVariables (InvertedSection key nodes) = keyVariables key <> concatMap nodeVariables nodes
nodeVariables (Partial _ _) = []

requiredNodeVariables :: Node -> [Text]
requiredNodeVariables (TextBlock _) = []
requiredNodeVariables (EscapedVar key) = keyVariables key
requiredNodeVariables (UnescapedVar key) = keyVariables key
requiredNodeVariables (Section _ _) = []
requiredNodeVariables (InvertedSection _ _) = []
requiredNodeVariables (Partial _ _) = []

keyVariables :: Key -> [Text]
keyVariables = (: []) . showKey

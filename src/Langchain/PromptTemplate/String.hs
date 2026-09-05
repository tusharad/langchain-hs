{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Format.Heavy.Build (formatEither)
import Data.Text.Format.Heavy.Instances ()
import Data.Text.Format.Heavy.Parse (FormatParseItem (..), parse, parseFormat)
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError, validationError)

data TemplateFormat
  = FString
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

renderTemplateWithFormat :: TemplateFormat -> Map.Map Text Text -> Text -> Either LangchainError Text
renderTemplateWithFormat FString = renderFStringTemplate

renderFStringTemplate :: Map.Map Text Text -> Text -> Either LangchainError Text
renderFStringTemplate vars source = do
  items <- parseFStringTemplate source
  traverse_ validateFStringItem items
  format <- mapParseError $ parseFormat (TL.fromStrict source)
  mapFormatError $ TL.toStrict <$> formatEither format (toFStringVars vars)
  where
    mapFormatError :: Either String a -> Either LangchainError a
    mapFormatError (Left err) = Left $ validationError (T.pack err) (Just "PromptTemplate") Nothing
    mapFormatError (Right result) = Right result

toFStringVars :: Map.Map Text Text -> Map.Map TL.Text Text
toFStringVars = Map.mapKeys TL.fromStrict

parseFStringTemplate :: Text -> Either LangchainError [FormatParseItem]
parseFStringTemplate source = mapParseError $ parse (TL.fromStrict source)

mapParseError :: (Show err) => Either err a -> Either LangchainError a
mapParseError (Left err) = Left $ validationError (T.pack $ show err) (Just "PromptTemplate") Nothing
mapParseError (Right result) = Right result

validateFStringItem :: FormatParseItem -> Either LangchainError ()
validateFStringItem (FormatString _) = Right ()
validateFStringItem (FormatReplacementField variableName formatSpec) = do
  validateFStringVariableName variableName
  traverse_ validateFStringFormatSpec formatSpec

validateFStringVariableName :: TL.Text -> Either LangchainError ()
validateFStringVariableName variableName
  | TL.all isDigit variableName =
      Left $
        validationError "Positional arguments are not supported" (Just $ TL.toStrict variableName) Nothing
  | TL.any (== '.') variableName =
      Left $ validationError "Attribute access is not supported" (Just $ TL.toStrict variableName) Nothing
  | TL.any (`elem` ['[', ']']) variableName =
      Left $ validationError "Index access is not supported" (Just $ TL.toStrict variableName) Nothing
  | otherwise = Right ()

validateFStringFormatSpec :: TL.Text -> Either LangchainError ()
validateFStringFormatSpec formatSpec
  | TL.any (`elem` ['{', '}']) formatSpec =
      Left $ validationError "Nested replacement fields are not allowed" (Just "PromptTemplate") Nothing
  | otherwise = Right ()

extractTemplateVariables :: Text -> [Text]
extractTemplateVariables = extractTemplateVariablesWithFormat FString

extractTemplateVariablesWithFormat :: TemplateFormat -> Text -> [Text]
extractTemplateVariablesWithFormat FString source =
  case parseFStringTemplate source of
    Left _ -> []
    Right parts -> unique [TL.toStrict variableName | FormatReplacementField variableName _ <- parts]

unique :: [Text] -> [Text]
unique = foldl addIfMissing []

addIfMissing :: [Text] -> Text -> [Text]
addIfMissing variableNames variableName
  | variableName `elem` variableNames = variableNames
  | otherwise = variableNames <> [variableName]

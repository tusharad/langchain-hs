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

import Control.Monad.Trans.Writer.Lazy (Writer)
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Format.Heavy.Build (formatEither)
import Data.Text.Format.Heavy.Instances ()
import Data.Text.Format.Heavy.Parse (FormatParseItem (..), parse, parseFormat)
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import qualified Text.Ginger as Ginger
import Text.Ginger.Html (Html, htmlSource)
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
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

renderTemplateWithFormat ::
  TemplateFormat -> Map.Map Text Text -> Text -> Either LangchainError Text
renderTemplateWithFormat FString vars source = renderFStringTemplate vars source
renderTemplateWithFormat Mustache vars source = renderMustacheTemplate vars source
renderTemplateWithFormat Jinja2 vars source = renderJinja2Template vars source

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
  | TL.all isAsciiDigit variableName =
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

isAsciiDigit :: Char -> Bool
isAsciiDigit = isDigit

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

renderJinja2Template :: Map.Map Text Text -> Text -> Either LangchainError Text
renderJinja2Template vars source = do
  template <- parseJinja2Template source
  traverse_ requireVariable $ gingerTemplateVariables template
  Right . htmlSource $ Ginger.runGinger (gingerContext vars) template
  where
    requireVariable variableName
      | variableName `Map.member` vars = Right ()
      | otherwise =
          Left $ validationError ("Missing variable: " <> variableName) (Just variableName) Nothing

parseJinja2Template :: Text -> Either LangchainError (Ginger.Template Ginger.SourcePos)
parseJinja2Template source =
  case runIdentity $ Ginger.parseGinger noIncludes Nothing (T.unpack source) of
    Left err -> Left $ validationError (T.pack $ show err) (Just "PromptTemplate") Nothing
    Right template -> Right template
  where
    noIncludes _ = pure Nothing

gingerContext :: Map.Map Text Text -> Ginger.GingerContext Ginger.SourcePos (Writer Html) Html
gingerContext vars = Ginger.makeContextHtml $ \variableName ->
  Ginger.toGVal $ Map.findWithDefault "" variableName vars

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
extractTemplateVariablesWithFormat Jinja2 source =
  case parseJinja2Template source of
    Left _ -> []
    Right template -> gingerTemplateVariables template
extractTemplateVariablesWithFormat FString source =
  case parseFStringTemplate source of
    Left _ -> []
    Right parts -> unique [TL.toStrict variableName | FormatReplacementField variableName _ <- parts]

gingerTemplateVariables :: Ginger.Template a -> [Text]
gingerTemplateVariables Ginger.Template {Ginger.templateBody = body} = unique $ statementVariables [] body

statementVariables :: [Ginger.VarName] -> Ginger.Statement a -> [Text]
statementVariables scope statement =
  case statement of
    Ginger.MultiS _ statements -> fst $ foldl collectStatementVariables ([], scope) statements
    Ginger.ScopedS _ scopedStatement -> statementVariables scope scopedStatement
    Ginger.IndentS _ expression indentedStatement -> expressionVariables scope expression <> statementVariables scope indentedStatement
    Ginger.LiteralS _ _ -> []
    Ginger.InterpolationS _ expression -> expressionVariables scope expression
    Ginger.ExpressionS _ expression -> expressionVariables scope expression
    Ginger.IfS _ condition trueBranch falseBranch ->
      expressionVariables scope condition
        <> statementVariables scope trueBranch
        <> statementVariables scope falseBranch
    Ginger.SwitchS _ expression branches defaultBranch ->
      expressionVariables scope expression
        <> concatMap
          ( \(branchExpression, branchBody) -> expressionVariables scope branchExpression <> statementVariables scope branchBody
          )
          branches
        <> statementVariables scope defaultBranch
    Ginger.ForS _ maybeIndexName itemName expression body ->
      expressionVariables scope expression
        <> statementVariables (itemName : maybe scope (: scope) maybeIndexName) body
    Ginger.SetVarS _ _ expression -> expressionVariables scope expression
    Ginger.DefMacroS _ macroName Ginger.Macro {Ginger.macroArgs = args, Ginger.macroBody = body} ->
      statementVariables (macroName : args <> scope) body
    Ginger.BlockRefS _ variableName -> [variableName | variableName `notElem` scope]
    Ginger.PreprocessedIncludeS _ includedTemplate -> gingerTemplateVariables includedTemplate
    Ginger.NullS _ -> []
    Ginger.TryCatchS _ tryBody catchBlocks finallyBody ->
      statementVariables scope tryBody
        <> concatMap (catchBlockVariables scope) catchBlocks
        <> statementVariables scope finallyBody

catchBlockVariables :: [Ginger.VarName] -> Ginger.CatchBlock a -> [Text]
catchBlockVariables scope Ginger.Catch {Ginger.catchCaptureAs = captureName, Ginger.catchBody = body} =
  statementVariables (maybe scope (: scope) captureName) body

expressionVariables :: [Ginger.VarName] -> Ginger.Expression a -> [Text]
expressionVariables scope expression =
  case expression of
    Ginger.StringLiteralE _ _ -> []
    Ginger.NumberLiteralE _ _ -> []
    Ginger.BoolLiteralE _ _ -> []
    Ginger.NullLiteralE _ -> []
    Ginger.VarE _ variableName -> [variableName | variableName `notElem` scope]
    Ginger.ListE _ expressions -> concatMap (expressionVariables scope) expressions
    Ginger.ObjectE _ pairs ->
      concatMap
        ( \(keyExpression, valueExpression) -> expressionVariables scope keyExpression <> expressionVariables scope valueExpression
        )
        pairs
    Ginger.MemberLookupE _ objectExpression memberExpression ->
      expressionVariables scope objectExpression <> expressionVariables scope memberExpression
    Ginger.CallE _ callee arguments ->
      callableVariables scope callee
        <> concatMap (expressionVariables scope . snd) arguments
    Ginger.LambdaE _ args body -> expressionVariables (args <> scope) body
    Ginger.TernaryE _ condition trueExpression falseExpression ->
      expressionVariables scope condition
        <> expressionVariables scope trueExpression
        <> expressionVariables scope falseExpression
    Ginger.DoE _ body -> statementVariables scope body

callableVariables :: [Ginger.VarName] -> Ginger.Expression a -> [Text]
callableVariables _ (Ginger.VarE _ _) = []
callableVariables scope expression = expressionVariables scope expression

collectStatementVariables ::
  ([Text], [Ginger.VarName]) -> Ginger.Statement a -> ([Text], [Ginger.VarName])
collectStatementVariables (variableNames, scope) statement =
  (variableNames <> statementVariables scope statement, statementBindings scope statement)

statementBindings :: [Ginger.VarName] -> Ginger.Statement a -> [Ginger.VarName]
statementBindings scope statement =
  case statement of
    Ginger.SetVarS _ variableName _ -> variableName : scope
    Ginger.DefMacroS _ macroName _ -> macroName : scope
    _ -> scope

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

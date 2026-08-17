{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Langchain.PromptTemplate.Chat
Description : Chat prompt template primitives
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Minimal chat prompt primitives ported from LangChain Python chat prompts.
-}
module Langchain.PromptTemplate.Chat
  ( BaseStringMessagePromptTemplate (..)
  , BaseMessagePromptTemplate (..)
  , extractTemplateVariables
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model.Types (Message)

-- | Base class for message prompt templates backed by string prompt templates.
class BaseStringMessagePromptTemplate template options | template -> options where
  fromTemplateFile :: FilePath -> options -> IO template

-- | Base class for message prompt templates.
class BaseMessagePromptTemplate template input where
  formatMessages :: template -> input -> Either LangchainError [Message]

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

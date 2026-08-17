{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Langchain.OutputParser.Enum
Description : Fuzzy enum output parser for constrained categorical classifications
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Parses and validates LLM categorical responses against a predefined Haskell bounded enum.
-}
module Langchain.OutputParser.Enum
  ( EnumParser (..)
  , newEnumParser
  , parseEnum
  , parseEnumFuzzy
  ) where

import Data.Char (toLower)
import Data.List (find)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, parsingError)
import Langchain.OutputParser.Core (OutputParser (..))

-- | Enum parser container
data EnumParser a = EnumParser
  { enumOptions :: ![a]
  }

-- | Construct an EnumParser for any Bounded Enum type
newEnumParser :: forall a. (Bounded a, Enum a) => EnumParser a
newEnumParser = EnumParser [minBound .. maxBound]

-- | Parse text into an enum value with case-insensitive and whitespace tolerance
parseEnum :: forall a. (Bounded a, Enum a, Show a) => Text -> Either Text a
parseEnum = parseEnumFuzzy (Proxy :: Proxy a)

-- | Fuzzy match enum against all possible enum constructors
parseEnumFuzzy :: forall a. (Bounded a, Enum a, Show a) => Proxy a -> Text -> Either Text a
parseEnumFuzzy _ rawText =
  let allEnums = [minBound .. maxBound] :: [a]
      cleanInput = T.toLower (T.strip rawText)
      -- 1. Exact match
      exactMatch = find (\e -> cleanInput == T.toLower (T.pack (show e))) allEnums
      -- 2. Substring match
      subMatch = find (\e -> T.toLower (T.pack (show e)) `T.isInfixOf` cleanInput) allEnums
   in case exactMatch of
        Just val -> Right val
        Nothing -> case subMatch of
          Just val -> Right val
          Nothing ->
            let validOptions = T.intercalate ", " [T.pack (show e) | e <- allEnums]
             in Left $
                  "Invalid option '"
                    <> rawText
                    <> "'. Expected one of: ["
                    <> validOptions
                    <> "]"

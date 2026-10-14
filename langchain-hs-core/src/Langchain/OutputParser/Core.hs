{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Langchain.OutputParser.Core
Copyright:   (c) 2025-2026 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

Core output parser typeclass and standard instances for basic text transformations.
For typed JSON and schema-driven parsing, see "Langchain.OutputParser.Structured".
-}
module Langchain.OutputParser.Core
  ( -- * Typeclass
    OutputParser (..)

    -- * Parsers
  , CommaSeparatedList (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Read as TR

import Langchain.Error (LangchainResult, parsingError)

{- | Typeclass for parsing output from language models into specific types.
Instances of this class define how to convert a 'Text' output into a value of type @a@.
-}
class OutputParser a where
  {- | Parse the given text into a value of type @a@.
  Returns 'Left' with an error message if parsing fails, or 'Right' with the parsed value.
  -}
  parse :: Text -> LangchainResult a

-- | Identity parser returning the raw text output unchanged.
instance OutputParser Text where
  parse = Right

{- | Parse a boolean value from text using case-insensitive normalization.

Accepts @"true"@, @"t"@, @"yes"@, @"1"@ as 'True', and @"false"@, @"f"@, @"no"@, @"0"@ as 'False'.
-}
instance OutputParser Bool where
  parse txt =
    case T.toLower (T.strip txt) of
      "true" -> Right True
      "t" -> Right True
      "yes" -> Right True
      "1" -> Right True
      "false" -> Right False
      "f" -> Right False
      "no" -> Right False
      "0" -> Right False
      _ -> Left $ parsingError "Invalid boolean value" (Just "Bool") (Just txt)

-- | Represents a list of text items separated by commas.
newtype CommaSeparatedList = CommaSeparatedList [Text]
  deriving (Show, Eq)

instance OutputParser CommaSeparatedList where
  parse txt = Right $ CommaSeparatedList $ map T.strip $ T.splitOn "," txt

-- | Parse an 'Int' value from trimmed text output.
instance OutputParser Int where
  parse txt =
    case TR.readMaybe (T.unpack (T.strip txt)) of
      Just n -> Right n
      Nothing -> Left $ parsingError "Failed to parse Int from output" (Just "Int") (Just txt)

-- | Parse an 'Integer' value from trimmed text output.
instance OutputParser Integer where
  parse txt =
    case TR.readMaybe (T.unpack (T.strip txt)) of
      Just n -> Right n
      Nothing -> Left $ parsingError "Failed to parse Integer from output" (Just "Integer") (Just txt)

-- | Parse a 'Double' value from trimmed text output.
instance OutputParser Double where
  parse txt =
    case TR.readMaybe (T.unpack (T.strip txt)) of
      Just n -> Right n
      Nothing -> Left $ parsingError "Failed to parse Double from output" (Just "Double") (Just txt)

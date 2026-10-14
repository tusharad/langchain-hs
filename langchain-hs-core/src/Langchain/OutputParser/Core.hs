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

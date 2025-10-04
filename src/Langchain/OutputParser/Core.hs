{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Langchain.OutputParser.Core
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

This module provides the core types and instances for output parsers in the Langchain Haskell port.
Output parsers are used to transform the raw output from language models into structured data formats,
making it easier to work with the results in downstream applications.

The 'OutputParser' typeclass defines the interface for parsing model output into specific types,
and this module provides instances for common data structures such as booleans, lists, and JSON objects.

For more information on output parsers in the original Langchain library, see:
https://python.langchain.com/docs/concepts/output_parsers/
-}
module Langchain.OutputParser.Core
  ( -- * Typeclass
    OutputParser (..)

    -- * Parsers
  , CommaSeparatedList (..)
  , JSONOutputStructure (..)
  , NumberSeparatedList (..)
  ) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Internal.Search (indices)
import Langchain.Error (LangchainResult, parsingError)

{- | Typeclass for parsing output from language models into specific types.
Instances of this class define how to convert a 'Text' output into a value of type 'a'.
-}
class OutputParser a where
  {- | Parse the given text into a value of type 'a'.
  Returns 'Left' with an error message if parsing fails, or 'Right' with the parsed value.
  -}
  parse :: Text -> LangchainResult a

-- | Represents a list of text items separated by commas.
newtype CommaSeparatedList = CommaSeparatedList [Text]
  deriving (Show, Eq)

instance OutputParser Bool where
  -- \| Parse a boolean value from the text.
  -- The text is considered 'True' if it contains the word "true" (case-insensitive),
  -- and 'False' if it contains "false". Otherwise, parsing fails.
  --
  -- === Examples
  --
  -- \* Parsing "true":
  --
  -- @
  -- parse "true" :: Either String Bool == Right True
  -- @
  --
  -- \* Parsing "False":
  --
  -- @
  -- parse "False" :: Either String Bool == Right False
  -- @
  --
  -- \* Parsing invalid input:
  --
  -- @
  -- parse "yes" :: Either String Bool == Left "Invalid boolean value"
  -- @
  parse txt = do
    let txt' = T.strip $ T.toLower txt
    if not (null $ indices "true" txt')
      then
        Right True
      else
        if not (null $ indices "false" txt')
          then
            Right False
          else
            Left $ parsingError "Invalid boolean value" (Just "Bool") (Just txt)

instance OutputParser CommaSeparatedList where
  -- \| Parse a comma-separated list from the text.
  -- The text is split by commas, and each part is stripped of leading/trailing whitespace.
  --
  -- === Examples
  --
  -- \* Parsing an empty string:
  --
  -- @
  -- parse "" :: Either String CommaSeparatedList == Right (CommaSeparatedList [""])
  -- @
  --
  -- \* Parsing a single item:
  --
  -- @
  -- parse "item" :: Either String CommaSeparatedList == Right (CommaSeparatedList ["item"])
  -- @
  --
  -- \* Parsing multiple items:
  --
  -- @
  -- parse "item1,item2,item3" :: Either String CommaSeparatedList == Right (CommaSeparatedList ["item1", "item2", "item3"])
  -- @
  parse txt = Right $ CommaSeparatedList $ map T.strip $ T.splitOn "," txt

{- | JSON parser wrapper
Requires 'FromJSON' instance for target type. Uses Aeson for parsing.

Example data type:

@
data Person = Person
  { name :: Text
  , age :: Int
  } deriving (Show, Eq, FromJSON)
@

Usage:

>>> parse "{\"name\": \"Bob\", \"age\": 25}" :: Either String (JSONOutputStructure Person)
Right (JSONOutputStructure {jsonValue = Person {name = "Bob", age = 25}})
-}
newtype FromJSON a => JSONOutputStructure a = JSONOutputStructure
  { jsonValue :: a
  }
  deriving (Show, Eq, FromJSON)

-- | Instance for parsing JSON into any type that implements FromJSON.
instance FromJSON a => OutputParser (JSONOutputStructure a) where
  parse txt =
    case eitherDecode (fromStrict $ encodeUtf8 txt) of
      Left err ->
        Left $ parsingError ("JSON parsing error: " <> T.pack err) (Just "JSONOutputStructure") (Just txt)
      Right val -> Right val

-- | Represents a list of text items separated by numbered prefixes, like "1. First item".
newtype NumberSeparatedList = NumberSeparatedList [Text]
  deriving (Show, Eq)

instance OutputParser NumberSeparatedList where
  -- \| Parse a numbered list from the text.
  -- The input is expected to be a list of items prefixed with numbers followed by dots,
  -- such as "1. First item\n2. Second item". Whitespace is trimmed from each item.
  --
  -- === Examples
  --
  -- \* Parsing a simple numbered list:
  --
  -- @
  -- parse "1. First item\n2. Second item\n3. Third item" :: Either String NumberSeparatedList == Right (NumberSeparatedList ["First item", "Second item", "Third item"])
  -- @
  --
  -- \* Handling whitespace:
  --
  -- @
  -- parse "1.   First item  \n  2.  Second item\n3. Third item" :: Either String NumberSeparatedList == Right (NumberSeparatedList ["First item", "Second item", "Third item"])
  -- @
  --
  -- \* Handling multi-digit numbers:
  --
  -- @
  -- parse "10. First item\n11. Second item\n12. Third item" :: Either String NumberSeparatedList == Right (NumberSeparatedList ["First item", "Second item", "Third item"])
  -- @
  parse txt =
    let s = trim (T.unpack txt)
     in case dropUntilAndConsumeBoundary s of
          Nothing -> Left $ parsingError "No valid numbered items found" (Just "NumberSeparatedList") (Just txt)
          Just rest ->
            -- Parse the rest into items and wrap them in our newtype.
            Right . NumberSeparatedList . map (T.pack . trim) $
              parseItems rest

{- | Drops noise until we find a valid boundary marker (number with dot)
and then consumes the marker.
-}
dropUntilAndConsumeBoundary :: String -> Maybe String
dropUntilAndConsumeBoundary s =
  case findBoundary s of
    Nothing -> Nothing
    Just (idx, n) -> Just (drop (idx + n) s)

{- | Scans the string for the next occurrence of a valid boundary.
Returns the index and length of the marker.
-}
findBoundary :: String -> Maybe (Int, Int)
findBoundary = go 0
  where
    go _ [] = Nothing
    go i xs@(_ : rest) =
      case isBoundaryAt xs of
        Just n -> Just (i, n)
        Nothing -> go (i + 1) rest

{- | Checks if the given string starts with a valid boundary.
A valid boundary has one or more digits, optional spaces,
a dot, then optional spaces. Returns the number of characters
consumed by the boundary if matched.
-}
isBoundaryAt :: String -> Maybe Int
isBoundaryAt s =
  let (digits, rest1) = span isDigit s
   in if null digits
        then Nothing
        else
          let (spaces, rest2) = span isSpace rest1
           in case rest2 of
                (c : rest3)
                  | c == '.' ->
                      let (spaces2, _) = span isSpace rest3
                       in Just
                            ( length digits
                                + length spaces
                                + 1
                                + length spaces2
                            )
                _ -> Nothing

-- | Recursively splits the string into items using the boundary markers.
parseItems :: String -> [String]
parseItems s =
  case findBoundary s of
    Nothing -> [s] -- No further boundaries; the rest is one item.
    Just (idx, n) ->
      let item = take idx s
          rest = drop (idx + n) s
       in item : parseItems rest

-- | A simple trim function to remove leading and trailing whitespace.
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

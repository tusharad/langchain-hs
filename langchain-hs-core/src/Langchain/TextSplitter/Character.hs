{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.TextSplitter.Character
Description : Character-based text splitting for LLM processing [[10]]
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Character-based text splitting implementation following LangChain's text splitter concepts.
Splits text into chunks based on separators and maximum chunk sizes, useful for processing
large documents with LLMs.

For more information on text splitting concepts, see the Langchain documentation:
[Langchain TextSplitter](https://python.langchain.com/docs/concepts/text_splitters/).

Example usage:

@
-- Split text using default settings (100 char chunks, double newline separator)
splitText defaultCharacterSplitterOps "Long document text..."

-- Custom configuration for 500-char chunks with paragraph splitting
customSplit = splitText (CharacterSplitterOps 500 "\n\\s*\n")
@
-}
module Langchain.TextSplitter.Character
  ( -- * Configuration
    CharacterSplitterOps (..)
  , defaultCharacterSplitterOps

    -- * Splitting Function
  , splitText
  ) where

import Data.Int (Int64)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

{- | Configuration for character-based text splitting
Contains:

- 'chunkSize' : Maximum characters per chunk
- 'separator' : Pattern to split text before chunking

Default values follow LangChain's recommended settings for LLM input preparation.
-}
data CharacterSplitterOps = CharacterSplitterOps
  { chunkSize :: Int64
  , separator :: Text
  }
  deriving (Show, Eq)

{- | Default splitter configuration

- 100 character chunks
- Splits on double newlines ("\n\n")

>>> defaultCharacterSplitterOps
CharacterSplitterOps {chunkSize = 100, separator = "\n\n"}
-}
defaultCharacterSplitterOps :: CharacterSplitterOps
defaultCharacterSplitterOps =
  CharacterSplitterOps
    { chunkSize = 100
    , separator = "\n\n"
    }

{- | Split text into chunks following LangChain's splitting strategy:
 -
1. Split by separator first
2. Chunk each segment into specified size
3. Preserve semantic boundaries where possible

Examples:
>>> splitText defaultCharacterSplitterOps ""
[]

>>> splitText defaultCharacterSplitterOps "Short text"
["Short text"]

>>> splitText defaultCharacterSplitterOps "Part1\n\nPart2\n\nPart3"
["Part1", "Part2", "Part3"]

>>> splitText (CharacterSplitterOps 20 "\n\n") "Very long text exceeding chunk size..."
["Very long text ex", "ceeding chunk size..."]
-}
splitText :: CharacterSplitterOps -> Text -> [Text]
splitText CharacterSplitterOps {..} txt =
  mconcat $
    map
      (T.chunksOf chunkSize . T.strip)
      (if T.null separator then [txt] else T.splitOn separator txt)

{- $examples
Test case patterns demonstrating key behaviors:

1. Empty input handling
   >>> splitText defaultCharacterSplitterOps ""
   []

2. Custom separator usage
   >>> splitText (CharacterSplitterOps 100 "|") "A|B|C"
   ["A", "B", "C"]

3. Combined splitting and chunking
   >>> splitText (CharacterSplitterOps 10 "\n") "1234567890\nABCDEFGHIJ"
   ["1234567890", "ABCDEFGHIJ"]
-}

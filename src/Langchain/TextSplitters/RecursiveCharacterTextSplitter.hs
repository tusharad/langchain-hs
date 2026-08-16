{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Langchain.TextSplitters.RecursiveCharacterTextSplitter
-- Description : Recursive character-based text splitting for LLM processing
-- Copyright   : (c) 2025 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-- Stability   : experimental
--
-- Recursive character-based text splitting following LangChain's text splitter concepts.
-- Splits text into chunks using an ordered list of separators, recursively falling back
-- to smaller separators until chunks satisfy the configured size.
--
-- For more information on text splitting concepts, see the Langchain documentation:
-- [Langchain TextSplitter](https://python.langchain.com/docs/concepts/text_splitters/).
--
-- Example usage:
--
-- @
-- -- Split text using default settings (100 char chunks, double newline separator)
-- splitText defaultCharacterSplitterOps "Long document text..."
--
-- -- Custom configuration for 500-char chunks with paragraph splitting
-- customSplit = splitText (CharacterSplitterOps 500 "\n\\s*\n")
-- @
module Langchain.TextSplitters.RecursiveCharacterTextSplitter
  ( -- * Configuration
    KeepSeparator (..)
  , Language (..)
  , RecursiveCharacterSplitterOps (..)
  , defaultRecursiveCharacterSplitterOps

    -- * Splitting Function
  , splitText
  , fromLanguage
  , getSeparatorsForLanguage
  )
where

import Data.Int (Int64)
import Data.Text.Lazy (Text)
import qualified Langchain.TextSplitters.TextSplitter as Core

-- | Controls whether separator text is kept in output chunks.
data KeepSeparator
  = KeepSeparatorNone
  -- ^ Drop separator text from output chunks.
  | KeepSeparatorStart
  -- ^ Keep separator text at the start of the following chunk.
  | KeepSeparatorEnd
  -- ^ Keep separator text at the end of the preceding chunk.
  deriving (Show, Eq)

-- | Programming and markup languages with language-specific separator lists.
data Language
  = C
  | CPP
  | GO
  | JAVA
  | KOTLIN
  | JS
  | TS
  | PHP
  | PROTO
  | PYTHON
  | R
  | RST
  | RUBY
  | ELIXIR
  | RUST
  | SCALA
  | SWIFT
  | MARKDOWN
  | LATEX
  | HTML
  | CSHARP
  | SOL
  | COBOL
  | LUA
  | HASKELL
  | POWERSHELL
  | VISUALBASIC6
  deriving (Show, Eq)

-- | Configuration for recursive character-based text splitting.
data RecursiveCharacterSplitterOps = RecursiveCharacterSplitterOps
  { chunkSize :: Int64
    -- ^ Maximum number of characters per chunk.
  , chunkOverlap :: Int64
    -- ^ Number of characters to overlap between adjacent chunks.
  , separators :: [Text]
    -- ^ Separators to try in order from strongest boundary to weakest.
  , keepSeparator :: KeepSeparator
    -- ^ How separator text should be preserved in output chunks.
  , isSeparatorRegex :: Bool
    -- ^ Whether separators should be interpreted as regular expressions.
  }
  deriving (Show, Eq)

-- | Default recursive character splitter configuration.
defaultRecursiveCharacterSplitterOps :: RecursiveCharacterSplitterOps
defaultRecursiveCharacterSplitterOps =
  RecursiveCharacterSplitterOps
    { chunkSize = 100
    , chunkOverlap = 0
    , separators = ["\n\n", "\n", " ", ""]
    , keepSeparator = KeepSeparatorStart
    , isSeparatorRegex = False
    }

-- | Split text into chunks using recursive separator fallback.
splitText :: RecursiveCharacterSplitterOps -> Text -> [Text]
splitText = splitTextRecursive

instance Core.TextSplitter RecursiveCharacterSplitterOps where
  splitText = splitTextRecursive

splitTextRecursive :: RecursiveCharacterSplitterOps -> Text -> [Text]
splitTextRecursive _ _ = []

-- | Configure a recursive splitter with separators appropriate for a language.
fromLanguage :: Language -> RecursiveCharacterSplitterOps -> RecursiveCharacterSplitterOps
fromLanguage _ ops = ops

-- | Return the separator list used for the given language.
getSeparatorsForLanguage :: Language -> [Text]
getSeparatorsForLanguage _ = []

-- $examples
-- Test case patterns demonstrating key behaviors:
--
-- 1. Empty input handling
--    >>> splitText defaultCharacterSplitterOps ""
--    []
--
-- 2. Custom separator usage
--    >>> splitText (CharacterSplitterOps 100 "|") "A|B|C"
--    ["A", "B", "C"]
--
-- 3. Combined splitting and chunking
--    >>> splitText (CharacterSplitterOps 10 "\n") "1234567890\nABCDEFGHIJ"
--    ["1234567890", "ABCDEFGHIJ"]

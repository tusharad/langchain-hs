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
    KeepSeparator (..),
    Language (..),
    RecursiveCharacterSplitterOps (..),
    defaultRecursiveCharacterSplitterOps,

    -- * Splitting Function
    splitText,
    fromLanguage,
    getSeparatorsForLanguage,
  )
where

import Data.Int (Int64)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Langchain.TextSplitters.TextSplitter as Core
import qualified Text.Regex.Pcre2 as Pcre

-- | Controls whether separator text is kept in output chunks.
data KeepSeparator
  = -- | Drop separator text from output chunks.
    KeepSeparatorNone
  | -- | Keep separator text at the start of the following chunk.
    KeepSeparatorStart
  | -- | Keep separator text at the end of the preceding chunk.
    KeepSeparatorEnd
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
  { -- | Maximum number of characters per chunk.
    chunkSize :: Int64,
    -- | Number of characters to overlap between adjacent chunks.
    chunkOverlap :: Int64,
    -- | Separators to try in order from strongest boundary to weakest.
    separators :: [Text],
    -- | How separator text should be preserved in output chunks.
    keepSeparator :: KeepSeparator,
    -- | Whether separators should be interpreted as regular expressions.
    isSeparatorRegex :: Bool
  }
  deriving (Show, Eq)

-- | Default recursive character splitter configuration.
defaultRecursiveCharacterSplitterOps :: RecursiveCharacterSplitterOps
defaultRecursiveCharacterSplitterOps =
  RecursiveCharacterSplitterOps
    { chunkSize = 100,
      chunkOverlap = 0,
      separators = ["\n\n", "\n", " ", ""],
      keepSeparator = KeepSeparatorStart,
      isSeparatorRegex = False
    }

-- | Split text into chunks using recursive separator fallback.
splitText :: RecursiveCharacterSplitterOps -> Text -> [Text]
splitText ops text =
  splitTextRecursive ops text (separators ops)

instance Core.TextSplitter RecursiveCharacterSplitterOps where
  splitText ops text =
    splitTextRecursive ops text (separators ops)
  chunkOverlapFor = chunkOverlap

splitTextRecursive :: RecursiveCharacterSplitterOps -> Text -> [Text] -> [Text]
splitTextRecursive ops text availableSeparators = mergeChunks [] [] splits
  where
    marker =
      until
        (not . T.isInfixOf text)
        (<> "\xE000")
        "\xE000"

    keepSep = keepSeparator ops

    splitLiteralText =
      T.replace separator replacement
      where
        replacement =
          case keepSep of
            KeepSeparatorNone -> marker
            KeepSeparatorStart -> marker <> separator
            KeepSeparatorEnd -> separator <> marker

    splitRegexText =
      T.fromStrict $
        Pcre.gsub
          (T.toStrict separator)
          (T.toStrict replacement)
          (T.toStrict text)
      where
        replacement =
          case keepSep of
            KeepSeparatorNone -> marker
            KeepSeparatorStart -> marker <> "$0"
            KeepSeparatorEnd -> "$0" <> marker

    separatorMatchesRegex sep =
      Pcre.matches (T.toStrict sep) (T.toStrict text)

    separatorMatchesLiteral sep =
      sep `T.isInfixOf` text

    (markedText, separatorMatches) =
      if isSeparatorRegex ops
        then (splitRegexText, separatorMatchesRegex)
        else (splitLiteralText text, separatorMatchesLiteral)

    (separator, nextSeparators) =
      case dropWhile (not . isSelectable) availableSeparators of
        [] -> ("", [])
        candidate : remaining -> (candidate, remaining)
      where
        isSelectable candidate =
          T.null candidate || separatorMatches candidate

    splits
      | T.null separator = T.singleton <$> T.unpack text
      | otherwise =
          filter (not . T.null) $
            T.splitOn marker markedText

    merge finalChunks goodSplits = finalChunks <> ss
      where
        ss =
          if null goodSplits
            then []
            else Core.mergeSplits (chunkSize ops) (chunkOverlap ops) mergeSeparator goodSplits
        mergeSeparator =
          case keepSep of
            KeepSeparatorNone -> separator
            _ -> ""

    mergeChunks finalChunks goodSplits [] = merge finalChunks goodSplits
    mergeChunks finalChunks goodSplits (split : rest)
      | T.length split < chunkSize ops =
          mergeChunks finalChunks (goodSplits <> [split]) rest
      | otherwise =
          let finalChunks' = merge finalChunks goodSplits
              nextChunks =
                if null nextSeparators
                  then [split]
                  else splitTextRecursive ops split nextSeparators
           in mergeChunks (finalChunks' <> nextChunks) [] rest

-- | Configure a recursive splitter with separators appropriate for a language.
fromLanguage :: Language -> RecursiveCharacterSplitterOps -> RecursiveCharacterSplitterOps
fromLanguage language ops =
  ops
    { separators = getSeparatorsForLanguage language,
      isSeparatorRegex = True
    }

-- | Return the separator list used for the given language.
getSeparatorsForLanguage :: Language -> [Text]
getSeparatorsForLanguage language =
  case language of
    C -> cLikeSeparators
    CPP -> cLikeSeparators
    GO ->
      [ "\nfunc ",
        "\nvar ",
        "\nconst ",
        "\ntype ",
        "\nif ",
        "\nfor ",
        "\nswitch ",
        "\ncase ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    JAVA ->
      [ "\nclass ",
        "\npublic ",
        "\nprotected ",
        "\nprivate ",
        "\nstatic ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\nswitch ",
        "\ncase ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    KOTLIN ->
      [ "\nclass ",
        "\npublic ",
        "\nprotected ",
        "\nprivate ",
        "\ninternal ",
        "\ncompanion ",
        "\nfun ",
        "\nval ",
        "\nvar ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\nwhen ",
        "\nelse ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    JS ->
      [ "\nfunction ",
        "\nconst ",
        "\nlet ",
        "\nvar ",
        "\nclass ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\nswitch ",
        "\ncase ",
        "\ndefault ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    TS ->
      [ "\nenum ",
        "\ninterface ",
        "\nnamespace ",
        "\ntype ",
        "\nclass ",
        "\nfunction ",
        "\nconst ",
        "\nlet ",
        "\nvar ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\nswitch ",
        "\ncase ",
        "\ndefault ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    PHP ->
      [ "\nfunction ",
        "\nclass ",
        "\nif ",
        "\nforeach ",
        "\nwhile ",
        "\ndo ",
        "\nswitch ",
        "\ncase ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    PROTO ->
      [ "\nmessage ",
        "\nservice ",
        "\nenum ",
        "\noption ",
        "\nimport ",
        "\nsyntax ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    PYTHON ->
      [ "\nclass ",
        "\ndef ",
        "\n\tdef ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    R ->
      [ "\nfunction ",
        "\nsetClass\\(",
        "\nsetMethod\\(",
        "\nsetGeneric\\(",
        "\nif ",
        "\nelse ",
        "\nfor ",
        "\nwhile ",
        "\nrepeat ",
        "\nlibrary\\(",
        "\nrequire\\(",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    RST ->
      [ "\n=+\n",
        "\n-+\n",
        "\n\\*+\n",
        "\n\n.. *\n\n",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    RUBY ->
      [ "\ndef ",
        "\nclass ",
        "\nif ",
        "\nunless ",
        "\nwhile ",
        "\nfor ",
        "\ndo ",
        "\nbegin ",
        "\nrescue ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    ELIXIR ->
      [ "\ndef ",
        "\ndefp ",
        "\ndefmodule ",
        "\ndefprotocol ",
        "\ndefmacro ",
        "\ndefmacrop ",
        "\nif ",
        "\nunless ",
        "\ncase ",
        "\ncond ",
        "\nwith ",
        "\nfor ",
        "\ndo ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    RUST ->
      [ "\nfn ",
        "\nconst ",
        "\nlet ",
        "\nif ",
        "\nwhile ",
        "\nfor ",
        "\nloop ",
        "\nmatch ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    SCALA ->
      [ "\nclass ",
        "\nobject ",
        "\ndef ",
        "\nval ",
        "\nvar ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\nmatch ",
        "\ncase ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    SWIFT ->
      [ "\nfunc ",
        "\nclass ",
        "\nstruct ",
        "\nenum ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\ndo ",
        "\nswitch ",
        "\ncase ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    MARKDOWN ->
      [ "\n#{1,6} ",
        "```\n",
        "\n\\*\\*\\*+\n",
        "\n---+\n",
        "\n___+\n",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    LATEX ->
      [ "\n\\\\chapter{",
        "\n\\\\section{",
        "\n\\\\subsection{",
        "\n\\\\subsubsection{",
        "\n\\\\begin{enumerate}",
        "\n\\\\begin{itemize}",
        "\n\\\\begin{description}",
        "\n\\\\begin{list}",
        "\n\\\\begin{quote}",
        "\n\\\\begin{quotation}",
        "\n\\\\begin{verse}",
        "\n\\\\begin{verbatim}",
        "\n\\\\begin{align}",
        "$$",
        "$",
        " ",
        ""
      ]
    HTML ->
      [ "<body",
        "<div",
        "<p",
        "<br",
        "<li",
        "<h1",
        "<h2",
        "<h3",
        "<h4",
        "<h5",
        "<h6",
        "<span",
        "<table",
        "<tr",
        "<td",
        "<th",
        "<ul",
        "<ol",
        "<header",
        "<footer",
        "<nav",
        "<head",
        "<style",
        "<script",
        "<meta",
        "<title",
        ""
      ]
    CSHARP ->
      [ "\ninterface ",
        "\nenum ",
        "\ndelegate ",
        "\nevent ",
        "\nclass ",
        "\nabstract ",
        "\npublic ",
        "\nprotected ",
        "\nprivate ",
        "\nstatic ",
        "\nreturn ",
        "\nif ",
        "\ncontinue ",
        "\nfor ",
        "\nforeach ",
        "\nwhile ",
        "\nswitch ",
        "\nbreak ",
        "\ncase ",
        "\nelse ",
        "\ntry ",
        "\nthrow ",
        "\nfinally ",
        "\ncatch ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    SOL ->
      [ "\npragma ",
        "\nusing ",
        "\ncontract ",
        "\ninterface ",
        "\nlibrary ",
        "\nconstructor ",
        "\ntype ",
        "\nfunction ",
        "\nevent ",
        "\nmodifier ",
        "\nerror ",
        "\nstruct ",
        "\nenum ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\ndo while ",
        "\nassembly ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    COBOL ->
      [ "\nIDENTIFICATION DIVISION.",
        "\nENVIRONMENT DIVISION.",
        "\nDATA DIVISION.",
        "\nPROCEDURE DIVISION.",
        "\nWORKING-STORAGE SECTION.",
        "\nLINKAGE SECTION.",
        "\nFILE SECTION.",
        "\nINPUT-OUTPUT SECTION.",
        "\nOPEN ",
        "\nCLOSE ",
        "\nREAD ",
        "\nWRITE ",
        "\nIF ",
        "\nELSE ",
        "\nMOVE ",
        "\nPERFORM ",
        "\nUNTIL ",
        "\nVARYING ",
        "\nACCEPT ",
        "\nDISPLAY ",
        "\nSTOP RUN.",
        "\n",
        " ",
        ""
      ]
    LUA ->
      [ "\nlocal ",
        "\nfunction ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\nrepeat ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    HASKELL ->
      [ "\nmain :: ",
        "\nmain = ",
        "\nlet ",
        "\nin ",
        "\ndo ",
        "\nwhere ",
        "\n:: ",
        "\n= ",
        "\ndata ",
        "\nnewtype ",
        "\ntype ",
        "\nmodule ",
        "\nimport ",
        "\nqualified ",
        "\nimport qualified ",
        "\nclass ",
        "\ninstance ",
        "\ncase ",
        "\n| ",
        "\n= {",
        "\n, ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    POWERSHELL ->
      [ "\nfunction ",
        "\nparam ",
        "\nif ",
        "\nforeach ",
        "\nfor ",
        "\nwhile ",
        "\nswitch ",
        "\nclass ",
        "\ntry ",
        "\ncatch ",
        "\nfinally ",
        "\n\n",
        "\n",
        " ",
        ""
      ]
    VISUALBASIC6 ->
      let visibility = "(?:Public|Private|Friend|Global|Static)\\s+"
       in [ "\n(?!End\\s)" <> visibility <> "?Sub\\s+",
            "\n(?!End\\s)" <> visibility <> "?Function\\s+",
            "\n(?!End\\s)" <> visibility <> "?Property\\s+(?:Get|Let|Set)\\s+",
            "\n(?!End\\s)" <> visibility <> "?Type\\s+",
            "\n(?!End\\s)" <> visibility <> "?Enum\\s+",
            "\n(?!End\\s)If\\s+",
            "\nElseIf\\s+",
            "\nElse\\s+",
            "\nSelect\\s+Case\\s+",
            "\nCase\\s+",
            "\nFor\\s+",
            "\nDo\\s+",
            "\nWhile\\s+",
            "\nWith\\s+",
            "\n\n",
            "\n",
            " ",
            ""
          ]
  where
    cLikeSeparators =
      [ "\nclass ",
        "\nvoid ",
        "\nint ",
        "\nfloat ",
        "\ndouble ",
        "\nif ",
        "\nfor ",
        "\nwhile ",
        "\nswitch ",
        "\ncase ",
        "\n\n",
        "\n",
        " ",
        ""
      ]

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

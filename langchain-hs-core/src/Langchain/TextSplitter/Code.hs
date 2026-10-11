{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.TextSplitter.Code
Description : Language-aware code text splitter
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Splits programming language source code at top-level declarations, class/function
boundaries, or language-specific constructs.
-}
module Langchain.TextSplitter.Code
  ( Language (..)
  , CodeSplitterOps (..)
  , defaultCodeSplitterOps
  , languageSeparators
  , splitCode
  ) where

import Data.Int (Int64)
import Data.Text.Lazy (Text)

import Langchain.TextSplitter.RecursiveCharacter
  ( RecursiveCharacterSplitterOps (..)
  , splitTextRecursive
  )

-- | Supported programming languages for code splitting
data Language
  = Haskell
  | Python
  | JavaScript
  | TypeScript
  | Rust
  | Go
  | Java
  | Cpp
  | CSharp
  | MarkdownCode
  deriving (Show, Eq, Enum, Bounded)

-- | Configuration options for code splitting
data CodeSplitterOps = CodeSplitterOps
  { codeLanguage :: Language
  , codeChunkSize :: Int64
  , codeChunkOverlap :: Int64
  }
  deriving (Show, Eq)

-- | Return language-specific separator hierarchy
languageSeparators :: Language -> [Text]
languageSeparators Haskell =
  [ "\nmodule "
  , "\ndata "
  , "\nnewtype "
  , "\ntype "
  , "\nclass "
  , "\ninstance "
  , "\n\n"
  , "\n"
  , " "
  , ""
  ]
languageSeparators Python =
  [ "\nclass "
  , "\ndef "
  , "\n\tdef "
  , "\n\n"
  , "\n"
  , " "
  , ""
  ]
languageSeparators JavaScript =
  [ "\nfunction "
  , "\nclass "
  , "\nexport default "
  , "\nexport const "
  , "\nconst "
  , "\nlet "
  , "\nvar "
  , "\n\n"
  , "\n"
  , " "
  , ""
  ]
languageSeparators TypeScript = languageSeparators JavaScript
languageSeparators Rust =
  [ "\nfn "
  , "\npub fn "
  , "\nstruct "
  , "\npub struct "
  , "\nenum "
  , "\npub enum "
  , "\nimpl "
  , "\ntrait "
  , "\n\n"
  , "\n"
  , " "
  , ""
  ]
languageSeparators Go =
  [ "\nfunc "
  , "\ntype "
  , "\n\n"
  , "\n"
  , " "
  , ""
  ]
languageSeparators Java =
  [ "\npublic class "
  , "\nclass "
  , "\npublic interface "
  , "\ninterface "
  , "\npublic enum "
  , "\npublic "
  , "\nprivate "
  , "\nprotected "
  , "\n\n"
  , "\n"
  , " "
  , ""
  ]
languageSeparators Cpp =
  [ "\nclass "
  , "\nstruct "
  , "\nenum "
  , "\ntemplate "
  , "\n\n"
  , "\n"
  , " "
  , ""
  ]
languageSeparators CSharp = languageSeparators Java
languageSeparators MarkdownCode =
  [ "\n# "
  , "\n## "
  , "\n### "
  , "\n#### "
  , "\n```"
  , "\n\n"
  , "\n"
  , " "
  , ""
  ]

-- | Default code splitter options for a language
defaultCodeSplitterOps :: Language -> CodeSplitterOps
defaultCodeSplitterOps lang =
  CodeSplitterOps
    { codeLanguage = lang
    , codeChunkSize = 1000
    , codeChunkOverlap = 150
    }

-- | Split code using language-specific syntax separators
splitCode :: CodeSplitterOps -> Text -> [Text]
splitCode ops text =
  let seps = languageSeparators (codeLanguage ops)
      recOps =
        RecursiveCharacterSplitterOps
          { chunkSize = codeChunkSize ops
          , chunkOverlap = codeChunkOverlap ops
          , separators = seps
          }
   in splitTextRecursive recOps text

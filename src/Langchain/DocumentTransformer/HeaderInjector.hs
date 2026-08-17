{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.DocumentTransformer.HeaderInjector
Description : Chunk Header Injection Transformer
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Injects structured metadata headers (e.g., Document title, section breadcrumbs, source URL)
directly into the textual content of document chunks. This dramatically improves dense and sparse
retrieval precision in enterprise RAG systems.
-}
module Langchain.DocumentTransformer.HeaderInjector
  ( HeaderInjector (..)
  , newHeaderInjector
  , injectChunkHeader
  , injectChunkHeaders
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.DocumentTransformer.MetadataEnricher (DocumentTransformer (..))

-- | Header Injector transformer configuration
data HeaderInjector = HeaderInjector
  { targetMetaKeys :: ![Text]
  , headerPrefix :: !Text
  , headerSuffix :: !Text
  }
  deriving (Show, Eq)

-- | Construct a default Header Injector targeting title and section
newHeaderInjector :: [Text] -> HeaderInjector
newHeaderInjector keys =
  HeaderInjector
    { targetMetaKeys = keys
    , headerPrefix = "=== Context Header ===\n"
    , headerSuffix = "\n=== Content ===\n"
    }

instance DocumentTransformer HeaderInjector where
  transformDocuments injector docs = pure $ injectChunkHeaders (targetMetaKeys injector) docs

-- | Prepend formatted metadata headers to a single document's content
injectChunkHeader :: [Text] -> Document -> Document
injectChunkHeader keys doc@Document {..} =
  let headerParts = [k <> ": " <> formatVal v | k <- keys, Just v <- [Map.lookup k metadata]]
   in if null headerParts
        then doc
        else
          let headerText = "=== [Header: " <> T.intercalate " | " headerParts <> "] ===\n"
              newContent = TL.fromStrict headerText <> pageContent
           in doc {pageContent = newContent}
  where
    formatVal (String s) = s
    formatVal (Number n) = T.pack (show n)
    formatVal (Bool b) = T.pack (show b)
    formatVal Null = "null"
    formatVal (Array _) = "[array]"
    formatVal (Object _) = "{object}"

-- | Prepend formatted metadata headers to a list of documents
injectChunkHeaders :: [Text] -> [Document] -> [Document]
injectChunkHeaders keys = map (injectChunkHeader keys)

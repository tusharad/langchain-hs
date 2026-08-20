{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.DocumentLoader.PdfLoader
Description : A PDF loader that extracts documents from PDF files.
Copyright   : (C) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

PDF loader using pdf-toolbox-document.
-}
module Langchain.DocumentLoader.PdfLoader
  ( PdfLoader (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.Map (fromList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Pdf.Document hiding (Document)
import System.Directory (doesFileExist)

import Langchain.Core.Error (documentLoaderError)
import Langchain.DocumentLoader.Core
import Langchain.TextSplitter.Character

readPdf :: FilePath -> IO (Either SomeException [Document])
readPdf fPath = try $ do
  withPdfFile fPath $ \pdf -> do
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    textList <-
      sequence
        [ pageExtractText =<< pageNodePageByNum rootNode i
        | i <- [0 .. count - 1]
        ]
    pure $
      zipWith
        ( \content pageNum ->
            Document
              { pageContent = content
              , metadata =
                  fromList
                    [ ("page number", Number $ fromIntegral pageNum)
                    ]
              }
        )
        (map TL.fromStrict textList)
        [1 .. count]

-- | PDF file loader
newtype PdfLoader = PdfLoader FilePath
  deriving (Eq, Show)

instance BaseLoader PdfLoader where
  load (PdfLoader path) = do
    exists <- liftIO $ doesFileExist path
    if exists
      then do
        eRes <- liftIO $ readPdf path
        case eRes of
          Left err ->
            throwError $
              documentLoaderError
                (T.pack $ "Failed to parse PDF " ++ path ++ ": " ++ show err)
                (Just "PdfLoader")
                (Just $ T.pack path)
          Right docs -> pure docs
      else
        throwError $
          documentLoaderError
            (T.pack $ "File not found: " ++ path)
            (Just "PdfLoader")
            (Just $ T.pack path)

  loadAndSplit (PdfLoader path) = do
    docs <- load (PdfLoader path)
    pure $ splitText defaultCharacterSplitterOps (pageContent $ mconcat docs)

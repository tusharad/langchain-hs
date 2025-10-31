{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.DocumentLoader.PdfLoader
Description : A PDF loader that extracts documents from PDF files.
Copyright   : (C) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides a loader for loading PDF files.
-}
module Langchain.DocumentLoader.PdfLoader
  ( PdfLoader (..)
  ) where

import Data.Aeson
import Data.Map (fromList)
import qualified Data.Text.Lazy as TL
import Langchain.DocumentLoader.Core
import Langchain.Error (llmError)
import Langchain.TextSplitter.Character
import Langchain.Utils (showText)
import Pdf.Document hiding (Document)
import System.Directory (doesFileExist)

-- TODO: Need some error handling for this function

{- |
An internal function
Reads a PDF file and extracts a list of 'Document's, one per page.

This function opens the PDF file at the specified 'FilePath' and uses
the Pdf.Document library to extract the text from each page. Each page's
content is wrapped in a 'Document' along with metadata indicating the page number.

Note: This function currently has minimal error handling. Improvements may be
required to properly handle various PDF parsing errors.

@param fPath The file path to the PDF file.
@return An IO action yielding a list of 'Document's extracted from the PDF.
-}
readPdf :: FilePath -> IO [Document]
readPdf fPath = do
  withPdfFile fPath $ \pdf -> do
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    textList <-
      sequence
        [ pageExtractText
            =<< pageNodePageByNum rootNode i
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

{- |
A loader for PDF files.

The 'PdfLoader' data type encapsulates a 'FilePath' pointing to a PDF document.
It implements the 'BaseLoader' interface to provide methods for loading and
splitting PDF content.
-}
newtype PdfLoader = PdfLoader FilePath

instance BaseLoader PdfLoader where
  -- \|
  --  Loads all pages from the PDF file specified by the 'PdfLoader'.
  --
  --  This function first checks whether the file exists. If it does, it uses
  --  'readPdf' to extract the content of each page as a separate 'Document'. If
  --  the file is not found, an appropriate error message is returned.
  --
  --  @param loader A 'PdfLoader' containing the file path to the PDF.
  --  @return An IO action yielding either an error message or a list of 'Document's.
  --
  load (PdfLoader path) = do
    exists <- doesFileExist path
    if exists
      then do
        content <- readPdf path
        return $ Right content
      else
        return $
          Left $
            llmError (showText $ "File not found: " ++ path) Nothing Nothing

  -- \|
  --  Loads the raw content of the PDF file and splits it using a character splitter.
  --
  --  This method reads the entire pdf as text and applies
  --  'splitText' with default recursive character options to divide the text into chunks.
  --  This approach is useful when only a simple text split is required rather than structured
  --  page extraction.
  --
  --  @param loader A 'PdfLoader' containing the file path to the PDF.
  --  @return An IO action yielding either an error message or a list of text chunks.
  --
  loadAndSplit (PdfLoader path) = do
    exists <- doesFileExist path
    if exists
      then do
        documents <- readPdf path
        return $
          Right $
            splitText
              defaultCharacterSplitterOps
              (pageContent $ mconcat documents)
      else
        return $
          Left $
            llmError (showText $ "File not found: " ++ path) Nothing Nothing

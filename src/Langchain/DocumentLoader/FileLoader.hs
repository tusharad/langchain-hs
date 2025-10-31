{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.DocumentLoader.FileLoader
Description : File loading implementation for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

File-based document loader implementation following LangChain's document loading patterns
Integrates with the core document splitting functionality for processing text files.

Example usage:

@
-- Load a document from file
loader = FileLoader "data.txt"
docs <- load loader
-- Right [Document {pageContent = "File content", metadata = ...}]

-- Load and split document content
chunks <- loadAndSplit loader
-- Right ["First paragraph", "Second paragraph", ...]
@
-}
module Langchain.DocumentLoader.FileLoader
  ( FileLoader (..)
  ) where

import Data.Aeson
import Data.Map (fromList)
import qualified Data.Text.Lazy as T
import Langchain.DocumentLoader.Core
import Langchain.Error (SomeException, llmError, try)
import Langchain.TextSplitter.Character
import System.Directory (doesFileExist)

{- | File loader configuration
Specifies the file path to load documents from.

Example:

>>> FileLoader "docs/example.txt"
FileLoader "docs/example.txt"
-}
newtype FileLoader = FileLoader FilePath

instance BaseLoader FileLoader where
  -- \| Load document with file source metadata
  --
  --  Example:

  --  >>> load (FileLoader "test.txt")
  --  Right [Document {pageContent = "Test content", metadata = fromList [("source", "test.txt")]}]
  --
  load (FileLoader path) = do
    exists <- doesFileExist path
    if exists
      then do
        eContent <- try $ readFile path
        case eContent of
          Left err ->
            return $
              Left $
                llmError
                  (T.toStrict $ T.pack $ "Error reading file: " ++ path ++ show (err :: SomeException))
                  Nothing
                  Nothing
          Right content -> do
            let meta = fromList [("source", String $ T.toStrict $ T.pack path)]
            return $ Right [Document (T.pack content) meta]
      else
        return $
          Left
            ( llmError
                (T.toStrict $ T.pack $ "File not found: " ++ path)
                Nothing
                Nothing
            )

  -- \| Load and split content using default character splitter
  --
  --  Example:

  --  >>> loadAndSplit (FileLoader "split.txt")
  --  Right ["Paragraph 1", "Paragraph 2", ...]
  --
  loadAndSplit (FileLoader path) = do
    exists <- doesFileExist path
    if exists
      then do
        eContent <- try $ readFile path
        case eContent of
          Left err ->
            return $
              Left $
                llmError
                  (T.toStrict $ T.pack $ "Error reading file: " ++ path ++ show (err :: SomeException))
                  Nothing
                  Nothing
          Right content -> return $ Right $ splitText defaultCharacterSplitterOps (T.pack content)
      else
        return $
          Left
            ( llmError
                (T.toStrict $ T.pack $ "File not found: " ++ path)
                Nothing
                Nothing
            )

{- $examples
Test case patterns:
1. Successful load with metadata
   >>> withTestFile "Content" $ \path -> load (FileLoader path)
   Right [Document {pageContent = "Content", metadata = ...}]

2. Error handling for missing files
   >>> load (FileLoader "missing.txt")
   Left "File not found: missing.txt"

3. Content splitting with default parameters
   >>> withTestFile "A\n\nB\n\nC" $ \path -> loadAndSplit (FileLoader path)
   Right ["A", "B", "C"]
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.DocumentLoader.DirectoryLoader
Description : Directory loading implementation for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

DirectoryLoader document loader implements functionality for reading files from disk into Documents
-}
module Langchain.DocumentLoader.DirectoryLoader
  ( -- * Directory loader
    DirectoryLoader (..)
  , DirectoryLoaderOptions (..)

    -- * Default functions
  , defaultDirectoryLoaderOptions
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.FileLoader (FileLoader (FileLoader))
import Langchain.DocumentLoader.PdfLoader (PdfLoader (PdfLoader))
import Langchain.Error (LangchainError, llmError)
import Langchain.TextSplitter.Character
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))

-- | Options for directory loading behavior
data DirectoryLoaderOptions = DirectoryLoaderOptions
  { recursiveDepth :: Maybe Int
  -- ^ Nothing = unlimited depth, Just 0 = No recursive, Just 3 = 3 level deep
  , extensions :: [String]
  -- ^ File extensions to include (e.g., [".txt", ".md"])
  , excludeHidden :: Bool
  -- ^ Whether to exclude hidden files (starting with '.')
  , useMultithreading :: Bool
  -- ^ Whether to use multithreading when loading files
  }
  deriving (Eq, Show)

-- | Default directory loader options
defaultDirectoryLoaderOptions :: DirectoryLoaderOptions
defaultDirectoryLoaderOptions =
  DirectoryLoaderOptions
    { recursiveDepth = Nothing
    , extensions = [] -- Empty list means all files
    , excludeHidden = True
    , useMultithreading = False
    }

{- | Directory loader configuration
Specifies the path to load documents from.

Example:

>>> DirectoryLoader "langchain-hs/src" defaultDirectoryLoaderOptions
-}
data DirectoryLoader = DirectoryLoader
  { dirPath :: FilePath
  , directoryLoaderOptions :: DirectoryLoaderOptions
  }
  deriving (Eq, Show)

-- | Helper to check if a file should be included based on options
shouldIncludeFile :: DirectoryLoaderOptions -> FilePath -> Bool
shouldIncludeFile opts path =
  let ext = takeExtension path
      fName = takeFileName path
      isHidden = listToMaybe fName == Just '.'
      matchesExt = null (extensions opts) || ext `elem` extensions opts
      passesHiddenCheck = not (excludeHidden opts) || not isHidden
   in matchesExt && passesHiddenCheck

-- | Get all files in a directory, with controlled recursion
getFilesInDirectory :: DirectoryLoaderOptions -> Int -> FilePath -> IO [FilePath]
getFilesInDirectory opts currentDepth dir = do
  -- Check if we've reached max depth (if specified)
  let canRecurse = case recursiveDepth opts of
        Nothing -> True
        Just maxD -> currentDepth < maxD

  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries

  -- Find all files in current directory
  files <- filterM doesFileExist fullPaths
  let filteredFiles = filter (shouldIncludeFile opts) files

  -- If we can recurse deeper and recursion is enabled, process subdirectories
  subFiles <-
    if canRecurse
      then do
        subdirs <- filterM doesDirectoryExist fullPaths
        -- Skip hidden directories if excludeHidden is set
        let visibleSubdirs =
              if excludeHidden opts
                then filter (\d -> not (null d) && listToMaybe d /= Just '.') subdirs
                else subdirs

        -- Process subdirectories (potentially in parallel)
        if useMultithreading opts && not (null visibleSubdirs)
          then
            concat
              <$> mapConcurrently
                (getFilesInDirectory opts (currentDepth + 1))
                visibleSubdirs
          else concat <$> mapM (getFilesInDirectory opts (currentDepth + 1)) visibleSubdirs
      else return []

  return $ filteredFiles ++ subFiles

loadFileToDocument :: FilePath -> IO (Either LangchainError [Document])
loadFileToDocument path = do
  exists <- doesFileExist path
  if not exists
    then
      return $
        Left
          ( llmError
              (T.pack $ "File does not exist: " ++ path)
              Nothing
              Nothing
          )
    else do
      -- if file is pdf then read it using PdfLoader else use fileLoader
      if takeExtension path == ".pdf"
        then
          load (PdfLoader path)
        else
          load (FileLoader path)

instance BaseLoader DirectoryLoader where
  load DirectoryLoader {..} = do
    exists <- doesDirectoryExist dirPath
    if exists
      then do
        filePaths <- getFilesInDirectory directoryLoaderOptions 0 dirPath
        -- Process files (using multithreading if enabled)
        docs <-
          if useMultithreading directoryLoaderOptions && not (null filePaths)
            then mapConcurrently loadFileToDocument filePaths
            else mapM loadFileToDocument filePaths
        -- Separate successes and failures
        let (errors, documents) = foldr separateResults ([], []) docs

        -- Return documents or combined error message
        case listToMaybe errors of
          Nothing -> return $ Right documents
          Just err -> return $ Left err
      else
        return $
          Left $
            llmError (T.pack $ "Directory does not exist: " ++ dirPath) Nothing Nothing
    where
      separateResults (Left err) (errs, docs) = (err : errs, docs)
      separateResults (Right doc) (errs, docs) = (errs, doc <> docs)

  loadAndSplit dirLoader = do
    eRes <- load dirLoader
    case eRes of
      Left e -> pure $ Left e
      Right documents ->
        pure $
          Right $
            splitText
              defaultCharacterSplitterOps
              (pageContent $ mconcat documents)

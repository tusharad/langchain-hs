{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.DocumentLoader.DirectoryLoader
Description : Directory loading implementation for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

DirectoryLoader document loader reads files from disk into Documents.
-}
module Langchain.DocumentLoader.DirectoryLoader
  ( DirectoryLoader (..)
  , DirectoryLoaderOptions (..)
  , defaultDirectoryLoaderOptions
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (filterM, forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))

import Langchain.Core.Error (documentLoaderError)
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.FileLoader (FileLoader (FileLoader))
import Langchain.DocumentLoader.PdfLoader (PdfLoader (PdfLoader))
import Langchain.TextSplitter.Character

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
    , extensions = []
    , excludeHidden = True
    , useMultithreading = False
    }

-- | Directory loader configuration
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
  let canRecurse = case recursiveDepth opts of
        Nothing -> True
        Just maxD -> currentDepth < maxD

  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries

  files <- filterM doesFileExist fullPaths
  let filteredFiles = filter (shouldIncludeFile opts) files

  subFiles <-
    if canRecurse
      then do
        subdirs <- filterM doesDirectoryExist fullPaths
        let visibleSubdirs =
              if excludeHidden opts
                then filter (\d -> not (null d) && listToMaybe d /= Just '.') subdirs
                else subdirs

        if useMultithreading opts && not (null visibleSubdirs)
          then
            concat
              <$> mapConcurrently
                (getFilesInDirectory opts (currentDepth + 1))
                visibleSubdirs
          else concat <$> mapM (getFilesInDirectory opts (currentDepth + 1)) visibleSubdirs
      else pure []

  pure $ filteredFiles ++ subFiles

instance BaseLoader DirectoryLoader where
  load DirectoryLoader {..} = do
    exists <- liftIO $ doesDirectoryExist dirPath
    if exists
      then do
        filePaths <- liftIO $ getFilesInDirectory directoryLoaderOptions 0 dirPath
        fmap concat $ forM filePaths $ \path -> do
          if takeExtension path == ".pdf"
            then load (PdfLoader path)
            else load (FileLoader path)
      else
        throwError $
          documentLoaderError
            (T.pack $ "Directory does not exist: " ++ dirPath)
            (Just "DirectoryLoader")
            (Just $ T.pack dirPath)

  loadAndSplit dirLoader = do
    documents <- load dirLoader
    pure $ splitText defaultCharacterSplitterOps (pageContent $ mconcat documents)

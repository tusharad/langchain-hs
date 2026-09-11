{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.DocumentLoader.FileLoader
Description : File loading implementation for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

File-based document loader implementation following LangChain's document loading patterns.
-}
module Langchain.DocumentLoader.FileLoader
  ( FileLoader (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Map (fromList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory (doesFileExist)

import Langchain.Core.Error (documentLoaderError)
import Langchain.DocumentLoader.Core
import Langchain.TextSplitter.Character

-- | File loader configuration
newtype FileLoader = FileLoader FilePath
  deriving (Eq, Show)

instance BaseLoader FileLoader where
  load (FileLoader path) = do
    exists <- liftIO $ doesFileExist path
    if exists
      then do
        eContent <- liftIO $ try (readFile path)
        case eContent of
          Left err ->
            throwError $
              documentLoaderError
                (T.pack $ "Error reading file " ++ path ++ ": " ++ show (err :: SomeException))
                (Just "FileLoader")
                (Just $ T.pack path)
          Right content -> do
            let meta = fromList [("source", String $ T.pack path)]
            pure [Document (TL.pack content) meta]
      else
        throwError $
          documentLoaderError
            (T.pack $ "File not found: " ++ path)
            (Just "FileLoader")
            (Just $ T.pack path)

  loadAndSplit (FileLoader path) = do
    exists <- liftIO $ doesFileExist path
    if exists
      then do
        eContent <- liftIO $ try (readFile path)
        case eContent of
          Left err ->
            throwError $
              documentLoaderError
                (T.pack $ "Error reading file " ++ path ++ ": " ++ show (err :: SomeException))
                (Just "FileLoader")
                (Just $ T.pack path)
          Right content -> pure $ splitText defaultCharacterSplitterOps (TL.pack content)
      else
        throwError $
          documentLoaderError
            (T.pack $ "File not found: " ++ path)
            (Just "FileLoader")
            (Just $ T.pack path)

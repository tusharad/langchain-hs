{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Tool.FileSystem
Description : Standard File System Tools implementation
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

File system tools (readFile, writeFile, listDir) built on Langchain.Core.Tool.
-}
module Langchain.Tool.FileSystem
  ( readFileTool
  , writeFileTool
  , listDirTool
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)

import Langchain.Core.Error (toolError)
import Langchain.Core.Tool (Tool (..), createTool)

-- | Read file content tool
readFileTool :: MonadIO m => Tool m
readFileTool =
  createTool
    "read_file"
    "Read text contents from a file path"
    ( object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
              ["path" .= object ["type" .= ("string" :: Text)]]
        , "required" .= (["path"] :: [Text])
        ]
    )
    ( \case
        Object o -> case parseEither (.:? "path") o of
          Right (Just p) -> do
            eContent <- liftIO $ try (TIO.readFile (T.unpack p))
            case eContent of
              Left err -> pure $ Left $ toolError (T.pack $ show (err :: IOError)) (Just "read_file") Nothing
              Right txt -> pure $ Right txt
          _ -> pure $ Left $ toolError "Missing 'path' field" (Just "read_file") Nothing
        _ -> pure $ Left $ toolError "Invalid arguments object" (Just "read_file") Nothing
    )

-- | Write content to file tool
writeFileTool :: MonadIO m => Tool m
writeFileTool =
  createTool
    "write_file"
    "Write text contents to a file path"
    ( object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
              [ "path" .= object ["type" .= ("string" :: Text)]
              , "content" .= object ["type" .= ("string" :: Text)]
              ]
        , "required" .= (["path", "content"] :: [Text])
        ]
    )
    ( \case
        Object o -> case (parseEither (.:? "path") o, parseEither (.:? "content") o) of
          (Right (Just p), Right (Just content)) -> do
            eRes <- liftIO $ try (TIO.writeFile (T.unpack p) content)
            case eRes of
              Left err -> pure $ Left $ toolError (T.pack $ show (err :: IOError)) (Just "write_file") Nothing
              Right () -> pure $ Right ("Successfully wrote to " <> p)
          _ -> pure $ Left $ toolError "Missing 'path' or 'content' field" (Just "write_file") Nothing
        _ -> pure $ Left $ toolError "Invalid arguments object" (Just "write_file") Nothing
    )

-- | List directory contents tool
listDirTool :: MonadIO m => Tool m
listDirTool =
  createTool
    "list_directory"
    "List files and subdirectories in a directory path"
    ( object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
              ["path" .= object ["type" .= ("string" :: Text)]]
        , "required" .= (["path"] :: [Text])
        ]
    )
    ( \case
        Object o -> case parseEither (.:? "path") o of
          Right (Just p) -> do
            eFiles <- liftIO $ try (listDirectory (T.unpack p))
            case eFiles of
              Left err -> pure $ Left $ toolError (T.pack $ show (err :: IOError)) (Just "list_directory") Nothing
              Right files -> pure $ Right (T.unlines $ map T.pack files)
          _ -> pure $ Left $ toolError "Missing 'path' field" (Just "list_directory") Nothing
        _ -> pure $ Left $ toolError "Invalid arguments object" (Just "list_directory") Nothing
    )

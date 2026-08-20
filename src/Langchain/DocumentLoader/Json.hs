{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.DocumentLoader.Json
Description : JSON and JSONL document loader
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Loads JSON arrays or JSON Lines (JSONL) files as LangChain Documents.
-}
module Langchain.DocumentLoader.Json
  ( JsonLoader (..)
  , defaultJsonLoader
  , jsonlLoader
  ) where

import Control.Exception (try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as TS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Langchain.Core.Error (LangchainError, documentLoaderError)
import Langchain.DocumentLoader.Core (BaseLoader (..), Document (..))
import Langchain.TextSplitter.Character (defaultCharacterSplitterOps, splitText)

-- | Configuration options for JSON document loader
data JsonLoader = JsonLoader
  { jsonFilePath :: FilePath
  , jsonContentKey :: Maybe TS.Text
  -- ^ Key to extract as pageContent. If Nothing, full JSON is stringified.
  , jsonIsLines :: Bool
  -- ^ True for JSON Lines (.jsonl), False for regular JSON
  , jsonSplitter :: Maybe (Text -> [Text])
  }

-- | Default JSON loader for standard JSON array files
defaultJsonLoader :: FilePath -> JsonLoader
defaultJsonLoader path =
  JsonLoader
    { jsonFilePath = path
    , jsonContentKey = Nothing
    , jsonIsLines = False
    , jsonSplitter = Nothing
    }

-- | JSON loader configured for JSON Lines (.jsonl) files
jsonlLoader :: FilePath -> JsonLoader
jsonlLoader path =
  JsonLoader
    { jsonFilePath = path
    , jsonContentKey = Nothing
    , jsonIsLines = True
    , jsonSplitter = Nothing
    }

instance BaseLoader JsonLoader where
  load loader = do
    rawBytesRes <- liftIO $ try $ LBS.readFile (jsonFilePath loader)
    rawBytes <- case rawBytesRes of
      Left err ->
        throwError $
          documentLoaderError
            (TS.pack $ "Failed to read JSON file: " ++ show (err :: IOError))
            (Just "JsonLoader")
            Nothing
      Right b -> pure b

    let filePath = jsonFilePath loader
        mbKey = jsonContentKey loader

    if jsonIsLines loader
      then do
        let linesList = filter (not . LBS.null) (LBSC.lines rawBytes)
        mapM (parseJsonLine filePath mbKey) (zip [1 :: Int ..] linesList)
      else case decode rawBytes of
        Nothing ->
          throwError $
            documentLoaderError
              "Failed to parse JSON content: invalid JSON syntax"
              (Just "JsonLoader")
              Nothing
        Just (Array arr) ->
          pure
            [valueToDocument filePath mbKey (Just idx) v | (idx, v) <- zip [1 :: Int ..] (foldr (:) [] arr)]
        Just obj@(Object _) ->
          pure [valueToDocument filePath mbKey Nothing obj]
        Just otherVal ->
          pure [valueToDocument filePath mbKey Nothing otherVal]

  loadAndSplit loader = do
    docs <- load loader
    let splitter = case jsonSplitter loader of
          Just s -> s
          Nothing -> splitText defaultCharacterSplitterOps
    pure $ concatMap (splitter . pageContent) docs

parseJsonLine ::
  (MonadIO m, MonadError LangchainError m) =>
  FilePath -> Maybe TS.Text -> (Int, LBS.ByteString) -> m Document
parseJsonLine filePath mbKey (lineNum, bs) =
  case decode bs of
    Nothing ->
      throwError $
        documentLoaderError
          (TS.pack $ "Invalid JSON at line " ++ show lineNum)
          (Just "JsonLoader")
          Nothing
    Just val -> pure $ valueToDocument filePath mbKey (Just lineNum) val

valueToDocument :: FilePath -> Maybe TS.Text -> Maybe Int -> Value -> Document
valueToDocument filePath mbKey mbLine val =
  let baseMeta =
        Map.insert "source" (String $ TS.pack filePath) $
          case mbLine of
            Just l -> Map.singleton "line" (Number $ fromIntegral l)
            Nothing -> Map.empty
   in case val of
        Object km ->
          let (content, extraMeta) = case mbKey of
                Just k ->
                  case KeyMap.lookup (Key.fromText k) km of
                    Just (String s) -> (TL.fromStrict s, Map.delete k (kmToMap km))
                    Just other -> (TLE.decodeUtf8 (encode other), Map.delete k (kmToMap km))
                    Nothing -> (TLE.decodeUtf8 (encode val), kmToMap km)
                Nothing -> (TLE.decodeUtf8 (encode val), kmToMap km)
              finalMeta = Map.union baseMeta extraMeta
           in Document content finalMeta
        _ ->
          let content = TLE.decodeUtf8 (encode val)
           in Document content baseMeta

kmToMap :: KeyMap.KeyMap Value -> Map.Map TS.Text Value
kmToMap km = Map.fromList [(Key.toText k, v) | (k, v) <- KeyMap.toList km]

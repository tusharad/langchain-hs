{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.DocumentLoader.Csv
Description : CSV file document loader
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Loads CSV files as LangChain Documents where each row produces a Document.
-}
module Langchain.DocumentLoader.Csv
  ( CsvLoader (..)
  , defaultCsvLoader
  , parseCsvRows
  ) where

import Control.Exception (try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as TS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import Langchain.Core.Error (documentLoaderError)
import Langchain.DocumentLoader.Core (BaseLoader (..), Document (..))
import Langchain.TextSplitter.Character (defaultCharacterSplitterOps, splitText)

-- | Configuration options for CSV loader
data CsvLoader = CsvLoader
  { csvFilePath :: FilePath
  , csvDelimiter :: Char
  , csvContentColumns :: Maybe [TS.Text]
  -- ^ Optional list of columns to include in pageContent. If Nothing, all columns are concatenated.
  , csvSplitter :: Maybe (Text -> [Text])
  }

-- | Default CSV loader configuration
defaultCsvLoader :: FilePath -> CsvLoader
defaultCsvLoader path =
  CsvLoader
    { csvFilePath = path
    , csvDelimiter = ','
    , csvContentColumns = Nothing
    , csvSplitter = Nothing
    }

instance BaseLoader CsvLoader where
  load loader = do
    contentRes <- liftIO $ try $ TLIO.readFile (csvFilePath loader)
    content <- case contentRes of
      Left err ->
        throwError $
          documentLoaderError
            (TS.pack $ "Failed to read CSV file: " ++ show (err :: IOError))
            (Just "CsvLoader")
            Nothing
      Right c -> pure c

    let rows = parseCsvRows (csvDelimiter loader) content
    case rows of
      [] -> pure []
      (headers : dataRows) -> do
        let headerTexts = map (TS.pack . TL.unpack . TL.strip) headers
            docs =
              [ makeDocument headerTexts (map TL.strip row) (csvContentColumns loader) (csvFilePath loader) idx
              | (idx, row) <- zip [1 ..] dataRows
              , not (null row) && not (all TL.null row)
              ]
        pure docs

  loadAndSplit loader = do
    docs <- load loader
    let splitter = case csvSplitter loader of
          Just s -> s
          Nothing -> splitText defaultCharacterSplitterOps
    pure $ concatMap (splitter . pageContent) docs

makeDocument :: [TS.Text] -> [Text] -> Maybe [TS.Text] -> FilePath -> Int -> Document
makeDocument headers values mbSelectedCols filePath rowIdx =
  let pairs = zip headers values
      metaMap =
        Map.fromList
          [ (h, String (TS.pack $ TL.unpack val))
          | (h, val) <- pairs
          ]
      metaWithSource =
        Map.insert "source" (String $ TS.pack filePath) $
          Map.insert "row" (Number $ fromIntegral rowIdx) metaMap
      contentLines = case mbSelectedCols of
        Just selected ->
          [ h <> ": " <> TS.pack (TL.unpack val)
          | (h, val) <- pairs
          , h `elem` selected
          ]
        Nothing ->
          [ h <> ": " <> TS.pack (TL.unpack val)
          | (h, val) <- pairs
          ]
      content = TL.pack $ TS.unpack $ TS.intercalate "\n" contentLines
   in Document content metaWithSource

-- | Robust CSV line parser supporting quoted cells with commas
parseCsvRows :: Char -> Text -> [[Text]]
parseCsvRows delim text =
  let allLines = TL.lines text
   in map (parseCsvLine delim) allLines

parseCsvLine :: Char -> Text -> [Text]
parseCsvLine delim line = go False [] "" (TL.unpack line)
  where
    go :: Bool -> [Text] -> String -> String -> [Text]
    go _ acc cur [] = reverse (TL.pack (reverse cur) : acc)
    go inQuote acc cur ('"' : cs) =
      case cs of
        ('"' : rest) -> go inQuote acc ('"' : cur) rest
        _ -> go (not inQuote) acc cur cs
    go inQuote acc cur (c : cs)
      | c == delim && not inQuote =
          go inQuote (TL.pack (reverse cur) : acc) "" cs
      | otherwise =
          go inQuote acc (c : cur) cs

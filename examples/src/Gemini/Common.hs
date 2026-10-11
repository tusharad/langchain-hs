{-# LANGUAGE OverloadedStrings #-}

module Gemini.Common
  ( getGeminiKey
  , getGeminiModel
  , getGeminiEmbeddings
  , defaultGeminiModelName
  ) where

import Control.Exception (IOException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Embeddings.Gemini (GeminiEmbeddings (..), defaultGeminiEmbeddings)
import Langchain.Provider.Gemini (Gemini, newGemini)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

defaultGeminiModelName :: T.Text
defaultGeminiModelName = "gemini-3.6-flash"

-- | Read Gemini API key from GEMINI_API_KEY env or ~/.gemini_api_key or local files.
getGeminiKey :: IO T.Text
getGeminiKey = do
  mbEnv <- lookupEnv "GEMINI_API_KEY"
  case mbEnv of
    Just k | not (null k) -> pure $ T.strip $ T.pack k
    _ -> do
      home <- getHomeDirectory
      let homeKey = home </> ".gemini_api_key"
      homeExists <- doesFileExist homeKey
      if homeExists
        then do
          txt <- T.strip <$> T.readFile homeKey
          if T.null txt then checkRelativeFiles else pure txt
        else checkRelativeFiles
  where
    checkRelativeFiles = readFirstFile ["key", "../key", "../../key"]

    readFirstFile [] = pure ""
    readFirstFile (p : ps) = do
      res <- try (T.readFile p) :: IO (Either IOException T.Text)
      case res of
        Right content | not (T.null (T.strip content)) -> pure (T.strip content)
        _ -> readFirstFile ps

-- | Construct a Gemini provider instance using environment key.
getGeminiModel :: T.Text -> IO Gemini
getGeminiModel modelName = (\k -> newGemini k modelName Nothing) <$> getGeminiKey

-- | Construct a GeminiEmbeddings instance using environment key.
getGeminiEmbeddings :: IO GeminiEmbeddings
getGeminiEmbeddings = defaultGeminiEmbeddings <$> getGeminiKey

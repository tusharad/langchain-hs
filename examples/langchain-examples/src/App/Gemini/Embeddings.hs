{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Gemini.Embeddings
  ( runApp
  ) where

import qualified Data.Text as T
import Langchain.Embeddings.Gemini
import System.Environment

runApp :: IO ()
runApp = do
  mbApiKey <- lookupEnv "GEMINI_API_KEY"
  case mbApiKey of
    Nothing -> putStrLn "Please provide API Key"
    Just aKey -> do
      let geminiEmbed =
            defaultGeminiEmbeddings
              { apiKey = T.pack aKey
              }
      eRes <- embedQuery geminiEmbed "Some large query to emebed"
      case eRes of
        Left err -> putStrLn $ "Something went wrong: " <> err
        Right r -> print r

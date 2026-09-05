{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Test.Langchain.TestHelpers
Description : Test helpers, environment filtering, and Ollama integration testing utilities
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Test.Langchain.TestHelpers
  ( TestLevel (..)
  , isOllamaRunning
  , isModelAvailable
  , ollamaModelName
  , defaultTestModel
  , defaultEmbedModel
  , defaultIntegrationTimeout
  , newTestOllama
  , withOllamaModel
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, decode)
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Provider.Ollama (Ollama, defaultConfig, newOllama)
import Network.HTTP.Simple
  ( getResponseBody
  , getResponseStatusCode
  , httpLBS
  , parseRequest_
  , setRequestCheckStatus
  )

-- | Test categorization levels configured via LANGCHAIN_TEST_LEVEL environment variable
data TestLevel
  = UnitLevel
  | PropertyLevel
  | IntegrationLevel
  | E2ELevel
  deriving (Eq, Ord, Show, Read)

-- | Default primary LLM model for integration and E2E tests
defaultTestModel :: Text
defaultTestModel = "qwen3.5:2b"

-- | Fallback test model name if qwen is not available
ollamaModelName :: Text
ollamaModelName = "gemma3:latest"

-- | Default embedding model for integration tests
defaultEmbedModel :: Text
defaultEmbedModel = "nomic-embed-text"

-- | Check if Ollama daemon is running on localhost:11434
isOllamaRunning :: IO Bool
isOllamaRunning = do
  eRes <- try (httpLBS $ setRequestCheckStatus $ parseRequest_ "GET http://localhost:11434/api/tags")
  case eRes of
    Left (_ :: SomeException) -> pure False
    Right res -> pure (getResponseStatusCode res == 200)

-- | Check if a specific model tag is available in local Ollama
isModelAvailable :: Text -> IO Bool
isModelAvailable targetModel = do
  eRes <- try (httpLBS $ setRequestCheckStatus $ parseRequest_ "GET http://localhost:11434/api/tags")
  case eRes of
    Left (_ :: SomeException) -> pure False
    Right res -> do
      let body = getResponseBody res
      case decode body :: Maybe Value of
        Nothing -> pure False
        Just _ -> pure $ T.isInfixOf targetModel (T.pack $ show body)

-- | Execute an action with an Ollama model if available, otherwise skip cleanly
withOllamaModel :: Text -> (Text -> IO ()) -> IO ()
withOllamaModel preferredModel action = do
  running <- isOllamaRunning
  if not running
    then putStrLn " [SKIPPED] Ollama daemon is not running on http://localhost:11434"
    else do
      hasPref <- isModelAvailable preferredModel
      if hasPref
        then action preferredModel
        else do
          hasFallback <- isModelAvailable ollamaModelName
          if hasFallback
            then action ollamaModelName
            else
              putStrLn $
                " [SKIPPED] Neither "
                  ++ T.unpack preferredModel
                  ++ " nor fallback "
                  ++ T.unpack ollamaModelName
                  ++ " is available in Ollama."

-- | Default timeout in seconds for Ollama integration tests
defaultIntegrationTimeout :: Int
defaultIntegrationTimeout = 600

newTestOllama :: MonadIO m => Text -> m Ollama
newTestOllama model =
  newOllama
    model
    defaultConfig
      { configTimeout = 600
      }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Test.Langchain.TestHelpers
Description : Test helpers, environment filtering, and provider selection utilities
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides smart model selection: uses OpenRouter when API key is present,
and falls back to a local Ollama instance otherwise.
-}
module Test.Langchain.TestHelpers
  ( -- * Provider selection
    withAnyModel
  , withOpenRouterOrOllama

    -- * OpenRouter helpers
  , getOpenRouterApiKey
  , newTestOpenRouter
  , defaultOpenRouterModel
  , defaultOpenRouterEndpoint

    -- * Ollama helpers
  , isOllamaInstalled
  , isOllamaRunning
  , isModelAvailable
  , newTestOllama
  , withOllamaModel

    -- * Shared defaults
  , defaultTestModel
  , defaultEmbedModel
  , ollamaModelName

    -- * TestLevel
  , TestLevel (..)
  ) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, decode)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (findExecutable)

import Langchain.Provider.Ollama (Ollama, configTimeout, defaultConfig, newOllama)
import Langchain.Provider.OpenAI (OpenAI, openAICompatible)
import Network.HTTP.Simple
  ( getResponseBody
  , getResponseStatusCode
  , httpLBS
  , parseRequest_
  , setRequestCheckStatus
  )
import System.Environment (lookupEnv)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Test categorization levels configured via LANGCHAIN_TEST_LEVEL environment variable
data TestLevel
  = UnitLevel
  | PropertyLevel
  | IntegrationLevel
  | E2ELevel
  deriving (Eq, Ord, Show, Read)

-- ---------------------------------------------------------------------------
-- Shared defaults
-- ---------------------------------------------------------------------------

-- | Default Ollama model for integration tests
defaultTestModel :: Text
defaultTestModel = "qwen3.5:2b"

-- | Fallback Ollama model
ollamaModelName :: Text
ollamaModelName = "gemma3:latest"

-- | Default embedding model (Ollama)
defaultEmbedModel :: Text
defaultEmbedModel = "nomic-embed-text"

-- ---------------------------------------------------------------------------
-- OpenRouter helpers
-- ---------------------------------------------------------------------------

-- | Default OpenRouter model for integration tests
defaultOpenRouterModel :: Text
defaultOpenRouterModel = "nex-agi/nex-n2.5-mini:free"

-- | Default OpenRouter base URL
defaultOpenRouterEndpoint :: Text
defaultOpenRouterEndpoint = "https://openrouter.ai/api"

{- | Read OpenRouter API key from @OPENROUTER_API_KEY@, @KEY@ env vars or
  the local @key@ / @../key@ file.
-}
getOpenRouterApiKey :: IO (Maybe Text)
getOpenRouterApiKey = do
  mbEnv <- lookupFirstEnv ["OPENROUTER_API_KEY", "OPEN_ROUTER_API_KEY", "KEY", "OPENAI_API_KEY"]
  case mbEnv of
    Just k | not (T.null k) -> pure (Just k)
    _ -> readFirstFile ["key", "../key"]
  where
    lookupFirstEnv [] = pure Nothing
    lookupFirstEnv (e : es) = do
      mv <- lookupEnv e
      case mv of
        Just v | not (null v) -> pure $ Just (T.strip $ T.pack v)
        _ -> lookupFirstEnv es
    readFirstFile [] = pure Nothing
    readFirstFile (p : ps) = do
      res <- try (TIO.readFile p) :: IO (Either IOException Text)
      case res of
        Right c | not (T.null (T.strip c)) -> pure $ Just (T.strip c)
        _ -> readFirstFile ps

-- | Build an 'OpenAI' provider pointing at OpenRouter with @openrouter/free@.
newTestOpenRouter :: Text -> OpenAI
newTestOpenRouter apiKey =
  openAICompatible apiKey defaultOpenRouterModel defaultOpenRouterEndpoint

-- ---------------------------------------------------------------------------
-- Ollama helpers
-- ---------------------------------------------------------------------------

-- | Check if the Ollama CLI executable is installed on the system PATH
isOllamaInstalled :: IO Bool
isOllamaInstalled = isJust <$> findExecutable "ollama"

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

-- | Execute an action with an Ollama model if available, otherwise skip cleanly.
withOllamaModel :: Text -> (Text -> IO ()) -> IO ()
withOllamaModel preferredModel action = do
  running <- isOllamaRunning
  if not running
    then do
      installed <- isOllamaInstalled
      if not installed
        then putStrLn " [SKIPPED] Ollama is not installed"
        else putStrLn " [SKIPPED] Ollama daemon is not running on http://localhost:11434"
    else do
      hasPref <- isModelAvailable preferredModel
      if hasPref
        then action preferredModel
        else do
          hasDef <- isModelAvailable defaultTestModel
          if hasDef
            then action defaultTestModel
            else do
              hasFallback <- isModelAvailable ollamaModelName
              if hasFallback
                then action ollamaModelName
                else
                  putStrLn $
                    " [SKIPPED] Neither "
                      ++ T.unpack preferredModel
                      ++ ", "
                      ++ T.unpack defaultTestModel
                      ++ ", nor "
                      ++ T.unpack ollamaModelName
                      ++ " is available in Ollama."

-- | Build an Ollama provider with a generous timeout.
newTestOllama :: MonadIO m => Text -> m Ollama
newTestOllama modelName =
  newOllama
    modelName
    defaultConfig
      { configTimeout = 600
      }

-- ---------------------------------------------------------------------------
-- Combined provider selection
-- ---------------------------------------------------------------------------

{- | Run @openRouterAction@ if an OpenRouter API key is available,
  otherwise fall back to @ollamaAction@.
-}
withOpenRouterOrOllama ::
  -- | Action when neither OpenRouter key nor Ollama is available (e.g. skip)
  IO () ->
  -- | Action given an OpenRouter 'OpenAI' provider
  (OpenAI -> IO ()) ->
  -- | Action given an 'Ollama' provider
  (Ollama -> IO ()) ->
  IO ()
withOpenRouterOrOllama onMissing openRouterAction ollamaAction = do
  mbKey <- getOpenRouterApiKey
  case mbKey of
    Just key -> openRouterAction (newTestOpenRouter key)
    Nothing -> do
      running <- isOllamaRunning
      if running
        then withOllamaModel defaultTestModel (\mName -> do o <- newTestOllama mName; ollamaAction o)
        else onMissing

{- | Run a test action with OpenRouter (OpenAI-compatible) when an API key is
  present (in @OPENROUTER_API_KEY@, @KEY@, or the local @key@ file),
  otherwise fall back to Ollama.
-}
withAnyModel ::
  -- | Action when OpenRouter key is available
  (OpenAI -> IO ()) ->
  -- | Action when falling back to Ollama
  (Ollama -> IO ()) ->
  IO ()
withAnyModel =
  withOpenRouterOrOllama
    ( do
        installed <- isOllamaInstalled
        if not installed
          then putStrLn " [SKIPPED] No OpenRouter key and Ollama is not installed — skipping test"
          else putStrLn " [SKIPPED] No OpenRouter key and no Ollama daemon — skipping test"
    )

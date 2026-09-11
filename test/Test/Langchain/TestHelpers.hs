{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Test.Langchain.TestHelpers
Description : Test helpers, environment filtering, and provider selection utilities
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides smart model selection: uses Gemini (native or OpenAI-compatible) when
@GEMINI_API_KEY@ is present, and falls back to a local Ollama instance otherwise.
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

    -- * Gemini helpers
  , withGeminiOrOllama
  , getGeminiApiKey
  , newTestGemini
  , newTestGeminiCompat
  , defaultGeminiModel
  , defaultGeminiCompatModel
  , withGeminiRetry

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
  , defaultIntegrationTimeout

    -- * TestLevel
  , TestLevel (..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, throwIO, try)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, decode)
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (findExecutable)
import Text.Read (readMaybe)

import Langchain.Provider.Gemini (Gemini, newGemini)
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

-- | Default Gemini native model
defaultGeminiModel :: Text
defaultGeminiModel = "gemini-3.6-flash"

-- | Default Gemini model used via the OpenAI-compatible endpoint
defaultGeminiCompatModel :: Text
defaultGeminiCompatModel = "gemini-3.6-flash"

-- | Default timeout in seconds for integration tests
defaultIntegrationTimeout :: Int
defaultIntegrationTimeout = 600

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
-- Gemini helpers
-- ---------------------------------------------------------------------------

{- | Read the Gemini API key from the @GEMINI_API_KEY@ env var or the local
  @gemini_api_key@ / @../gemini_api_key@ file (for local development).
-}
getGeminiApiKey :: IO (Maybe Text)
getGeminiApiKey = do
  mbEnv <- lookupEnv "GEMINI_API_KEY"
  case mbEnv of
    Just k | not (null k) -> pure $ Just (T.strip $ T.pack k)
    _ -> readFirstFile ["gemini_api_key", "../gemini_api_key"]
  where
    readFirstFile [] = pure Nothing
    readFirstFile (p : ps) = do
      res <- try (TIO.readFile p) :: IO (Either IOException Text)
      case res of
        Right c | not (T.null (T.strip c)) -> pure $ Just (T.strip c)
        _ -> readFirstFile ps

-- | Build a native 'Gemini' provider for the given API key.
newTestGemini :: Text -> Gemini
newTestGemini key = newGemini key defaultGeminiModel Nothing

{- | Build an 'OpenAI' provider pointing at Gemini's OpenAI-compatible endpoint.

Gemini exposes @https://generativelanguage.googleapis.com/v1beta/openai@
as a drop-in OpenAI-compatible base URL.
-}
newTestGeminiCompat :: Text -> OpenAI
newTestGeminiCompat key =
  openAICompatible
    key
    defaultGeminiCompatModel
    "https://generativelanguage.googleapis.com/v1beta/openai"

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
-- Gemini rate-limit retry
-- ---------------------------------------------------------------------------

{- | Parse the suggested retry delay (in seconds) from a Gemini quota-exceeded
  error message such as @"Please retry in 21.49s"@ or default to 35s on 429.
-}
parseRetryDelay :: String -> Maybe Int
parseRetryDelay msg
  | "quota" `isInfixOf` msg
      || "retry in" `isInfixOf` msg
      || "429" `isInfixOf` msg
      || "RESOURCE_EXHAUSTED" `isInfixOf` msg =
      let findRetry [] = Nothing
          findRetry (c : rest)
            | "retry in " `isPrefixOf` (c : rest) =
                let numStr = takeWhile (\ch -> ch == '.' || isDigit ch) (drop 9 (c : rest))
                 in case readMaybe numStr :: Maybe Double of
                      Just secs -> Just (ceiling (secs :: Double) + 5) -- add 5s buffer
                      Nothing -> Just 35
            | otherwise = findRetry rest
       in case findRetry msg of
            Just d -> Just d
            Nothing -> Just 35
  | otherwise = Nothing

{- | Run a test action against the Gemini API, automatically retrying up to
  3 times when the free-tier rate limit is exceeded.  The retry delay is
  parsed from the error message ("Please retry in Xs").
-}
withGeminiRetry :: IO () -> IO ()
withGeminiRetry action = go (3 :: Int)
  where
    go 0 = action
    go n = do
      result <- try action :: IO (Either SomeException ())
      case result of
        Right () -> pure ()
        Left ex ->
          case parseRetryDelay (show ex) of
            Just delaySecs -> do
              putStrLn $
                " [RATE LIMIT] Gemini quota exceeded. Retrying in "
                  ++ show delaySecs
                  ++ "s ("
                  ++ show (n - 1)
                  ++ " attempts left)..."
              threadDelay (delaySecs * 1000000)
              go (n - 1)
            Nothing -> throwIO ex

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

{- | Run @geminiAction@ if a Gemini API key is available, otherwise fall back
  to @ollamaAction@.  Automatically retries on Gemini free-tier rate limits.
-}
withGeminiOrOllama ::
  -- | Action when neither Gemini key nor Ollama is available (e.g. skip)
  IO () ->
  -- | Action given a Gemini API key (use 'newTestGemini' or 'newTestGeminiCompat')
  (Text -> IO ()) ->
  -- | Action given an Ollama model name (use 'newTestOllama')
  (Text -> IO ()) ->
  IO ()
withGeminiOrOllama onMissing geminiAction ollamaAction = do
  mbKey <- getGeminiApiKey
  case mbKey of
    Just key -> withGeminiRetry (geminiAction key)
    Nothing -> do
      running <- isOllamaRunning
      if running
        then withOllamaModel defaultTestModel ollamaAction
        else onMissing

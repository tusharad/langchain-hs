{-# LANGUAGE OverloadedStrings #-}

module Ollama.Resilience (runApp) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  cache <- newInMemoryCache
  cb <- newCircuitBreaker "OllamaBreaker" defaultCircuitConfig
  limiter <- newRateLimiter 5 2
  o <- newOllama "qwen3.5:2b" defaultConfig
  let cachedModel = withCaching o cache
      query =
        "List 3 core Haskell language features as a comma-separated list. Output only the comma-separated items, nothing else."

  res <- runExceptT $ do
    firstResult <- askWithResilience cb limiter cachedModel query
    liftIO $ do
      T.putStrLn "First call (cache miss):"
      printItems firstResult

    secondResult <- askWithResilience cb limiter cachedModel query
    liftIO $ do
      T.putStrLn "\nSecond call (cache hit):"
      printItems secondResult

  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right () -> pure ()

askWithResilience ::
  CircuitBreaker ->
  RateLimiter ->
  CachedModel Ollama InMemoryCache ->
  Text ->
  ExceptT LangchainError IO [Text]
askWithResilience cb limiter model_ prompt =
  withCircuitBreaker cb $
    withRetry defaultRetryPolicy $
      withRateLimit limiter $ do
        resp <- invoke model_ [userMessage prompt] Nothing
        case parse (extractMessageText resp) of
          Left err -> throwError err
          Right (CommaSeparatedList items) -> pure items

printItems :: [Text] -> IO ()
printItems = mapM_ (\item -> T.putStrLn $ "- " <> item)

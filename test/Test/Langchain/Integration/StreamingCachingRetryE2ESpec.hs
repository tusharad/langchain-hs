{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Test.Langchain.Integration.StreamingCachingRetryE2ESpec
Description : Caching and retry resilience integration tests (Gemini or Ollama)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Test.Langchain.Integration.StreamingCachingRetryE2ESpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Cache.Core
import Langchain.Core.Model
import Langchain.Resilience.Retry
import Test.Langchain.TestHelpers (withAnyModel)

assertCachingRetry :: CacheableChatModel m => m -> IO ()
assertCachingRetry baseModel = do
  cache <- newInMemoryCache
  let cachedModel = withCaching baseModel cache
      msgs = [userMessage "Respond with the single word 'OK'."]

  -- First call: populates cache
  r1 <- runExceptT $ withRetry defaultRetryPolicy (invoke cachedModel msgs Nothing)
  case r1 of
    Left err -> assertFailure ("First invocation failed: " ++ show err)
    Right msg1 -> do
      assertBool "Response is non-empty" (not $ T.null (extractMessageText msg1))

      -- Second call: hits cache (must return identical result)
      r2 <- runExceptT $ withRetry defaultRetryPolicy (invoke cachedModel msgs Nothing)
      case r2 of
        Left err -> assertFailure ("Cached invocation failed: " ++ show err)
        Right msg2 ->
          extractMessageText msg2 @?= extractMessageText msg1

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.StreamingCachingRetryE2ESpec"
    [ testCase "Model wrapped in Caching and Retry policies (OpenRouter or Ollama)" $
        withAnyModel assertCachingRetry assertCachingRetry
    ]

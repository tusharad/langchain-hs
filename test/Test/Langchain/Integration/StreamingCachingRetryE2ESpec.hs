{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.StreamingCachingRetryE2ESpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Cache.Core
import Langchain.Core.Model
import Langchain.Provider.Ollama
import Langchain.Resilience.Retry
import Test.Langchain.TestHelpers (defaultTestModel, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.StreamingCachingRetryE2ESpec"
    [ testCase "Live Ollama model wrapped in Caching and Retry policies" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          baseModel <- newOllama modelName
          cache <- newInMemoryCache
          let cachedModel = withCaching baseModel cache
              msgs = [userMessage "Respond with the single word 'OK'."]

          -- First call: Populates cache
          r1 <- runExceptT $ withRetry defaultRetryPolicy (invoke cachedModel msgs Nothing)
          case r1 of
            Left err -> assertFailure ("First invocation failed: " ++ show err)
            Right msg1 -> do
              assertBool "Response is non-empty" (not $ T.null (extractMessageText msg1))

              -- Second call: Hits cache
              r2 <- runExceptT $ withRetry defaultRetryPolicy (invoke cachedModel msgs Nothing)
              case r2 of
                Left err -> assertFailure ("Cached invocation failed: " ++ show err)
                Right msg2 -> do
                  extractMessageText msg2 @?= extractMessageText msg1
    ]

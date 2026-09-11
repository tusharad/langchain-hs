{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Resilience.RetrySpec (tests) where

import Control.Concurrent.STM
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (internalError)
import Langchain.Resilience.Retry

tests :: TestTree
tests =
  testGroup
    "Langchain.Resilience.RetrySpec"
    [ testCase "withRetry succeeds after failing attempts" $ do
        attemptVar <- newTVarIO (0 :: Int)
        let policy = defaultRetryPolicy {maxRetries = 3, baseDelayMicros = 1000, useJitter = False}
            action = do
              curr <- liftIO $ atomically $ do
                c <- readTVar attemptVar
                writeTVar attemptVar (c + 1)
                pure c
              if curr < 2
                then throwError $ internalError "Temporary failure" Nothing Nothing
                else pure ("Success on attempt " ++ show (curr + 1))
        res <- runExceptT $ withRetry policy action
        res @?= Right "Success on attempt 3"
    , testCase "RateLimiter consumes tokens and executes action" $ do
        limiter <- newRateLimiter 5.0 5.0
        res <- withRateLimit limiter (pure (42 :: Int))
        res @?= 42
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Resilience (tests) where

import Control.Concurrent.STM
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Error (internalError)
import Langchain.Resilience

tests :: TestTree
tests =
  testGroup
    "Langchain.Resilience"
    [ testGroup
        "CircuitBreaker"
        [ testCase "CircuitBreaker starts in Closed state and passes successful requests" $ do
            cb <- newCircuitBreaker "test-cb" defaultCircuitConfig
            st <- getCircuitState cb
            st @?= CircuitClosed
            res <- runExceptT $ withCircuitBreaker cb (pure ("ok" :: String))
            res @?= Right "ok"
        , testCase "CircuitBreaker transitions to Open after exceeding failure threshold" $ do
            let cfg = CircuitBreakerConfig {failureThreshold = 2, resetTimeoutSec = 0.1}
            cb <- newCircuitBreaker "failing-cb" cfg
            -- First failure
            _ <- runExceptT $ withCircuitBreaker cb (throwError (internalError "fail 1" Nothing Nothing))
            st1 <- getCircuitState cb
            st1 @?= CircuitClosed
            -- Second failure -> should open
            _ <- runExceptT $ withCircuitBreaker cb (throwError (internalError "fail 2" Nothing Nothing))
            st2 <- getCircuitState cb
            case st2 of
              CircuitOpen _ -> pure ()
              _ -> assertFailure "Expected CircuitOpen state"
            -- Third request fast-fails without executing action
            resFastFail <- runExceptT $ withCircuitBreaker cb (pure ("should not execute" :: String))
            case resFastFail of
              Left _ -> pure ()
              Right _ -> assertFailure "Expected circuit breaker fast-fail error"
        ]
    , testGroup
        "Retry"
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
    ]

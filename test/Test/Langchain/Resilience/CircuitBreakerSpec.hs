{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Resilience.CircuitBreakerSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT, throwError)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (internalError)
import Langchain.Resilience.CircuitBreaker

tests :: TestTree
tests =
  testGroup
    "Langchain.Resilience.CircuitBreakerSpec"
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

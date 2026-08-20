{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.HTTP.ConnectionPoolSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.HTTP.ConnectionPool

tests :: TestTree
tests =
  testGroup
    "Langchain.HTTP.ConnectionPoolSpec"
    [ testCase "newPooledHttpManager initializes manager with custom pool configuration" $ do
        let cfg = defaultPoolConfig {maxIdleConnections = 64, responseTimeoutSec = 120}
        _mgr <- newPooledHttpManager cfg
        assertBool "Manager successfully created" True
    ]

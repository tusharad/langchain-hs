{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Logging.StructuredLoggingSpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar')
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Logging.Structured

tests :: TestTree
tests =
  testGroup
    "Langchain.Logging.StructuredLoggingSpec"
    [ testCase "InMemoryLogger records events and respects minLevel" $ do
        logger <- newInMemoryLogger InfoLevel
        let logHandler =
              Logger
                { minLevel = InfoLevel
                , writeLog = \ev -> do
                    atomically $ modifyTVar' (inMemoryVar logger) (\ls -> ls ++ [ev])
                }
        logDebug logHandler "Agent" "This debug log should be ignored"
        logInfo logHandler "Agent" "Starting agent turn"
        logWarn logHandler "Retriever" "Slow response from vector store"
        logError logHandler "Model" "Rate limit reached"

        logs <- getInMemoryLogs logger
        case logs of
          (firstLog : _) -> do
            length logs @?= 3
            logLevel firstLog @?= InfoLevel
            logMessage firstLog @?= "Starting agent turn"
          [] -> assertFailure "Expected logs to be non-empty"
    , testCase "logEvent attaches custom metadata" $ do
        logger <- newInMemoryLogger DebugLevel
        let logHandler =
              Logger
                { minLevel = DebugLevel
                , writeLog = \ev -> do
                    atomically $ modifyTVar' (inMemoryVar logger) (\ls -> ls ++ [ev])
                }
        let meta = Map.fromList [("model", "qwen2.5:7b"), ("tokens", "128")]
        logEvent logHandler InfoLevel "Provider" "Model invocation complete" meta

        logs <- getInMemoryLogs logger
        case logs of
          [firstLog] -> logMetadata firstLog @?= meta
          _ -> assertFailure ("Expected exactly 1 log, got " ++ show (length logs))
    ]

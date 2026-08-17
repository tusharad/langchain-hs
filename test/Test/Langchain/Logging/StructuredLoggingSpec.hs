{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Langchain.Logging.StructuredLoggingSpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad.IO.Class (liftIO)
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
        length logs @?= 3
        logLevel (head logs) @?= InfoLevel
        logMessage (head logs) @?= "Starting agent turn"
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
        length logs @?= 1
        logMetadata (head logs) @?= meta
    ]

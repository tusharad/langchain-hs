{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.ObservabilitySpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Observability

tests :: TestTree
tests =
  testGroup
    "Langchain.Observability"
    [ testGroup
        "Structured Logging"
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
    , testGroup
        "OpenTelemetry Tracing"
        [ testCase "withSpan wraps computation, records duration and Ok status" $ do
            tracer <- newOTelTracer (Just "trace-100")
            res <- runExceptT $ withSpan tracer "llm_invoke" Nothing ClientSpan (Map.singleton "provider" "ollama") $ do
              pure ("success response" :: T.Text)
            res @?= Right "success response"

            spans <- getSpans tracer
            case spans of
              [sp] -> do
                spanName sp @?= "llm_invoke"
                spanTraceId sp @?= "trace-100"
                spanStatus sp @?= StatusOk
                assertBool "Duration recorded" (isJust (spanDurationMicros sp))
              _ -> assertFailure ("Expected exactly 1 span, got " ++ show (length spans))
        , testCase "exportSpansJson exports valid JSON formatted trace" $ do
            tracer <- newOTelTracer (Just "trace-export")
            _ <- startSpan tracer "step1" Nothing InternalSpan Map.empty
            jsonText <- exportSpansJson tracer
            assertBool "Contains span name" ("step1" `T.isInfixOf` jsonText)
            assertBool "Contains trace-export" ("trace-export" `T.isInfixOf` jsonText)
        ]
    ]

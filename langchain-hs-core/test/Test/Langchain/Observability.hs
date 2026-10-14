{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Observability (tests) where

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
            logWarn logHandler "Agent" "Model rate limit approaching"
            logError logHandler "Agent" "Tool execution failed"

            logs <- getInMemoryLogs logger
            length logs @?= 3
            map logLevel logs @?= [InfoLevel, WarnLevel, ErrorLevel]
        , testCase "logEvent attaches custom metadata" $ do
            logger <- newInMemoryLogger DebugLevel
            let logHandler =
                  Logger
                    { minLevel = DebugLevel
                    , writeLog = \ev -> do
                        atomically $ modifyTVar' (inMemoryVar logger) (\ls -> ls ++ [ev])
                    }
            let customMeta = Map.fromList [("runId", "run-123"), ("attempt", "1")]
            logEvent logHandler DebugLevel "LLM" "Prompt sent" customMeta
            logs <- getInMemoryLogs logger
            case logs of
              [ev] -> do
                logComponent ev @?= "LLM"
                logMetadata ev @?= customMeta
              _ -> assertFailure "Expected exactly 1 log"
        ]
    , testGroup
        "OpenTelemetry Tracing"
        [ testCase "withSpan wraps computation, records duration and Ok status" $ do
            tracer <- newOTelTracer (Just "trace-unit-1")
            res <-
              runExceptT $
                withSpan tracer "test-operation" Nothing InternalSpan Map.empty (pure ("success" :: String))
            res @?= Right "success"

            spans <- getSpans tracer
            case spans of
              [sp] -> do
                spanName sp @?= "test-operation"
                spanTraceId sp @?= "trace-unit-1"
                spanStatus sp @?= StatusOk
                isJust (spanEndTime sp) @? "Expected span end time"
                isJust (spanDurationMicros sp) @? "Expected duration"
              _ -> assertFailure "Expected exactly 1 span"
        , testCase "exportSpansJson exports valid JSON formatted trace" $ do
            tracer <- newOTelTracer (Just "trace-unit-2")
            _ <- runExceptT $ withSpan tracer "child-span" Nothing ClientSpan Map.empty (pure (42 :: Int))
            jsonOutput <- exportSpansJson tracer
            "child-span" `T.isInfixOf` jsonOutput @? "Expected JSON to contain span name"
            "trace-unit-2" `T.isInfixOf` jsonOutput @? "Expected JSON to contain trace ID"
        ]
    ]

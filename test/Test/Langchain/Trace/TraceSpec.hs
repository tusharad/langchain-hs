{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Trace.TraceSpec (tests) where

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Trace.Core

tests :: TestTree
tests =
  testGroup
    "Langchain.Trace.TraceSpec"
    [ testCase "Tracer records execution steps and calculates total duration" $ do
        tracer <- newTracer "session-123"
        _ <- recordStep tracer (LLMCallAction "ollama:qwen") "Prompt 1" "Response 1" 1500
        _ <- recordStep tracer (ToolCallAction "calc") "{\"expr\":\"2+2\"}" "4" 300
        _ <- recordStep tracer AgentDecisionAction "Input" "Finish" 50

        trace <- getTrace tracer
        traceSessionId trace @?= "session-123"
        length (traceSteps trace) @?= 3
        traceTotalDurationMicros trace @?= 1850
    , testCase "findSlowestStep identifies bottleneck step" $ do
        tracer <- newTracer "session-slow"
        _ <- recordStep tracer (ToolCallAction "fast") "input" "output" 100
        _ <- recordStep tracer (LLMCallAction "slow") "input" "output" 50000
        _ <- recordStep tracer (ToolCallAction "medium") "input" "output" 2000

        trace <- getTrace tracer
        let mbSlowest = findSlowestStep trace
        case mbSlowest of
          Nothing -> assertFailure "Expected slowest step"
          Just s -> do
            stepDurationMicros s @?= 50000
            stepActionType s @?= LLMCallAction "slow"
    ]

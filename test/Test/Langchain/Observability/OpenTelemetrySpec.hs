{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Observability.OpenTelemetrySpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Observability.OpenTelemetry

tests :: TestTree
tests =
  testGroup
    "Langchain.Observability.OpenTelemetrySpec"
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

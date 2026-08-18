{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Observability.StreamProtocolSpec (tests) where

import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Observability.StreamProtocol

tests :: TestTree
tests =
  testGroup
    "Langchain.Observability.StreamProtocol"
    [ testCase "TaskDecomposedEvent JSON serialization and SSE formatting" $ do
        let evt =
              TaskDecomposedEvent
                { eventTaskId = "task-100"
                , eventSubtasks = ["Search docs", "Summarize findings"]
                , eventReasoning = "Needs information gathering first"
                }
            sseText = formatSSE evt
        assertBool "Event type header present" (T.isPrefixOf "event: task_decomposed\n" sseText)
        assertBool "Data payload present" (T.isInfixOf "task-100" sseText)
    , testCase "FactCheckEvent JSON round-trip" $ do
        let evt =
              FactCheckEvent
                { eventClaim = "Haskell guarantees pure referential transparency."
                , eventVerified = True
                , eventConfidence = 0.98
                , eventCritique = "Verified against standard definitions"
                }
            encoded = encode evt
            decoded = decode encoded
        decoded @?= Just evt
    , testCase "formatNdJson terminates with newline" $ do
        let evt = ReportChunkEvent {eventChunk = "# Executive Summary\n"}
            ndjson = formatNdJson evt
        assertBool "Ends with newline" (T.isSuffixOf "\n" ndjson)
    ]

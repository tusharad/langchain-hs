{-# LANGUAGE OverloadedStrings #-}

module Test.Gemini.Client.TypesSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Gemini.Client.Types

tests :: TestTree
tests =
  testGroup
    "Gemini.Client.TypesSpec"
    [ testGroup
        "InteractionStatus JSON"
        [ testCase "encodes and decodes all statuses" $ do
            let statuses =
                  [ (StatusInProgress, "\"in_progress\"")
                  , (StatusRequiresAction, "\"requires_action\"")
                  , (StatusCompleted, "\"completed\"")
                  , (StatusFailed, "\"failed\"")
                  , (StatusCancelled, "\"cancelled\"")
                  , (StatusIncomplete, "\"incomplete\"")
                  ]
            mapM_ (\(st, raw) -> encode st @?= raw) statuses
            mapM_ (\(st, raw) -> decode raw @?= Just st) statuses
        ]
    , testGroup
        "SSE Events JSON"
        [ testCase "SseStepDelta TextDelta" $ do
            let raw =
                  "{\"event_type\": \"step.delta\", \"index\": 0, \"delta\": {\"type\": \"text\", \"text\": \"chunk\"}}" ::
                    LBS.ByteString
            case decode raw of
              Just (SseStepDelta idx (TextDelta txt)) -> do
                idx @?= 0
                txt @?= "chunk"
              other -> assertFailure ("Unexpected decode result: " ++ show other)
        , testCase "SseInteractionCompleted" $ do
            let raw =
                  "{\"event_type\": \"interaction.completed\", \"interaction\": {\"id\": \"v1_abc\", \"status\": \"completed\", \"steps\": []}}" ::
                    LBS.ByteString
            case decode raw of
              Just (SseInteractionCompleted inter) -> do
                interactionId inter @?= "v1_abc"
                interactionStatus inter @?= StatusCompleted
              other -> assertFailure ("Unexpected decode result: " ++ show other)
        ]
    ]

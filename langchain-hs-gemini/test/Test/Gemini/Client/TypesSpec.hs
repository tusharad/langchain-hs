{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Gemini.Client.TypesSpec (tests) where

import Data.Aeson (decode, encode)
import Data.Aeson.QQ (aesonQQ)
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
        "Content JSON"
        [ testCase "TextContent encoding and decoding" $ do
            let c = TextContent "Hello Gemini"
                encoded = encode c
            decode encoded @?= Just c
        , testCase "ImageContent encoding and decoding" $ do
            let c = ImageContent "image/png" "iVBORw0KGgoAAAANSUhEUg=="
                encoded = encode c
            decode encoded @?= Just c
        , testCase "AudioContent encoding and decoding" $ do
            let c = AudioContent "audio/mp3" "SUQzBAAAAAAA"
                encoded = encode c
            decode encoded @?= Just c
        , testCase "DocumentContent encoding and decoding" $ do
            let c = DocumentContent "application/pdf" "JVBERi0xLjQK"
                encoded = encode c
            decode encoded @?= Just c
        ]
    , testGroup
        "Step JSON Variants"
        [ testCase "UserInputStep" $ do
            let s = UserInputStep [TextContent "Hello"]
            decode (encode s) @?= Just s
        , testCase "ModelOutputStep" $ do
            let s = ModelOutputStep [TextContent "Hi there!"]
            decode (encode s) @?= Just s
        , testCase "FunctionCallStep" $ do
            let s = FunctionCallStep "get_weather" [aesonQQ|{"location": "Tokyo"}|] "call_123"
            decode (encode s) @?= Just s
        , testCase "FunctionResultStep" $ do
            let s = FunctionResultStep "call_123" (Just "get_weather") [aesonQQ|{"temp": 20}|] (Just False)
            decode (encode s) @?= Just s
        , testCase "ThoughtStep" $ do
            let s = ThoughtStep (Just "sig_abc") (Just [TextContent "Thinking about weather"])
            decode (encode s) @?= Just s
        , testCase "CodeExecutionCallStep" $ do
            let s = CodeExecutionCallStep "call_py1" "print(42)" (Just "sig_py")
            decode (encode s) @?= Just s
        , testCase "CodeExecutionResultStep" $ do
            let s = CodeExecutionResultStep "call_py1" "42\n" (Just False)
            decode (encode s) @?= Just s
        , testCase "GoogleSearchCallStep and ResultStep" $ do
            let call = GoogleSearchCallStep "call_gs1" "who won world cup" (Just "web")
                res =
                  GoogleSearchResultStep
                    "call_gs1"
                    [ GoogleSearchResultItem
                        (Just "Australia wins")
                        (Just "https://example.com/cricket")
                        (Just "Australia defeated India")
                    ]
            decode (encode call) @?= Just call
            decode (encode res) @?= Just res
        , testCase "GoogleMapsCallStep and ResultStep" $ do
            let call = GoogleMapsCallStep "call_map1" (Just "restaurants near me")
                res = GoogleMapsResultStep "call_map1" (Just [aesonQQ|{"places": ["Cafe 1"]}|])
            decode (encode call) @?= Just call
            decode (encode res) @?= Just res
        , testCase "FileSearchCallStep and ResultStep" $ do
            let call = FileSearchCallStep "call_fs1" (Just "find invoice 2024")
                res = FileSearchResultStep "call_fs1" (Just [aesonQQ|{"files": ["inv_2024.pdf"]}|])
            decode (encode call) @?= Just call
            decode (encode res) @?= Just res
        , testCase "UrlContextCallStep and ResultStep" $ do
            let call = UrlContextCallStep "call_url1" (Just "https://news.ycombinator.com")
                res = UrlContextResultStep "call_url1" (Just [aesonQQ|{"title": "Hacker News"}|])
            decode (encode call) @?= Just call
            decode (encode res) @?= Just res
        ]
    , testGroup
        "Tool JSON Variants"
        [ testCase "FunctionTool" $ do
            let t = functionTool "calculator" (Just "Math tool") (Just [aesonQQ|{"type": "object"}|])
            decode (encode t) @?= Just t
        , testCase "GoogleSearchTool" $ do
            let t = GoogleSearchTool
            decode (encode t) @?= Just t
        , testCase "GoogleMapsTool" $ do
            let t = GoogleMapsTool (Just 37.7749) (Just (-122.4194))
            decode (encode t) @?= Just t
        , testCase "CodeExecutionTool" $ do
            let t = CodeExecutionTool
            decode (encode t) @?= Just t
        , testCase "FileSearchTool" $ do
            let t = FileSearchTool (Just ["store1", "store2"])
            decode (encode t) @?= Just t
        , testCase "UrlContextTool" $ do
            let t = UrlContextTool
            decode (encode t) @?= Just t
        ]
    , testGroup
        "GenerationConfig & ResponseFormat JSON"
        [ testCase "GenerationConfig round-trip" $ do
            let cfg =
                  GenerationConfig
                    { maxOutputTokens = Just 1024
                    , seed = Just 42
                    , stopSequences = Just ["STOP", "END"]
                    , thinkingLevel = Just "high"
                    , thinkingSummaries = Just True
                    , toolChoice = Just "auto"
                    }
            decode (encode cfg) @?= Just cfg
        , testCase "ResponseFormat JSON schema" $ do
            let rf = jsonResponseFormat [aesonQQ|{"type": "object", "properties": {"name": {"type": "string"}}}|]
            decode (encode rf) @?= Just rf
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
    , testGroup
        "Embeddings JSON"
        [ testCase "EmbedContentRequest and Response" $ do
            let req = EmbedContentRequest (EmbedContentPayload [EmbedPart "Hello world"])
                res = EmbedContentResponse (EmbeddingValues [0.1, -0.2, 0.3])
            decode (encode req) @?= Just req
            decode (encode res) @?= Just res
        , testCase "BatchEmbedContentsRequest and Response" $ do
            let req =
                  BatchEmbedContentsRequest
                    [ BatchEmbedRequestItem "models/gemini-embedding-001" (EmbedContentPayload [EmbedPart "Text 1"])
                    , BatchEmbedRequestItem "models/gemini-embedding-001" (EmbedContentPayload [EmbedPart "Text 2"])
                    ]
                res =
                  BatchEmbedContentsResponse
                    [ EmbeddingValues [0.1, 0.2]
                    , EmbeddingValues [0.3, 0.4]
                    ]
            decode (encode req) @?= Just req
            decode (encode res) @?= Just res
        ]
    ]

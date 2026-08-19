{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.OllamaConversionSpec (tests) where

import qualified Data.List.NonEmpty as NonEmpty
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Provider.Ollama
  ( fromOllamaMessage
  , fromOllamaRole
  , toOllamaMessage
  , toOllamaRole
  )
import Ollama.Types.Common (Base64Image (..))
import qualified Ollama.Types.Message as O

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.OllamaConversionSpec"
    [ testGroup
        "Role Mapping Tests"
        [ testCase "System role maps to O.System" $
            toOllamaRole System @?= O.System
        , testCase "User role maps to O.User" $
            toOllamaRole User @?= O.User
        , testCase "Assistant role maps to O.Assistant" $
            toOllamaRole Assistant @?= O.Assistant
        , testCase "Tool role maps to O.Tool" $
            toOllamaRole Tool @?= O.Tool
        , testCase "Developer role maps to O.System" $
            toOllamaRole Developer @?= O.System
        , testCase "Function role maps to O.Tool" $
            toOllamaRole Function @?= O.Tool
        , testCase "fromOllamaRole O.System maps to System" $
            fromOllamaRole O.System @?= System
        , testCase "fromOllamaRole O.User maps to User" $
            fromOllamaRole O.User @?= User
        , testCase "fromOllamaRole O.Assistant maps to Assistant" $
            fromOllamaRole O.Assistant @?= Assistant
        , testCase "fromOllamaRole O.Tool maps to Tool" $
            fromOllamaRole O.Tool @?= Tool
        ]
    , testGroup
        "Message Conversion Tests"
        [ testCase "toOllamaMessage preserves User text" $ do
            let msg = userMessage "Hello from Haskell!"
                (O.Message r txt imgs _ _ _) = toOllamaMessage msg
            r @?= O.User
            txt @?= "Hello from Haskell!"
            imgs @?= Nothing
        , testCase "toOllamaMessage preserves System text" $ do
            let msg = systemMessage "You are a compiler assistant."
                (O.Message r txt _ _ _ _) = toOllamaMessage msg
            r @?= O.System
            txt @?= "You are a compiler assistant."
        , testCase "toOllamaMessage preserves Assistant text" $ do
            let msg = assistantMessage "Here is the result: 42."
                (O.Message r txt _ _ _ _) = toOllamaMessage msg
            r @?= O.Assistant
            txt @?= "Here is the result: 42."
        , testCase "toOllamaMessage extracts base64 image data" $ do
            let msg = imageMessage User "image/png" "iVBORw0KGgoAAAANSUhEUg=="
                (O.Message r _ imgs _ _ _) = toOllamaMessage msg
            r @?= O.User
            case imgs of
              Just [Base64Image b64] -> b64 @?= "iVBORw0KGgoAAAANSUhEUg=="
              _ -> assertFailure "Expected single base64 image in Ollama message"
        , testCase "fromOllamaMessage parses text content" $ do
            let oMsg = O.Message O.Assistant "Response content" Nothing Nothing Nothing Nothing
                msg = fromOllamaMessage oMsg
            messageRole msg @?= Assistant
            extractMessageText msg @?= "Response content"
        , testCase "fromOllamaMessage parses user message" $ do
            let oMsg = O.Message O.User "User query" Nothing Nothing Nothing Nothing
                msg = fromOllamaMessage oMsg
            messageRole msg @?= User
            extractMessageText msg @?= "User query"
        , testCase "Round-trip User message" $ do
            let msg = userMessage "What is pure functional programming?"
                oMsg = toOllamaMessage msg
                roundTripped = fromOllamaMessage oMsg
            roundTripped @?= msg
        , testCase "Round-trip System message" $ do
            let msg = systemMessage "Act as a strict type checker."
                oMsg = toOllamaMessage msg
                roundTripped = fromOllamaMessage oMsg
            roundTripped @?= msg
        , testCase "Round-trip Assistant message" $ do
            let msg = assistantMessage "Output: 100% verified."
                oMsg = toOllamaMessage msg
                roundTripped = fromOllamaMessage oMsg
            roundTripped @?= msg
        , testCase "Multi-modal message with text and image" $ do
            let msg =
                  Message
                    User
                    (TextBlock "Analyze this:" NonEmpty.:| [ImageBlock "image/jpeg" "dGVzdA=="])
                    Nothing
                    Nothing
                    Nothing
                (O.Message _ txt imgs _ _ _) = toOllamaMessage msg
            txt @?= "Analyze this:"
            case imgs of
              Just [Base64Image b64] -> b64 @?= "dGVzdA=="
              _ -> assertFailure "Expected image block conversion"
        ]
    ]

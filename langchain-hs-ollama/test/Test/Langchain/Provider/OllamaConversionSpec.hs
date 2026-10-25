{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.OllamaConversionSpec (tests) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Model
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
        [ testCase "Standard roles map to Ollama equivalents" $ do
            toOllamaRole System @?= O.System
            toOllamaRole User @?= O.User
            toOllamaRole Assistant @?= O.Assistant
            toOllamaRole Tool @?= O.Tool
        , testCase "Developer and Function roles map to System/Tool fallbacks" $ do
            toOllamaRole Developer @?= O.System
            toOllamaRole Function @?= O.Tool
        , testCase "fromOllamaRole parses Ollama roles to Langchain roles" $ do
            fromOllamaRole O.System @?= System
            fromOllamaRole O.User @?= User
            fromOllamaRole O.Assistant @?= Assistant
            fromOllamaRole O.Tool @?= Tool
        ]
    , testGroup
        "Message Conversion Tests"
        [ testCase "toOllamaMessage extracts base64 image data" $ do
            let msg = imageMessage User "image/png" "iVBORw0KGgoAAAANSUhEUg=="
                oMsg = toOllamaMessage msg
            O.messageRole oMsg @?= O.User
            case O.messageImages oMsg of
              Just [Base64Image b64] -> b64 @?= "iVBORw0KGgoAAAANSUhEUg=="
              _ -> assertFailure "Expected single base64 image in Ollama message"
        , testCase "fromOllamaMessage parses role and text content" $ do
            let oMsg = O.assistantMessage "Response content"
                msg = fromOllamaMessage oMsg
            messageRole msg @?= Assistant
            extractMessageText msg @?= "Response content"
        , testCase "Multi-modal message with text and image converts correctly" $ do
            let msg =
                  Message
                    User
                    ( TextBlock "Analyze this:"
                        NonEmpty.:| [ImageBlock $ ImageContent (ImageBase64 (Just "image/jpeg") "dGVzdA==") Nothing Nothing]
                    )
                    Nothing
                    Nothing
                    Nothing
                    Map.empty
                oMsg = toOllamaMessage msg
            O.messageContent oMsg @?= "Analyze this:"
            case O.messageImages oMsg of
              Just [Base64Image b64] -> b64 @?= "dGVzdA=="
              _ -> assertFailure "Expected image block conversion"
        ]
    ]

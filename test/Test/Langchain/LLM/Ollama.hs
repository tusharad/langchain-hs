{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.LLM.Ollama (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Ollama.Common.Types as O
import Langchain.Callback (Callback, Event (..))
import Langchain.LLM.Core
import Langchain.LLM.Ollama
import qualified Langchain.Runnable.Core as Run

captureEvents :: IO (Callback, IO [Event])
captureEvents = do
  eventsRef <- newIORef []
  let callback event = modifyIORef eventsRef (event :)
  let getEvents = reverse <$> readIORef eventsRef
  return (callback, getEvents)

testModelName :: Text
testModelName = "qwen3:0.6b"

tests :: TestTree
tests =
  testGroup
    "Ollama"
    [ testCase "Show instance formats Ollama correctly" $ do
        let ollama = Ollama "llama3" []
        show ollama @?= "Ollama \"llama3\""
    , testCase "generate returns text response for a prompt" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let prompt = "What is functional programming?"
        result <- generate ollama prompt Nothing
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> do
            assertBool "Non-empty response expected" (T.length response > 0)
            events <- getEvents
            assertBool
              "should contain all events"
              (events `shouldContainAll` [LLMStart, LLMEnd])
    , testCase "generate returns error for invalid model" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama "non_existent_model" [callback]
        let prompt = "Hello"
        result <- generate ollama prompt Nothing
        case result of
          Left err -> do
            assertBool "Error should mention model" ("model" `T.isInfixOf` T.pack err)
            events <- getEvents
            assertBool "LLM should tried to be started" (events `shouldContainAll` [LLMStart])
            length (filter isErrorEvent events) @?= 1
          Right _ -> assertFailure "Expected error, but got success"
    , testCase "chat returns text response for messages" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let messages = Message User "What's the capital of France?" defaultMessageData :| []
        result <- chat ollama messages (Just $ defaultOllamaParams {responseTimeOut = Just 1200})
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> do
            assertBool
              "Response should mention Paris"
              ("paris" `T.isInfixOf` T.toLower (content response))
            events <- getEvents
            assertBool "LLM should be completed" (events `shouldContainAll` [LLMStart, LLMEnd])
    , testCase "chat handles multi-turn conversations" $ do
        (callback, _) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let messages =
              Message System "You are a helpful assistant." defaultMessageData
                :| [ Message User "What's the capital of France?" defaultMessageData
                   , Message Assistant "The capital of France is Paris." defaultMessageData
                   , Message User "And what about Italy?" defaultMessageData
                   ]
        result <- chat ollama messages Nothing
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response ->
            assertBool
              "Response should mention Rome"
              ("rome" `T.isInfixOf` T.toLower (content response))
    , testCase "stream calls handlers for streaming responses" $ do
        let ollama = Ollama testModelName []
        let messages = Message User "Count from 1 to 5 briefly." defaultMessageData :| []

        tokensRef <- newIORef []

        let handler =
              StreamHandler
                { onToken = \token -> modifyIORef tokensRef (token :)
                , onComplete = pure ()
                }
        -- \| onComplete does not support Ollama

        result <- stream ollama messages handler Nothing
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right () -> do
            tokens <- readIORef tokensRef
            assertBool "Should receive tokens" (not (null tokens))
    , testCase "invoke calls chat with the input messages" $ do
        let ollama = Ollama testModelName []
        let input = Message User "What is 2+2?" defaultMessageData :| []
        result <- Run.invoke ollama (input, Nothing)
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response ->
            assertBool
              "Should mention 4"
              ("4" `T.isInfixOf` T.toLower (content response))
    , {- qwen3:06b does not support insert
      , testCase "generate appends suffix when provided" $ do
          (callback, getEvents) <- captureEvents
          let ollama = Ollama testModelName [callback]
          let prompt = "What is functional programming?"
          let params = defaultOllamaParams { suffix = Just " [End]" }
          result <- generate ollama prompt (Just params)
          case result of
            Left err -> assertFailure $ "Expected success, got error: " ++ err
            Right response -> do
              assertBool "Response should end with suffix" (T.isSuffixOf " [End]" response)
              events <- getEvents
              assertBool "should contain all events"
                  (events `shouldContainAll` [LLMStart, LLMEnd])
        -}

      testCase "generate uses system message for context" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let prompt = "What is 2 + 2?"
        let systemMsg = "You are a Haskell expert."
        let params = defaultOllamaParams {system = Just systemMsg}
        result <- generate ollama prompt (Just params)
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> do
            assertBool "Response should mention 4" ("4" `T.isInfixOf` T.toLower response)
            events <- getEvents
            assertBool "should contain all events" (events `shouldContainAll` [LLMStart, LLMEnd])
    , testCase "generate returns JSON response when format is set" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let prompt = "What is JSON?"
        let params = defaultOllamaParams {format = Just O.JsonFormat}
        result <- generate ollama prompt (Just params)
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> do
            case eitherDecode (BSL.fromStrict $ T.encodeUtf8 response) :: Either String Value of
              Left _ -> assertFailure "Response is not valid JSON"
              Right _ -> return ()
            events <- getEvents
            assertBool "should contain all events" (events `shouldContainAll` [LLMStart, LLMEnd])
    , {-
      , testCase "generate uses temperature option" $ do
          (callback, getEvents) <- captureEvents
          let ollama = Ollama testModelName [callback]
          let prompt = "Write a short story."
          let temp = 0.7
          let opts = object ["temperature" .= Number temp]
          let params = defaultOllamaParams { options = Just opts }
          result <- generate ollama prompt (Just params)
          case result of
            Left err -> assertFailure $ "Expected success, got error: " ++ err
            Right response -> do
              assertBool "Response should not be empty" (T.length response > 0)
              events <- getEvents
              assertBool "should contain all events" (events `shouldContainAll` [LLMStart, LLMEnd])
              -}

      testCase "chat returns JSON response when format is set" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let messages = Message User "What is JSON?" defaultMessageData :| []
        let params = defaultOllamaParams {format = Just O.JsonFormat}
        result <- chat ollama messages (Just params)
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> do
            case eitherDecode (BSL.fromStrict $ T.encodeUtf8 (content response)) :: Either String Value of
              Left _ -> assertFailure "Response is not valid JSON"
              Right _ -> return ()
            events <- getEvents
            assertBool "should contain all events" (events `shouldContainAll` [LLMStart, LLMEnd])
    ]
  where
    isErrorEvent (LLMError _) = True
    isErrorEvent _ = False

    shouldContainAll xs = all (`elem` xs)

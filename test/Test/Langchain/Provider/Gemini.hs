{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.Gemini (tests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (async, poll, wait)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (await, runConduit, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Network.HTTP.Types (status500)
import Network.Wai (Application, rawPathInfo, rawQueryString, requestMethod, responseLBS)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel (stream), extractMessageText, userMessage)
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..), collectEvents)
import Langchain.Provider.Gemini
import Test.Langchain.Provider.TestSseServer
  ( cancellationAwareSseServer
  , capturingRawSseRequestServer
  , collectModelStream
  , gatedSseServer
  , rawSseServer
  , sseFrame
  , withTestApplication
  )

withGeminiProvider :: T.Text -> (Gemini -> IO a) -> IO a
withGeminiProvider url action = action $ geminiWithBaseUrl "test-key" "test-model" url

withRawTestProvider :: [LBS.ByteString] -> (Gemini -> IO a) -> IO a
withRawTestProvider frames action =
  withTestApplication (rawSseServer frames) $ \url -> withGeminiProvider url action

withGatedProvider :: IO () -> (Gemini -> IO a) -> IO a
withGatedProvider waitForContinuation action =
  withTestApplication
    (gatedSseServer (sseFrame $ chunk "Hel") waitForContinuation [sseFrame $ chunk "lo"])
    $ \url -> withGeminiProvider url action

withCancellationAwareProvider :: IO () -> (Gemini -> IO a) -> IO a
withCancellationAwareProvider signalClientClosed action =
  withTestApplication (cancellationAwareSseServer (sseFrame $ chunk "Hello") signalClientClosed) $ \url ->
    withGeminiProvider url action

errorServer :: Application
errorServer _request respond = respond $ responseLBS status500 [] ""

collectRawStream :: [LBS.ByteString] -> IO (Either LangchainError [StreamEvent])
collectRawStream frames =
  withRawTestProvider frames $ \provider ->
    collectModelStream provider [userMessage "Hello"] Nothing

chunk :: LBS.ByteString -> LBS.ByteString
chunk content =
  "{\"candidates\":[{\"index\":0,\"content\":{\"parts\":[{\"text\":\""
    <> content
    <> "\"}]}}]}"

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.Gemini"
    [ testCase "newGemini initializes provider with model" $ do
        let p = newGemini "ai-key" "gemini-1.5-pro"
        model p @?= "gemini-1.5-pro"
    , testCase "stream emits incremental text chunks and ends" $ do
        result <- collectRawStream [sseFrame $ chunk "Hel", sseFrame $ chunk "lo"]
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [LLMStart {}, LLMChunk _ "Hel" Nothing, LLMChunk _ "lo" Nothing, LLMEnd _ responseMessage Nothing] ->
              extractMessageText responseMessage @?= "Hello"
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream delivers a chunk before the response completes" $ do
        firstChunkReceived <- newEmptyMVar
        continueResponse <- newEmptyMVar
        receivedEvents <- newTVarIO []
        withGatedProvider (takeMVar continueResponse) $ \provider -> do
          consumer <-
            async
              . runResourceT
              . runExceptT
              . runConduit
              $ stream provider [userMessage "Hello"] Nothing
                .| C.mapM_
                  ( \event -> do
                      liftIO . atomically $ modifyTVar' receivedEvents (event :)
                      case event of
                        LLMChunk _ "Hel" _ -> liftIO $ putMVar firstChunkReceived ()
                        _ -> pure ()
                  )
          received <- timeout 500000 $ takeMVar firstChunkReceived
          assertBool "expected first chunk before releasing the response" $ isJust received
          stillStreaming <- poll consumer
          assertBool "consumer should wait for the remaining response" $ isNothing stillStreaming
          putMVar continueResponse ()
          result <- timeout 500000 $ wait consumer
          case result of
            Nothing -> assertFailure "stream did not finish after releasing the response"
            Just (Left err) -> assertFailure $ "Expected stream success, got: " ++ show err
            Just (Right ()) -> do
              events <- reverse <$> readTVarIO receivedEvents
              case reverse events of
                LLMEnd _ responseMessage Nothing : _ -> extractMessageText responseMessage @?= "Hello"
                _ -> assertFailure $ "Expected a completed stream, got: " ++ show events
    , testCase "stream finishes when the SSE connection closes" $ do
        result <- collectRawStream [sseFrame $ chunk "Hello"]
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [LLMStart {}, LLMChunk _ "Hello" Nothing, LLMEnd _ responseMessage Nothing] ->
              extractMessageText responseMessage @?= "Hello"
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream converts malformed SSE data to LangchainError" $ do
        result <- collectRawStream [sseFrame "not JSON"]
        case result of
          Left _ -> pure ()
          Right events -> assertFailure $ "Expected stream failure, got: " ++ show events
    , testCase "stream converts HTTP errors to LangchainError" $ do
        result <- withTestApplication errorServer $ \url ->
          withGeminiProvider url $ \provider ->
            runResourceT $ runExceptT $ collectEvents (stream provider [userMessage "Hello"] Nothing)
        case result of
          Left _ -> pure ()
          Right events -> assertFailure $ "Expected stream failure, got: " ++ show events
    , testCase "stream includes usage metadata on LLMEnd" $ do
        let usage = TokenUsage 7 5 12
            frame =
              "{\"candidates\":[{\"index\":0,\"content\":{\"parts\":[{\"text\":\"Hello\"}]}}],\"usageMetadata\":{\"promptTokenCount\":7,\"candidatesTokenCount\":5,\"totalTokenCount\":12}}"
        result <- collectRawStream [sseFrame frame]
        case result of
          Right [LLMStart {}, LLMChunk _ "Hello" Nothing, LLMEnd _ responseMessage (Just actualUsage)] -> do
            extractMessageText responseMessage @?= "Hello"
            actualUsage @?= usage
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream uses the Gemini SSE endpoint and contents payload" $ do
        capturedRequest <- newEmptyMVar
        withTestApplication
          ( capturingRawSseRequestServer
              (\request body -> putMVar capturedRequest (request, body))
              [sseFrame "{}"]
          )
          $ \url -> do
            withGeminiProvider url $ \provider ->
              void . runResourceT . runExceptT $ collectEvents (stream provider [userMessage "Hello"] Nothing)
            (request, body) <- takeMVar capturedRequest
            requestMethod request @?= "POST"
            rawPathInfo request @?= "/v1beta/models/test-model:streamGenerateContent"
            rawQueryString request @?= "?alt=sse&key=test-key"
            Aeson.decode body
              @?= Just
                ( Aeson.object
                    [ "contents"
                        Aeson..= [ Aeson.object
                                     [ "role" Aeson..= ("user" :: T.Text)
                                     , "parts" Aeson..= [Aeson.object ["text" Aeson..= ("Hello" :: T.Text)]]
                                     ]
                                 ]
                    ]
                )
    , testCase "stream closes the SSE connection when the consumer stops after a chunk" $ do
        clientClosed <- newEmptyMVar
        withCancellationAwareProvider (putMVar clientClosed ()) $ \provider -> do
          void . runResourceT . runExceptT . runConduit $
            stream provider [userMessage "Hello"] Nothing .| (await >> await)
          closed <- timeout 500000 $ takeMVar clientClosed
          assertBool "expected the SSE connection to close" $ isJust closed
    ]

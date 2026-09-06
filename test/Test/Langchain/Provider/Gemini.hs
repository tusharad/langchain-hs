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
import Network.HTTP.Types (hContentType, status200, status500)
import Network.Wai
  ( Application
  , Request
  , rawPathInfo
  , rawQueryString
  , requestMethod
  , responseLBS
  , strictRequestBody
  )
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..), collectEvents)
import Langchain.Core.Tool (Tool, createTool)
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

capturingGenerateContentServer :: (Request -> LBS.ByteString -> IO ()) -> Application
capturingGenerateContentServer capture request respond = do
  body <- strictRequestBody request
  capture request body
  respond $
    responseLBS
      status200
      [(hContentType, "application/json")]
      "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"ok\"}]}}]}"

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
    , testCase "invoke sends Gemini function declarations" $ do
        let weatherTool :: Tool IO
            weatherTool = createTool "get_weather" "Gets the weather" weatherSchema (const $ pure $ Right "sunny")
        capturedRequest <- newEmptyMVar
        withTestApplication
          (capturingGenerateContentServer (\request body -> putMVar capturedRequest (request, body)))
          $ \url ->
            withGeminiProvider url $ \provider -> do
              result <- runExceptT $ invoke provider [userMessage "Hello"] (Just $ geminiTools [weatherTool])
              case result of
                Left err -> assertFailure $ "Expected invoke success, got: " ++ show err
                Right response -> extractMessageText response @?= "ok"
        (request, body) <- takeMVar capturedRequest
        requestMethod request @?= "POST"
        rawPathInfo request @?= "/v1beta/models/test-model:generateContent"
        rawQueryString request @?= "?key=test-key"
        Aeson.decode body
          @?= Just
            ( Aeson.object
                [ "contents"
                    Aeson..= [ Aeson.object
                                 [ "role" Aeson..= ("user" :: T.Text)
                                 , "parts" Aeson..= [Aeson.object ["text" Aeson..= ("Hello" :: T.Text)]]
                                 ]
                             ]
                , "tools"
                    Aeson..= [ Aeson.object
                                 [ "functionDeclarations"
                                     Aeson..= [ Aeson.object
                                                  [ "name" Aeson..= ("get_weather" :: T.Text)
                                                  , "description" Aeson..= ("Gets the weather" :: T.Text)
                                                  , "parameters" Aeson..= weatherSchema
                                                  ]
                                              ]
                                 ]
                             ]
                ]
            )
    , testCase "invoke rejects a non-object Gemini config" $ do
        result <-
          runExceptT $
            invoke
              (newGemini "test-key" "test-model")
              [userMessage "Hello"]
              (Just $ Aeson.String "invalid")
        case result of
          Left err ->
            assertBool "Expected config error" $
              "Gemini config must be a JSON object" `T.isInfixOf` T.pack (show err)
          Right _ -> assertFailure "Expected invalid config to fail"
    , testCase "parseGeminiResponse preserves function calls and text" $ do
        let response =
              Aeson.object
                [ "candidates"
                    Aeson..= [ Aeson.object
                                 [ "content"
                                     Aeson..= Aeson.object
                                       [ "parts"
                                           Aeson..= [ Aeson.object ["text" Aeson..= ("Checking weather" :: T.Text)]
                                                    , Aeson.object
                                                        [ "functionCall"
                                                            Aeson..= Aeson.object
                                                              [ "id" Aeson..= ("call_1" :: T.Text)
                                                              , "name" Aeson..= ("get_weather" :: T.Text)
                                                              , "args" Aeson..= Aeson.object ["city" Aeson..= ("Paris" :: T.Text)]
                                                              ]
                                                        ]
                                                    ]
                                       ]
                                 ]
                             ]
                ]
            expectedCall = ToolCall "call_1" "function" "get_weather" (Aeson.object ["city" Aeson..= ("Paris" :: T.Text)])
        case parseGeminiResponse response of
          Left err -> assertFailure $ "Expected function call response, got: " ++ err
          Right message -> do
            extractMessageText message @?= "Checking weather"
            messageToolCalls message @?= Just [expectedCall]
    , testCase "invoke groups adjacent Gemini function responses" $ do
        let weatherResult =
              (toolMessage "Sunny")
                { messageName = Just "get_weather"
                , messageToolId = Just "call_weather"
                }
            timeResult =
              (toolMessage "12:00")
                { messageName = Just "get_time"
                , messageToolId = Just "call_time"
                }
        capturedRequest <- newEmptyMVar
        withTestApplication
          (capturingGenerateContentServer (\request body -> putMVar capturedRequest (request, body)))
          $ \url ->
            withGeminiProvider url $ \provider -> do
              result <- runExceptT $ invoke provider [userMessage "Weather?", weatherResult, timeResult] Nothing
              case result of
                Left err -> assertFailure $ "Expected invoke success, got: " ++ show err
                Right _ -> pure ()
        (_, body) <- takeMVar capturedRequest
        Aeson.decode body
          @?= Just
            ( Aeson.object
                [ "contents"
                    Aeson..= [ Aeson.object
                                 [ "role" Aeson..= ("user" :: T.Text)
                                 , "parts" Aeson..= [Aeson.object ["text" Aeson..= ("Weather?" :: T.Text)]]
                                 ]
                             , Aeson.object
                                 [ "role" Aeson..= ("user" :: T.Text)
                                 , "parts"
                                     Aeson..= [ Aeson.object
                                                  [ "functionResponse"
                                                      Aeson..= Aeson.object
                                                        [ "id" Aeson..= ("call_weather" :: T.Text)
                                                        , "name" Aeson..= ("get_weather" :: T.Text)
                                                        , "response" Aeson..= Aeson.object ["result" Aeson..= ("Sunny" :: T.Text)]
                                                        ]
                                                  ]
                                              , Aeson.object
                                                  [ "functionResponse"
                                                      Aeson..= Aeson.object
                                                        [ "id" Aeson..= ("call_time" :: T.Text)
                                                        , "name" Aeson..= ("get_time" :: T.Text)
                                                        , "response" Aeson..= Aeson.object ["result" Aeson..= ("12:00" :: T.Text)]
                                                        ]
                                                  ]
                                              ]
                                 ]
                             ]
                ]
            )
    , testCase "stream sends Gemini function declarations and function responses" $ do
        let weatherTool :: Tool IO
            weatherTool = createTool "get_weather" "Gets the weather" weatherSchema (const $ pure $ Right "sunny")
            toolCall =
              ToolCall
                "call_weather"
                "function"
                "get_weather"
                (Aeson.object ["city" Aeson..= ("Paris" :: T.Text)])
            assistant = (assistantMessage "") {messageToolCalls = Just [toolCall]}
            toolResult = (toolMessage "Sunny") {messageToolId = Just "call_weather"}
        capturedRequest <- newEmptyMVar
        withTestApplication
          ( capturingRawSseRequestServer
              (\request body -> putMVar capturedRequest (request, body))
              [sseFrame "{}"]
          )
          $ \url -> withGeminiProvider url $ \provider ->
            void . runResourceT . runExceptT $
              collectEvents
                (stream provider [userMessage "Weather?", assistant, toolResult] (Just $ geminiTools [weatherTool]))
        (request, body) <- takeMVar capturedRequest
        requestMethod request @?= "POST"
        rawPathInfo request @?= "/v1beta/models/test-model:streamGenerateContent"
        Aeson.decode body
          @?= Just
            ( Aeson.object
                [ "contents"
                    Aeson..= [ Aeson.object
                                 [ "role" Aeson..= ("user" :: T.Text)
                                 , "parts" Aeson..= [Aeson.object ["text" Aeson..= ("Weather?" :: T.Text)]]
                                 ]
                             , Aeson.object
                                 [ "role" Aeson..= ("model" :: T.Text)
                                 , "parts"
                                     Aeson..= [ Aeson.object
                                                  [ "functionCall"
                                                      Aeson..= Aeson.object
                                                        [ "id" Aeson..= ("call_weather" :: T.Text)
                                                        , "name" Aeson..= ("get_weather" :: T.Text)
                                                        , "args" Aeson..= Aeson.object ["city" Aeson..= ("Paris" :: T.Text)]
                                                        ]
                                                  ]
                                              ]
                                 ]
                             , Aeson.object
                                 [ "role" Aeson..= ("user" :: T.Text)
                                 , "parts"
                                     Aeson..= [ Aeson.object
                                                  [ "functionResponse"
                                                      Aeson..= Aeson.object
                                                        [ "id" Aeson..= ("call_weather" :: T.Text)
                                                        , "name" Aeson..= ("get_weather" :: T.Text)
                                                        , "response" Aeson..= Aeson.object ["result" Aeson..= ("Sunny" :: T.Text)]
                                                        ]
                                                  ]
                                              ]
                                 ]
                             ]
                , "tools"
                    Aeson..= [ Aeson.object
                                 [ "functionDeclarations"
                                     Aeson..= [ Aeson.object
                                                  [ "name" Aeson..= ("get_weather" :: T.Text)
                                                  , "description" Aeson..= ("Gets the weather" :: T.Text)
                                                  , "parameters" Aeson..= weatherSchema
                                                  ]
                                              ]
                                 ]
                             ]
                ]
            )
    , testCase "stream emits incremental text chunks and ends" $ do
        result <- collectRawStream [sseFrame $ chunk "Hel", sseFrame $ chunk "lo"]
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [LLMStart {}, LLMChunk _ "Hel" Nothing, LLMChunk _ "lo" Nothing, LLMEnd _ responseMessage Nothing] ->
              extractMessageText responseMessage @?= "Hello"
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream emits mixed text and function call chunks" $ do
        let frame =
              "{\"candidates\":[{\"index\":0,\"content\":{\"parts\":[{\"text\":\"Checking weather\"},{\"functionCall\":{\"id\":\"call_1\",\"name\":\"get_weather\",\"args\":{\"city\":\"Paris\"}}}]}}]}"
            expectedCall = ToolCall "call_1" "function" "get_weather" (Aeson.object ["city" Aeson..= ("Paris" :: T.Text)])
        result <- collectRawStream [sseFrame frame]
        case result of
          Right [LLMStart {}, LLMChunk _ "Checking weather" (Just toolCall), LLMEnd _ responseMessage Nothing] -> do
            toolCall @?= expectedCall
            extractMessageText responseMessage @?= "Checking weather"
            messageToolCalls responseMessage @?= Just [expectedCall]
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> assertFailure $ "Unexpected stream events: " ++ show events
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
    , testCase "stream rejects malformed function calls" $ do
        result <-
          collectRawStream
            [sseFrame "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"args\":{}}}]}}]}"]
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
  where
    weatherSchema :: Aeson.Value
    weatherSchema =
      Aeson.object
        [ "type" Aeson..= ("OBJECT" :: T.Text)
        , "properties"
            Aeson..= Aeson.object ["city" Aeson..= Aeson.object ["type" Aeson..= ("STRING" :: T.Text)]]
        , "required" Aeson..= ["city" :: T.Text]
        ]

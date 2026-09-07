{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Langchain.Provider.Gemini (tests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (async, poll, wait)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (forM, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (await, runConduit, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
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
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..), collectEvents)
import Langchain.Core.Tool (Tool, createTool)
import qualified Langchain.Core.Tool as CoreTool
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
    , testGroup
        "invoke"
        [ testCase "invoke sends Gemini function declarations" $ do
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
                [aesonQQ|
              {
                "contents": [{"role": "user", "parts": [{"text": "Hello"}]}],
                "tools": [{"functionDeclarations": [{
                  "name": "get_weather",
                  "description": "Gets the weather",
                  "parameters": {
                    "type": "OBJECT",
                    "properties": {"city": {"type": "STRING"}},
                    "required": ["city"]
                  }
                }]}]
              }
            |]
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
        , testCase "parseGeminiResponse preserves function calls, text, and thought signatures" $ do
            let response =
                  [aesonQQ|
                {
                  "candidates": [
                    {
                      "content": {
                        "parts": [
                          { "text": "Checking weather" },
                          {
                            "thoughtSignature": "signature_1",
                            "functionCall": {
                              "id": "call_1",
                              "name": "get_weather",
                              "args": { "city": "Paris" }
                            }
                          }
                        ]
                      }
                    }
                  ]
                }
              |]
                expectedCall = ToolCall "call_1" "function" "get_weather" [aesonQQ|{"city": "Paris"}|]
            case parseGeminiResponse response of
              Left err -> assertFailure $ "Expected function call response, got: " ++ err
              Right message -> do
                extractMessageText message @?= "Checking weather"
                messageToolCalls message @?= Just [expectedCall]
                Map.lookup "langchain.gemini.thoughtSignatures" (messageMetadata message)
                  @?= Just (Aeson.toJSON [Just ("signature_1" :: T.Text)])
        , testCase "parseGeminiResponse leaves ordinary message metadata empty" $ do
            let response = [aesonQQ|{"candidates": [{"content": {"parts": [{"text": "ok"}]}}]}|]
            case parseGeminiResponse response of
              Left err -> assertFailure $ "Expected text response, got: " ++ err
              Right message -> messageMetadata message @?= Map.empty
        , testCase "invoke replays Gemini thought signatures on function calls" $ do
            let toolCall = ToolCall "call_1" "function" "get_weather" [aesonQQ|{"city": "Paris"}|]
                assistant =
                  (assistantMessage "")
                    { messageToolCalls = Just [toolCall]
                    , messageMetadata =
                        Map.singleton
                          "langchain.gemini.thoughtSignatures"
                          (Aeson.toJSON [Just ("signature_1" :: T.Text)])
                    }
            capturedRequest <- newEmptyMVar
            withTestApplication
              (capturingGenerateContentServer (\request body -> putMVar capturedRequest (request, body)))
              $ \url ->
                withGeminiProvider url $ \provider -> do
                  result <- runExceptT $ invoke provider [assistant] Nothing
                  case result of
                    Left err -> assertFailure $ "Expected invoke success, got: " ++ show err
                    Right _ -> pure ()
            (_, body) <- takeMVar capturedRequest
            Aeson.decode body
              @?= Just
                [aesonQQ|
              {
                "contents": [{"role": "model", "parts": [{
                  "thoughtSignature": "signature_1",
                  "functionCall": {
                    "id": "call_1", "name": "get_weather", "args": {"city": "Paris"}
                  }
                }]}]
                }
            |]
        , testCase "invoke replays thought signatures for their matching Gemini function calls" $ do
            let response =
                  [aesonQQ|
                {
                  "candidates": [{
                    "content": {
                      "parts": [
                        {"functionCall": {"id": "call_weather", "name": "get_weather", "args": {"city": "Paris"}}},
                        {"thoughtSignature": "signature_time", "functionCall": {"id": "call_time", "name": "get_time", "args": {"zone": "UTC"}}}
                      ]
                    }
                  }]
                }
              |]
            assistant <- case parseGeminiResponse response of
              Left err -> assertFailure ("Expected function call response, got: " ++ err) >> fail "unreachable"
              Right message -> pure message
            capturedRequest <- newEmptyMVar
            withTestApplication
              (capturingGenerateContentServer (\request body -> putMVar capturedRequest (request, body)))
              $ \url ->
                withGeminiProvider url $ \provider -> do
                  result <- runExceptT $ invoke provider [assistant] Nothing
                  case result of
                    Left err -> assertFailure $ "Expected invoke success, got: " ++ show err
                    Right _ -> pure ()
            (_, body) <- takeMVar capturedRequest
            Aeson.decode body
              @?= Just
                [aesonQQ|
              {
                "contents": [{"role": "model", "parts": [
                  {"functionCall": {"id": "call_weather", "name": "get_weather", "args": {"city": "Paris"}}},
                  {"thoughtSignature": "signature_time", "functionCall": {"id": "call_time", "name": "get_time", "args": {"zone": "UTC"}}}
                ]}]
              }
            |]
        , testCase "invoke omits Gemini thought signatures when metadata is malformed" $ do
            let toolCall = ToolCall "call_1" "function" "get_weather" [aesonQQ|{"city": "Paris"}|]
                assistant =
                  (assistantMessage "")
                    { messageToolCalls = Just [toolCall]
                    , messageMetadata = Map.singleton "langchain.gemini.thoughtSignatures" (Aeson.String "invalid")
                    }
            capturedRequest <- newEmptyMVar
            withTestApplication
              (capturingGenerateContentServer (\request body -> putMVar capturedRequest (request, body)))
              $ \url ->
                withGeminiProvider url $ \provider -> do
                  result <- runExceptT $ invoke provider [assistant] Nothing
                  case result of
                    Left err -> assertFailure $ "Expected invoke success, got: " ++ show err
                    Right _ -> pure ()
            (_, body) <- takeMVar capturedRequest
            Aeson.decode body
              @?= Just
                [aesonQQ|
              {
                "contents": [{"role": "model", "parts": [{"functionCall": {
                  "id": "call_1", "name": "get_weather", "args": {"city": "Paris"}
                }}]}]
              }
            |]
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
                [aesonQQ|
              {
                "contents": [
                  {"role": "user", "parts": [{"text": "Weather?"}]},
                  {"role": "user", "parts": [
                    {"functionResponse": {"id": "call_weather", "name": "get_weather", "response": {"result": "Sunny"}}},
                    {"functionResponse": {"id": "call_time", "name": "get_time", "response": {"result": "12:00"}}}
                  ]}
                ]
              }
            |]
        ]
    , testGroup
        "stream"
        [ testCase "live Gemini stream emits text and usage" $ do
            mbApiKey <- lookupEnv "GEMINI_API_KEY"
            case mbApiKey of
              Nothing -> putStrLn " [SKIPPED] GEMINI_API_KEY is not set"
              Just envApiKey -> do
                envModel <- fromMaybe "gemini-3.5-flash-lite" <$> lookupEnv "GEMINI_STREAM_TEST_MODEL"
                result <-
                  timeout 60000000
                    $ runResourceT
                    $ runExceptT
                    $ collectEvents
                    $ stream
                      (newGemini (T.pack envApiKey) (T.pack envModel))
                      [userMessage "Reply with exactly OK."]
                      Nothing
                case result of
                  Nothing -> assertFailure "Gemini stream timed out"
                  Just (Left err) -> assertFailure $ "Expected stream success, got: " ++ show err
                  Just (Right events) -> case reverse events of
                    LLMEnd _ responseMessage (Just usage) : _ -> do
                      assertBool "Expected non-empty streamed text" $ not $ T.null $ extractMessageText responseMessage
                      assertBool "Expected positive total token usage" $ totalTokens usage > 0
                    _ -> assertFailure $ "Expected LLMEnd with usage, got: " ++ show events
        , testCase "live Gemini stream invokes a tool and continues with its result" $ do
            mbApiKey <- lookupEnv "GEMINI_API_KEY"
            case mbApiKey of
              Nothing -> putStrLn " [SKIPPED] GEMINI_API_KEY is not set"
              Just envApiKey -> do
                envModel <- fromMaybe "gemini-3.5-flash-lite" <$> lookupEnv "GEMINI_STREAM_TEST_MODEL"
                let weatherTool :: Tool IO
                    weatherTool =
                      createTool
                        "get_weather"
                        "Returns the current weather for a city."
                        weatherSchema
                        (const $ pure $ Right "The weather in Paris is sunny and 22 C.")
                    provider = newGemini (T.pack envApiKey) (T.pack envModel)
                    runLive messages config =
                      timeout 60000000
                        $ runResourceT
                        $ runExceptT
                        $ collectEvents
                        $ stream provider messages config
                    prompt = userMessage "Use get_weather to look up the weather in Paris, then answer using the tool result."

                firstResult <- runLive [prompt] (Just $ geminiTools [weatherTool])
                firstEvents <- case firstResult of
                  Nothing -> assertFailure "Gemini tool-call stream timed out" >> fail "unreachable"
                  Just (Left err) -> assertFailure ("Expected tool-call stream success, got: " ++ show err) >> fail "unreachable"
                  Just (Right events) -> pure events
                (assistant, toolCalls) <- case reverse firstEvents of
                  LLMEnd _ responseMessage _ : _ -> case messageToolCalls responseMessage of
                    Just calls@[toolCall]
                      | toolCallName toolCall == "get_weather" -> pure (responseMessage, calls)
                    _ -> assertFailure ("Expected Gemini tool call, got: " ++ show firstEvents) >> fail "unreachable"
                  _ -> assertFailure ("Expected tool-call stream end, got: " ++ show firstEvents) >> fail "unreachable"
                toolResults <- forM toolCalls $ \toolCall -> do
                  output <- CoreTool.toolExecute weatherTool (toolCallArguments toolCall)
                  case output of
                    Left err -> assertFailure ("Tool execution failed: " ++ show err) >> fail "unreachable"
                    Right text ->
                      pure $
                        (textMessage Tool text)
                          { messageName = Just (toolCallName toolCall)
                          , messageToolId = Just (toolCallId toolCall)
                          }
                secondResult <- runLive ([prompt, assistant] <> toolResults) Nothing
                case secondResult of
                  Nothing -> assertFailure "Gemini tool-result stream timed out"
                  Just (Left err) -> assertFailure $ "Expected tool-result stream success, got: " ++ show err
                  Just (Right events) -> case reverse events of
                    LLMEnd _ responseMessage (Just usage) : _ -> do
                      assertBool "Expected final text after tool result"
                        $ not
                        $ T.null
                        $ extractMessageText responseMessage
                      assertBool "Expected positive total token usage" $ totalTokens usage > 0
                    _ -> assertFailure $ "Expected LLMEnd with usage, got: " ++ show events
        , testCase "stream sends Gemini function declarations and function responses" $ do
            let weatherTool :: Tool IO
                weatherTool = createTool "get_weather" "Gets the weather" weatherSchema (const $ pure $ Right "sunny")
                toolCall =
                  ToolCall
                    "call_weather"
                    "function"
                    "get_weather"
                    [aesonQQ|{"city": "Paris"}|]
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
                [aesonQQ|
              {
                "contents": [
                  {"role": "user", "parts": [{"text": "Weather?"}]},
                  {"role": "model", "parts": [{"functionCall": {
                    "id": "call_weather", "name": "get_weather", "args": {"city": "Paris"}
                  }}]},
                  {"role": "user", "parts": [{"functionResponse": {
                    "id": "call_weather", "name": "get_weather", "response": {"result": "Sunny"}
                  }}]}
                ],
                "tools": [{"functionDeclarations": [{
                  "name": "get_weather",
                  "description": "Gets the weather",
                  "parameters": {
                    "type": "OBJECT",
                    "properties": {"city": {"type": "STRING"}},
                    "required": ["city"]
                  }
                }]}]
              }
            |]
        , testCase "stream emits incremental text chunks and ends" $ do
            result <- collectRawStream [sseFrame $ chunk "Hel", sseFrame $ chunk "lo"]
            case result of
              Left err -> assertFailure $ "Expected stream success, got: " ++ show err
              Right events -> case events of
                [LLMStart {}, LLMChunk _ "Hel" Nothing, LLMChunk _ "lo" Nothing, LLMEnd _ responseMessage Nothing] ->
                  do
                    extractMessageText responseMessage @?= "Hello"
                    messageMetadata responseMessage @?= Map.empty
                _ -> assertFailure $ "Unexpected stream events: " ++ show events
        , testCase "stream emits mixed text and function call chunks" $ do
            let frame =
                  Aeson.encode
                    [aesonQQ|
                  {
                    "candidates": [
                      {
                        "index": 0,
                        "content": {
                          "parts": [
                             { "text": "Checking weather" },
                             {
                               "thoughtSignature": "signature_1",
                               "functionCall": {
                                "id": "call_1",
                                "name": "get_weather",
                                "args": { "city": "Paris" }
                              }
                            }
                          ]
                        }
                      }
                    ]
                  }
                |]
                expectedCall = ToolCall "call_1" "function" "get_weather" [aesonQQ|{"city": "Paris"}|]
            result <- collectRawStream [sseFrame frame]
            case result of
              Right [LLMStart {}, LLMChunk _ "Checking weather" (Just toolCall), LLMEnd _ responseMessage Nothing] -> do
                toolCall @?= expectedCall
                extractMessageText responseMessage @?= "Checking weather"
                messageToolCalls responseMessage @?= Just [expectedCall]
                Map.lookup "langchain.gemini.thoughtSignatures" (messageMetadata responseMessage)
                  @?= Just (Aeson.toJSON [Just ("signature_1" :: T.Text)])
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
                [ sseFrame $
                    Aeson.encode
                      [aesonQQ|
                    {
                      "candidates": [
                        {
                          "content": {
                            "parts": [
                              {
                                "functionCall": {
                                  "args": {}
                                }
                              }
                            ]
                          }
                        }
                      ]
                    }
                  |]
                ]
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
                  Aeson.encode
                    [aesonQQ|
                      {
                        "candidates": [
                          {
                            "index": 0,
                            "content": {
                              "parts": [{"text": "Hello"}]
                            }
                          }
                        ],
                        "usageMetadata": {
                          "promptTokenCount": 7,
                          "candidatesTokenCount": 5,
                          "totalTokenCount": 12
                        }
                      }
                    |]
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
                    [aesonQQ|{"contents": [{"role": "user", "parts": [{"text": "Hello"}]}]}|]
        , testCase "stream closes the SSE connection when the consumer stops after a chunk" $ do
            clientClosed <- newEmptyMVar
            withCancellationAwareProvider (putMVar clientClosed ()) $ \provider -> do
              void . runResourceT . runExceptT . runConduit $
                stream provider [userMessage "Hello"] Nothing .| (await >> await)
              closed <- timeout 500000 $ takeMVar clientClosed
              assertBool "expected the SSE connection to close" $ isJust closed
        ]
    ]
  where
    weatherSchema :: Aeson.Value
    weatherSchema =
      [aesonQQ|
        {
          "type": "OBJECT",
          "properties": {"city": {"type": "STRING"}},
          "required": ["city"]
        }
      |]

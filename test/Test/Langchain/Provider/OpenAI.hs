{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Langchain.Provider.OpenAI (tests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (async, poll, wait)
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (ConduitT, await, runConduit, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, responseStream, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant (Header, JSON, ReqBody, Server, err500, serve, throwError, (:>))
import Servant.API.EventStream
  ( PostServerSentEvents
  , ServerEvent (..)
  , ToServerEvent (..)
  )
import Servant.Conduit ()
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..), collectEvents)
import Langchain.Core.Tool (Tool, createTool, toolToValue)

import Langchain.Provider.OpenAI

newtype TestSseEvent = TestSseEvent LBS.ByteString

instance ToServerEvent TestSseEvent where
  toServerEvent (TestSseEvent event) = ServerEvent Nothing Nothing event Nothing Nothing

type TestOpenAIStreamApi =
  "v1"
    :> "chat"
    :> "completions"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] Value
    :> PostServerSentEvents (ConduitT () TestSseEvent IO ())

testStreamServer :: [TestSseEvent] -> Server TestOpenAIStreamApi
testStreamServer events _ _ = pure $ C.yieldMany events

testErrorServer :: Server TestOpenAIStreamApi
testErrorServer _ _ = throwError err500

rawSseServer :: [LBS.ByteString] -> Application
rawSseServer frames _request respond =
  respond $
    responseStream status200 [(hContentType, "text/event-stream")] $ \write flush ->
      mapM_
        (\frame -> write (Builder.lazyByteString frame) >> flush)
        frames

capturingRawSseServer :: (Maybe Value -> IO ()) -> [LBS.ByteString] -> Application
capturingRawSseServer captureRequest frames request respond = do
  captureRequest . Aeson.decode =<< strictRequestBody request
  rawSseServer frames request respond

sseFrame :: LBS.ByteString -> LBS.ByteString
sseFrame payload = "data: " <> payload <> "\n\n"

cancellationAwareSseServer :: IO () -> Application
cancellationAwareSseServer signalClientClosed _request respond =
  respond $
    responseStream status200 [(hContentType, "text/event-stream")] $ \write flush -> do
      let TestSseEvent firstEvent = chunk "Hello"
          keepAlive = do
            write ": keepalive\n\n"
            flush
            threadDelay 1000
            keepAlive
          onDisconnect :: SomeException -> IO ()
          onDisconnect _ = signalClientClosed
      write $ "data: " <> Builder.lazyByteString firstEvent <> "\n\n"
      flush
      keepAlive `catch` onDisconnect

gatedSseServer :: IO () -> Application
gatedSseServer waitForContinuation _request respond =
  respond $
    responseStream status200 [(hContentType, "text/event-stream")] $ \write flush -> do
      let TestSseEvent firstEvent = chunk "Hel"
          TestSseEvent secondEvent = chunk "lo"
      write $ "data: " <> Builder.lazyByteString firstEvent <> "\n\n"
      flush
      waitForContinuation
      write $ "data: " <> Builder.lazyByteString secondEvent <> "\n\n"
      write "data: [DONE]\n\n"
      flush

withTestProvider :: [TestSseEvent] -> (OpenAI -> IO a) -> IO a
withTestProvider events =
  withTestApplication (serve (Proxy :: Proxy TestOpenAIStreamApi) (testStreamServer events))

withErrorProvider :: (OpenAI -> IO a) -> IO a
withErrorProvider =
  withTestApplication $ serve (Proxy :: Proxy TestOpenAIStreamApi) testErrorServer

withRawTestProvider :: [LBS.ByteString] -> (OpenAI -> IO a) -> IO a
withRawTestProvider frames = withTestApplication (rawSseServer frames)

withRequestCapturingProvider :: (Maybe Value -> IO ()) -> (OpenAI -> IO a) -> IO a
withRequestCapturingProvider captureRequest =
  withTestApplication $ capturingRawSseServer captureRequest [sseFrame "[DONE]"]

withCancellationAwareProvider :: IO () -> (OpenAI -> IO a) -> IO a
withCancellationAwareProvider signalClientClosed =
  withTestApplication (cancellationAwareSseServer signalClientClosed)

withGatedProvider :: IO () -> (OpenAI -> IO a) -> IO a
withGatedProvider waitForContinuation =
  withTestApplication (gatedSseServer waitForContinuation)

withTestApplication :: Application -> (OpenAI -> IO a) -> IO a
withTestApplication app action =
  testWithApplication (pure app) $ \port ->
    action $
      (newOpenAI "test-key" "test-model")
        { baseUrl = "http://127.0.0.1:" <> T.pack (show port)
        }

collectStream :: [TestSseEvent] -> IO (Either LangchainError [StreamEvent])
collectStream events =
  withTestProvider events $ \provider ->
    runResourceT $ runExceptT $ collectEvents (stream provider [userMessage "Hello"] Nothing)

collectRawStream :: [LBS.ByteString] -> IO (Either LangchainError [StreamEvent])
collectRawStream frames =
  withRawTestProvider frames $ \provider ->
    runResourceT $ runExceptT $ collectEvents (stream provider [userMessage "Hello"] Nothing)

chunk :: LBS.ByteString -> TestSseEvent
chunk content =
  TestSseEvent $
    "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\""
      <> content
      <> "\"},\"finish_reason\":null}]}"

done :: TestSseEvent
done = TestSseEvent "[DONE]"

emptyChoices :: TestSseEvent
emptyChoices =
  TestSseEvent
    "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[]}"

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.OpenAI"
    [ testCase "normalizeBaseUrl strips endpoint paths for servant compatibility" $ do
        normalizeBaseUrl "https://api.openai.com" @?= "https://api.openai.com"
        normalizeBaseUrl "https://api.openai.com/" @?= "https://api.openai.com"
        normalizeBaseUrl "https://api.openai.com/v1" @?= "https://api.openai.com"
        normalizeBaseUrl "https://api.openai.com/v1/" @?= "https://api.openai.com"
        normalizeBaseUrl "https://api.openai.com/v1/chat/completions" @?= "https://api.openai.com"
        normalizeBaseUrl "https://openrouter.ai/api" @?= "https://openrouter.ai/api"
        normalizeBaseUrl "https://openrouter.ai/api/v1" @?= "https://openrouter.ai/api"
        normalizeBaseUrl "https://openrouter.ai/api/v1/chat/completions" @?= "https://openrouter.ai/api"
        normalizeBaseUrl "http://localhost:11434/v1" @?= "http://localhost:11434"
    , testCase "stream emits chunks and ends at [DONE]" $ do
        result <- collectStream [chunk "Hel", chunk "lo", done]
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [ LLMStart {}
              , LLMChunk _ "Hel" Nothing
              , LLMChunk _ "lo" Nothing
              , LLMEnd _ responseMessage Nothing
              ] -> extractMessageText responseMessage @?= "Hello"
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
                LLMEnd _ responseMessage Nothing : _ ->
                  extractMessageText responseMessage @?= "Hello"
                _ -> assertFailure $ "Expected a completed stream, got: " ++ show events
    , testCase "stream finishes when the SSE connection closes" $ do
        result <- collectStream [chunk "Hello"]
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [LLMStart {}, LLMChunk _ "Hello" Nothing, LLMEnd _ responseMessage Nothing] ->
              extractMessageText responseMessage @?= "Hello"
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream ignores chunks without choices" $ do
        result <- collectStream [emptyChoices, done]
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [LLMStart {}, LLMEnd _ responseMessage Nothing] ->
              extractMessageText responseMessage @?= ""
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream converts malformed SSE data to LangchainError" $ do
        result <- collectStream [TestSseEvent "not JSON"]
        case result of
          Left _ -> pure ()
          Right events -> assertFailure $ "Expected stream failure, got: " ++ show events
    , testCase "stream converts HTTP errors to LangchainError" $ do
        result <- withErrorProvider $ \provider ->
          runResourceT $ runExceptT $ collectEvents (stream provider [userMessage "Hello"] Nothing)
        case result of
          Left _ -> pure ()
          Right events -> assertFailure $ "Expected stream failure, got: " ++ show events
    , testCase "stream handles SSE frames written in multiple pieces" $ do
        let TestSseEvent event = chunk "Hello"
            frame = "data: " <> event <> "\n\n"
            splitPoint = LBS.length frame `div` 2
            fragments = [LBS.take splitPoint frame, LBS.drop splitPoint frame, "data: [DONE]\n\n"]
        result <- collectRawStream fragments
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [LLMStart {}, LLMChunk _ "Hello" Nothing, LLMEnd _ responseMessage Nothing] ->
              extractMessageText responseMessage @?= "Hello"
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream requests usage in stream options" $ do
        requestBody <- newEmptyMVar
        withRequestCapturingProvider (putMVar requestBody) $ \provider -> do
          void . runResourceT . runExceptT $ collectEvents (stream provider [userMessage "Hello"] Nothing)
        mbRequest <- takeMVar requestBody
        case mbRequest of
          Nothing -> assertFailure "Expected JSON request body"
          Just (Aeson.Object fields) -> do
            KeyMap.lookup "stream" fields @?= Just (Aeson.Bool True)
            KeyMap.lookup "stream_options" fields @?= Just (Aeson.object ["include_usage" Aeson..= True])
          Just request -> assertFailure $ "Expected JSON object, got: " ++ show request
    , testCase "stream sends tool definitions and tool choice" $ do
        let weatherTool :: Tool IO
            weatherTool = createTool "get_weather" "Gets the weather" (Aeson.object []) (const $ pure $ Right "sunny")
            config = openAITools [weatherTool] (OpenAIToolFunction "get_weather")
        requestBody <- newEmptyMVar
        withRequestCapturingProvider (putMVar requestBody) $ \provider -> do
          void . runResourceT . runExceptT $
            collectEvents (stream provider [userMessage "Hello"] (Just config))
        mbRequest <- takeMVar requestBody
        case mbRequest of
          Just (Aeson.Object fields) -> do
            KeyMap.lookup "tools" fields @?= Just (Aeson.toJSON [toolToValue weatherTool])
            KeyMap.lookup "tool_choice" fields
              @?= Just
                ( Aeson.object
                    [ "type" Aeson..= ("function" :: T.Text)
                    , "function" Aeson..= Aeson.object ["name" Aeson..= ("get_weather" :: T.Text)]
                    ]
                )
          Just request -> assertFailure $ "Expected JSON object, got: " ++ show request
          Nothing -> assertFailure "Expected JSON request body"
    , testCase "stream sends assistant tool calls before tool results" $ do
        let toolCall =
              ToolCall
                "call_weather"
                "function"
                "get_weather"
                (Aeson.object ["city" Aeson..= ("Paris" :: T.Text)])
            assistant = (assistantMessage "") {messageToolCalls = Just [toolCall]}
            toolResult = (textMessage Tool "Sunny") {messageToolId = Just "call_weather"}
        requestBody <- newEmptyMVar
        withRequestCapturingProvider (putMVar requestBody) $ \provider -> do
          void . runResourceT . runExceptT $
            collectEvents (stream provider [userMessage "Weather?", assistant, toolResult] Nothing)
        mbRequest <- takeMVar requestBody
        case mbRequest of
          Just (Aeson.Object fields) -> case KeyMap.lookup "messages" fields of
            Just (Aeson.Array messages) -> case V.toList messages of
              [_, Aeson.Object assistantFields, Aeson.Object toolResultFields] -> do
                KeyMap.lookup "tool_calls" assistantFields
                  @?= Just
                    ( Aeson.toJSON
                        [ Aeson.object
                            [ "id" Aeson..= ("call_weather" :: T.Text)
                            , "type" Aeson..= ("function" :: T.Text)
                            , "function"
                                Aeson..= Aeson.object
                                  [ "name" Aeson..= ("get_weather" :: T.Text)
                                  , "arguments" Aeson..= ("{\"city\":\"Paris\"}" :: T.Text)
                                  ]
                            ]
                        ]
                    )
                KeyMap.lookup "tool_call_id" toolResultFields @?= Just (Aeson.String "call_weather")
              messages' -> assertFailure $ "Expected three request messages, got: " ++ show messages'
            request -> assertFailure $ "Expected messages array, got: " ++ show request
          Just request -> assertFailure $ "Expected JSON object, got: " ++ show request
          Nothing -> assertFailure "Expected JSON request body"
    , testCase "stream accumulates text, fragmented tool calls, and usage" $ do
        let frames =
              [ sseFrame
                  "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Checking weather...\"},\"finish_reason\":null}]}"
              , sseFrame
                  "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"get_weather\",\"arguments\":\"{\\\"city\\\":\\\"\"}}]},\"finish_reason\":null}]}"
              , sseFrame
                  "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"Paris\\\"}\"}}]},\"finish_reason\":\"tool_calls\"}]}"
              , sseFrame
                  "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[],\"usage\":{\"prompt_tokens\":7,\"completion_tokens\":5,\"total_tokens\":12}}"
              , sseFrame "[DONE]"
              ]
            expectedToolCall =
              ToolCall
                { toolCallId = "call_1"
                , toolCallType = "function"
                , toolCallName = "get_weather"
                , toolCallArguments = Aeson.object ["city" Aeson..= ("Paris" :: T.Text)]
                }
            expectedUsage = TokenUsage 7 5 12
        result <- collectRawStream frames
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [ LLMStart {}
              , LLMChunk _ "Checking weather..." Nothing
              , LLMChunk _ "" (Just toolCall)
              , LLMEnd _ responseMessage (Just usage)
              ] -> do
                toolCall @?= expectedToolCall
                extractMessageText responseMessage @?= "Checking weather..."
                messageToolCalls responseMessage @?= Just [expectedToolCall]
                usage @?= expectedUsage
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream rejects invalid completed tool arguments" $ do
        let frames =
              [ sseFrame
                  "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"get_weather\",\"arguments\":\"not-json\"}}]},\"finish_reason\":\"tool_calls\"}]}"
              , sseFrame "[DONE]"
              ]
        result <- collectRawStream frames
        case result of
          Left _ -> pure ()
          Right events -> assertFailure $ "Expected stream failure, got: " ++ show events
    , testCase "stream closes the SSE connection when the consumer stops after a chunk" $ do
        clientClosed <- newEmptyMVar
        withCancellationAwareProvider (putMVar clientClosed ()) $ \provider -> do
          void . runResourceT . runExceptT . runConduit $
            stream provider [userMessage "Hello"] Nothing .| (await >> await)
          closed <- timeout 500000 $ takeMVar clientClosed
          assertBool "expected the SSE connection to close" $ isJust closed
    ]

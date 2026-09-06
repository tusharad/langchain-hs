{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.OpenAI (tests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (async, poll, wait)
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import Control.Monad (forM, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (await, runConduit, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Types (status500)
import Network.Wai (Application, responseLBS)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), TokenUsage (..), collectEvents)
import Langchain.Core.Tool (Tool, createTool, toolToValue)
import qualified Langchain.Core.Tool as CoreTool
import Langchain.Provider.OpenAI
import Test.Langchain.Provider.TestSseServer
  ( cancellationAwareSseServer
  , capturingRawSseServer
  , collectModelStream
  , gatedSseServer
  , rawSseServer
  , sseFrame
  , withTestApplication
  )

withErrorProvider :: (OpenAI -> IO a) -> IO a
withErrorProvider action =
  withTestApplication errorServer $ \url -> withOpenAIProvider url action

withRawTestProvider :: [LBS.ByteString] -> (OpenAI -> IO a) -> IO a
withRawTestProvider frames action =
  withTestApplication (rawSseServer frames) $ \url -> withOpenAIProvider url action

withRequestCapturingProvider :: (Maybe Value -> IO ()) -> (OpenAI -> IO a) -> IO a
withRequestCapturingProvider captureRequest action =
  withTestApplication (capturingRawSseServer (captureRequest . Aeson.decode) [sseFrame "[DONE]"]) $ \url ->
    withOpenAIProvider url action

withCancellationAwareProvider :: IO () -> (OpenAI -> IO a) -> IO a
withCancellationAwareProvider signalClientClosed action =
  withTestApplication (cancellationAwareSseServer (sseFrame $ chunk "Hello") signalClientClosed) $ \url ->
    withOpenAIProvider url action

withGatedProvider :: IO () -> (OpenAI -> IO a) -> IO a
withGatedProvider waitForContinuation action =
  withTestApplication
    ( gatedSseServer
        (sseFrame $ chunk "Hel")
        waitForContinuation
        [sseFrame (chunk "lo"), sseFrame "[DONE]"]
    )
    $ \url -> withOpenAIProvider url action

withOpenAIProvider :: T.Text -> (OpenAI -> IO a) -> IO a
withOpenAIProvider url action =
  action $ (newOpenAI "test-key" "test-model") {baseUrl = url}

errorServer :: Application
errorServer _request respond = respond $ responseLBS status500 [] ""

collectRawStream :: [LBS.ByteString] -> IO (Either LangchainError [StreamEvent])
collectRawStream frames =
  withRawTestProvider frames $ \provider ->
    collectModelStream provider [userMessage "Hello"] Nothing

chunk :: LBS.ByteString -> LBS.ByteString
chunk content =
  "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\""
    <> content
    <> "\"},\"finish_reason\":null}]}"

emptyChoices :: LBS.ByteString
emptyChoices =
  "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion.chunk\",\"created\":0,\"model\":\"test-model\",\"choices\":[]}"

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.OpenAI"
    [ testCase "newOpenAI initializes default provider" $ do
        let p = newOpenAI "sk-test" "gpt-4o"
        model p @?= "gpt-4o"
        baseUrl p @?= "https://api.openai.com"
    , testCase "openAICompatible initializes custom endpoint" $ do
        let p = openAICompatible "sk-test" "custom-llm" "https://custom-ai.example.com"
        model p @?= "custom-llm"
        baseUrl p @?= "https://custom-ai.example.com"
    , testCase "live OpenAI stream emits text and usage" $ do
        mbApiKey <- lookupEnv "OPENAI_API_KEY"
        case mbApiKey of
          Nothing -> putStrLn " [SKIPPED] OPENAI_API_KEY is not set"
          Just envApiKey -> do
            envModel <- fromMaybe "gpt-4o-mini" <$> lookupEnv "OPENAI_STREAM_TEST_MODEL"
            result <-
              timeout 60000000
                $ runResourceT
                $ runExceptT
                $ collectEvents
                $ stream
                  (newOpenAI (T.pack envApiKey) (T.pack envModel))
                  [userMessage "Reply with exactly OK."]
                  Nothing
            case result of
              Nothing -> assertFailure "OpenAI stream timed out"
              Just (Left err) -> assertFailure $ "Expected stream success, got: " ++ show err
              Just (Right events) -> do
                print events
                case reverse events of
                  LLMEnd _ responseMessage (Just usage) : _ -> do
                    assertBool "Expected non-empty streamed text" $ not $ T.null $ extractMessageText responseMessage
                    assertBool "Expected positive total token usage" $ totalTokens usage > 0
                  _ -> assertFailure $ "Expected LLMEnd with usage, got: " ++ show events
    , testCase "live OpenAI stream invokes a tool and continues with its result" $ do
        mbApiKey <- lookupEnv "OPENAI_API_KEY"
        case mbApiKey of
          Nothing -> putStrLn " [SKIPPED] OPENAI_API_KEY is not set"
          Just envApiKey -> do
            envModel <- fromMaybe "gpt-4o-mini" <$> lookupEnv "OPENAI_STREAM_TEST_MODEL"
            let weatherTool :: Tool IO
                weatherTool =
                  createTool
                    "get_weather"
                    "Returns the current weather for a city."
                    ( Aeson.object
                        [ "type" Aeson..= ("object" :: T.Text)
                        , "properties"
                            Aeson..= Aeson.object
                              [ "city" Aeson..= Aeson.object ["type" Aeson..= ("string" :: T.Text)]
                              ]
                        , "required" Aeson..= ["city" :: T.Text]
                        , "additionalProperties" Aeson..= False
                        ]
                    )
                    (const $ pure $ Right "The weather in Paris is sunny and 22 C.")
                provider = newOpenAI (T.pack envApiKey) (T.pack envModel)
                runLive messages config =
                  timeout 60000000
                    $ runResourceT
                    $ runExceptT
                    $ collectEvents
                    $ stream provider messages config
                prompt = userMessage "Use get_weather to look up the weather in Paris, then answer using the tool result."

            firstResult <-
              runLive [prompt] (Just $ openAITools [weatherTool] (OpenAIToolFunction "get_weather"))
            firstEvents <- case firstResult of
              Nothing -> assertFailure "OpenAI tool-call stream timed out" >> fail "unreachable"
              Just (Left err) -> assertFailure ("Expected tool-call stream success, got: " ++ show err) >> fail "unreachable"
              Just (Right events) -> pure events
            (assistant, toolCalls) <- case reverse firstEvents of
              LLMEnd _ responseMessage _ : _ -> case messageToolCalls responseMessage of
                Just calls@[toolCall]
                  | toolCallName toolCall == "get_weather" -> pure (responseMessage, calls)
                _ -> assertFailure ("Expected OpenAI tool call, got: " ++ show firstEvents) >> fail "unreachable"
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
              Nothing -> assertFailure "OpenAI tool-result stream timed out"
              Just (Left err) -> assertFailure $ "Expected tool-result stream success, got: " ++ show err
              Just (Right events) -> case reverse events of
                LLMEnd _ responseMessage (Just usage) : _ -> do
                  assertBool "Expected final text after tool result"
                    $ not
                    $ T.null
                    $ extractMessageText responseMessage
                  assertBool "Expected positive total token usage" $ totalTokens usage > 0
                _ -> assertFailure $ "Expected LLMEnd with usage, got: " ++ show events
    , testCase "normalizeBaseUrl strips endpoint paths for servant compatibility" $ do
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
        result <- collectRawStream [sseFrame $ chunk "Hel", sseFrame $ chunk "lo", sseFrame "[DONE]"]
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
        result <- collectRawStream [sseFrame $ chunk "Hello"]
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [LLMStart {}, LLMChunk _ "Hello" Nothing, LLMEnd _ responseMessage Nothing] ->
              extractMessageText responseMessage @?= "Hello"
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream ignores chunks without choices" $ do
        result <- collectRawStream [sseFrame emptyChoices, sseFrame "[DONE]"]
        case result of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> case events of
            [LLMStart {}, LLMEnd _ responseMessage Nothing] ->
              extractMessageText responseMessage @?= ""
            _ -> assertFailure $ "Unexpected stream events: " ++ show events
    , testCase "stream converts malformed SSE data to LangchainError" $ do
        result <- collectRawStream [sseFrame "not JSON"]
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
        let frame = sseFrame $ chunk "Hello"
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Langchain.Provider.AnthropicSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (ToJSON (..), Value (..), decodeStrict, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (HeaderName, hContentType, status200, status401)
import Network.Wai (Application, requestHeaders, responseLBS, responseStream, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Langchain.Cache.Core (CacheableChatModel (..))
import Langchain.Error (errorMessage)
import Langchain.Model
import Langchain.Stream (StreamEvent (..), TokenUsage (..), collectEvents, liftChatStreamResource)
import Langchain.Tool (Tool, createTool)
import Langchain.Tool.Binding (ToolBinder (..))

import Langchain.Provider.Anthropic

withTestServer :: Application -> (Text -> IO a) -> IO a
withTestServer app action =
  testWithApplication (pure app) $ \port ->
    action $ "http://127.0.0.1:" <> T.pack (show port)

sseEvent :: BS.ByteString -> LBS.ByteString -> LBS.ByteString
sseEvent evName payload =
  "event: " <> LBS.fromStrict evName <> "\ndata: " <> payload <> "\n\n"

findHeader :: HeaderName -> [(HeaderName, BS.ByteString)] -> Maybe BS.ByteString
findHeader = lookup

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.Anthropic"
    [ testCase "newAnthropic sets default base URL and version" $ do
        let p = newAnthropic "sk-ant-secret" "claude-sonnet-4-5"
        anthropicApiKey p @?= "sk-ant-secret"
        anthropicModel p @?= "claude-sonnet-4-5"
        anthropicBaseUrl p @?= "https://api.anthropic.com"
        anthropicVersion p @?= "2023-06-01"
    , testCase "ToolBinder merges tools as Anthropic input_schema definitions" $ do
        let weatherTool :: Tool IO
            weatherTool =
              createTool
                "get_weather"
                "Fetch weather"
                (object ["type" .= ("object" :: Text), "properties" .= object []])
                (const $ pure $ Right "sunny")
            cfg = bindToolsConfig @Anthropic [weatherTool] (Just defaultAnthropicConfig)
        fmap (length . fromMaybe [] . configTools) cfg @?= Just 1
        case cfg >>= configTools of
          Just [t] -> do
            anthropicToolName t @?= "get_weather"
            anthropicToolDescription t @?= "Fetch weather"
          other -> assertFailure $ "Expected 1 tool, got: " ++ show other
    , testCase "cacheModelIdentity distinguishes configs cleanly" $ do
        let p = newAnthropic "sk-ant-1" "claude-sonnet-4-5"
            id1 = cacheModelIdentity p (Just defaultAnthropicConfig)
            id2 = cacheModelIdentity p (Just $ withThinkingBudget 2048 defaultAnthropicConfig)
            id3 = cacheModelIdentity p (Just $ withPromptCaching True defaultAnthropicConfig)
        assertBool "Thinking budget changes identity" (id1 /= id2)
        assertBool "Prompt caching changes identity" (id1 /= id3)
    , testCase "invoke sends expected Anthropic JSON, headers, system, tool_use -> tool_result" $ do
        capturedReq <- newEmptyMVar
        let respJson =
              object
                [ "id" .= ("msg_123" :: Text)
                , "type" .= ("message" :: Text)
                , "role" .= ("assistant" :: Text)
                , "model" .= ("claude-sonnet-4-5" :: Text)
                , "content"
                    .= [ object ["type" .= ("text" :: Text), "text" .= ("Checking..." :: Text)]
                       , object
                           [ "type" .= ("tool_use" :: Text)
                           , "id" .= ("toolu_1" :: Text)
                           , "name" .= ("get_weather" :: Text)
                           , "input" .= object ["city" .= ("Paris" :: Text)]
                           ]
                       ]
                , "usage" .= object ["input_tokens" .= (11 :: Int), "output_tokens" .= (7 :: Int)]
                ]
            validApp :: Application
            validApp req respond = do
              body <- strictRequestBody req
              putMVar capturedReq (requestHeaders req, body)
              respond $
                responseLBS
                  status200
                  [(hContentType, "application/json")]
                  (encode respJson)
        let tc = ToolCall "toolu_0" "function" "lookup" (object ["q" .= ("hs" :: Text)])
            assistantTurn = (assistantMessage "Searching") {messageToolCalls = Just [tc]}
            toolTurn = (textMessage Tool "found") {messageToolId = Just "toolu_0"}
            cfg =
              withPromptCaching True $
                withThinkingBudget 1024 $
                  withMaxTokens 2048 defaultAnthropicConfig
        res <- withTestServer validApp $ \base -> do
          let provider = newAnthropicWithBaseUrl "sk-ant-test-key" "claude-sonnet-4-5" base
          runExceptT $
            invoke provider [systemMessage "Be concise.", userMessage "Hi", assistantTurn, toolTurn] (Just cfg)
        (hdrs, rawBody) <- takeMVar capturedReq
        findHeader "x-api-key" hdrs @?= Just "sk-ant-test-key"
        findHeader "anthropic-version" hdrs @?= Just "2023-06-01"
        case decodeStrict (LBS.toStrict rawBody) of
          Just (Object o) -> do
            KM.lookup "max_tokens" o @?= Just (toJSON (2048 :: Int))
            KM.lookup "thinking" o
              @?= Just (object ["type" .= ("enabled" :: Text), "budget_tokens" .= (1024 :: Int)])
            KM.lookup "system" o
              @?= Just
                ( toJSON
                    [ object
                        [ "type" .= ("text" :: Text)
                        , "text" .= ("Be concise." :: Text)
                        , "cache_control" .= object ["type" .= ("ephemeral" :: Text)]
                        ]
                    ]
                )
            case KM.lookup "messages" o of
              Just (Array msgs) -> length msgs @?= 3
              other -> assertFailure $ "Expected 3 messages, got: " ++ show other
          other -> assertFailure $ "Expected JSON object body, got: " ++ show other
        case res of
          Left err -> assertFailure $ "Expected invoke success, got: " ++ show err
          Right msg -> do
            extractMessageText msg @?= "Checking..."
            messageToolCalls msg
              @?= Just [ToolCall "toolu_1" "function" "get_weather" (object ["city" .= ("Paris" :: Text)])]
    , testCase "invoke redacts x-api-key on HTTP 4xx errors" $ do
        let secret = "sk-ant-very-secret-token"
            errApp :: Application
            errApp _ respond =
              respond $
                responseLBS
                  status401
                  [(hContentType, "application/json")]
                  (LBS.fromStrict $ TE.encodeUtf8 $ "Authentication failed for key " <> secret)
        res <- withTestServer errApp $ \base -> do
          let provider = newAnthropicWithBaseUrl secret "claude-sonnet-4-5" base
          runExceptT $ invoke provider [userMessage "Hi"] Nothing
        case res of
          Left err -> do
            assertBool "API key must be redacted" (not (secret `T.isInfixOf` errorMessage err))
            assertBool "Contains REDACTED marker" ("[REDACTED]" `T.isInfixOf` errorMessage err)
          Right msg -> assertFailure $ "Expected 401 error, got: " ++ show msg
    , testCase "messagesToAnthropic encodes ImageBase64 and ImageUrl blocks" $ do
        let imgB64 = imageMessage User "image/jpeg" "AQID"
            imgUrlMsg =
              Message
                User
                (ImageBlock (ImageContent (ImageUrl "https://example.com/a.png") Nothing Nothing) :| [])
                Nothing
                Nothing
                Nothing
                Map.empty
            (sys, msgs) = messagesToAnthropic Nothing [imgB64, imgUrlMsg]
        sys @?= Nothing
        length msgs @?= 1
    , testCase "stream parses SSE text_delta, fragmented input_json_delta, and TokenUsage" $ do
        let frames =
              [ sseEvent
                  "message_start"
                  "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_s1\",\"model\":\"claude-sonnet-4-5\",\"usage\":{\"input_tokens\":10,\"output_tokens\":0}}}"
              , sseEvent
                  "content_block_start"
                  "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
              , sseEvent
                  "content_block_delta"
                  "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Checking \"}}"
              , sseEvent "content_block_stop" "{\"type\":\"content_block_stop\",\"index\":0}"
              , sseEvent
                  "content_block_start"
                  "{\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_9\",\"name\":\"calc\"}}"
              , sseEvent
                  "content_block_delta"
                  "{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"x\\\":\"}}"
              , sseEvent
                  "content_block_delta"
                  "{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"42}\"}}"
              , sseEvent "content_block_stop" "{\"type\":\"content_block_stop\",\"index\":1}"
              , sseEvent
                  "message_delta"
                  "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":6}}"
              , sseEvent "message_stop" "{\"type\":\"message_stop\"}"
              ]
            app :: Application
            app _ respond =
              respond $
                responseStream status200 [(hContentType, "text/event-stream")] $ \write flush ->
                  mapM_ (\f -> write (Builder.lazyByteString f) >> flush) frames
        res <- withTestServer app $ \base -> do
          let provider = newAnthropicWithBaseUrl "sk-ant-1" "claude-sonnet-4-5" base
          runResourceT $ runExceptT $ collectEvents (stream provider [userMessage "Compute"] Nothing)
        case res of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> do
            let expectedTc = ToolCall "toolu_9" "function" "calc" (object ["x" .= (42 :: Int)])
            events
              @?= [ LLMStart "msg_s1" "claude-sonnet-4-5" [userMessage "Compute"]
                  , LLMChunk "msg_s1" "Checking " Nothing
                  , LLMChunk "msg_s1" "" (Just expectedTc)
                  , LLMEnd
                      "msg_s1"
                      ((assistantMessage "Checking ") {messageToolCalls = Just [expectedTc]})
                      (Just (TokenUsage 10 6 16))
                  ]
    , testCase "stream handles 0-arg tool_use block with empty input_json_delta" $ do
        let frames =
              [ sseEvent
                  "message_start"
                  "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_z\",\"usage\":{\"input_tokens\":3}}}"
              , sseEvent
                  "content_block_start"
                  "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_z\",\"name\":\"ping\"}}"
              , sseEvent "content_block_stop" "{\"type\":\"content_block_stop\",\"index\":0}"
              , sseEvent "message_delta" "{\"type\":\"message_delta\",\"usage\":{\"output_tokens\":2}}"
              , sseEvent "message_stop" "{\"type\":\"message_stop\"}"
              ]
            app :: Application
            app _ respond =
              respond $
                responseStream status200 [(hContentType, "text/event-stream")] $ \write flush ->
                  mapM_ (\f -> write (Builder.lazyByteString f) >> flush) frames
        res <- withTestServer app $ \base -> do
          let provider = newAnthropicWithBaseUrl "sk-ant-1" "claude-sonnet-4-5" base
          runResourceT $ runExceptT $ collectEvents (stream provider [userMessage "Ping"] Nothing)
        case res of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right events -> do
            let deltas = [tc | LLMChunk _ _ (Just tc) <- events]
            deltas @?= [ToolCall "toolu_z" "function" "ping" (object [])]
    , testCase "stream delivers first LLMChunk before gated SSE response finishes" $ do
        releaseTail <- newEmptyMVar
        seenChunks <- newTVarIO ([] :: [Text])
        let headFrame =
              sseEvent "message_start" "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_gate\"}}"
                <> sseEvent
                  "content_block_start"
                  "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}"
                <> sseEvent
                  "content_block_delta"
                  "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"first \"}}"
            tailFrames =
              [ sseEvent
                  "content_block_delta"
                  "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"second\"}}"
              , sseEvent "message_stop" "{\"type\":\"message_stop\"}"
              ]
            app :: Application
            app _ respond =
              respond $
                responseStream status200 [(hContentType, "text/event-stream")] $ \write flush -> do
                  write (Builder.lazyByteString headFrame)
                  flush
                  takeMVar releaseTail
                  mapM_ (\f -> write (Builder.lazyByteString f) >> flush) tailFrames
        res <- withTestServer app $ \base -> do
          let provider = newAnthropicWithBaseUrl "sk-ant-1" "claude-sonnet-4-5" base
          runner <-
            async $
              runResourceT $
                runExceptT $
                  runConduit $
                    stream provider [userMessage "Gated"] Nothing
                      .| CL.mapM_
                        ( \case
                            LLMChunk _ txt Nothing
                              | not (T.null txt) ->
                                  liftIO $ atomically $ modifyTVar' seenChunks (++ [txt])
                            _ -> pure ()
                        )
          let waitFirst :: Int -> IO ()
              waitFirst 0 = assertFailure "Timed out waiting for first gated chunk"
              waitFirst k = do
                xs <- readTVarIO seenChunks
                if xs == ["first "] then pure () else threadDelay 10000 >> waitFirst (k - 1)
          waitFirst 100
          putMVar releaseTail ()
          wait runner
        case res of
          Left err -> assertFailure $ "Expected stream success, got: " ++ show err
          Right () -> pure ()
        finalChunks <- readTVarIO seenChunks
        finalChunks @?= ["first ", "second"]
    , testCase "stream raises LLMError on invalid tool_use JSON" $ do
        let frames =
              [ sseEvent "message_start" "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_bad\"}}"
              , sseEvent
                  "content_block_start"
                  "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_bad\",\"name\":\"broken\"}}"
              , sseEvent
                  "content_block_delta"
                  "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{not valid json\"}}"
              , sseEvent "content_block_stop" "{\"type\":\"content_block_stop\",\"index\":0}"
              ]
            app :: Application
            app _ respond =
              respond $
                responseStream status200 [(hContentType, "text/event-stream")] $ \write flush ->
                  mapM_ (\f -> write (Builder.lazyByteString f) >> flush) frames
        res <- withTestServer app $ \base -> do
          let provider = newAnthropicWithBaseUrl "sk-ant-1" "claude-sonnet-4-5" base
          runResourceT $ runExceptT $ collectEvents (stream provider [userMessage "Broken"] Nothing)
        case res of
          Left _ -> pure ()
          Right evs -> assertFailure $ "Expected error on invalid tool JSON, got: " ++ show evs
    , testCase "early consumer close tears down SSE HTTP connection" $ do
        disconnected <- newEmptyMVar
        let firstFrame =
              sseEvent "message_start" "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_disc\"}}"
                <> sseEvent
                  "content_block_start"
                  "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}"
                <> sseEvent
                  "content_block_delta"
                  "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}"
            app :: Application
            app _ respond =
              respond $
                responseStream status200 [(hContentType, "text/event-stream")] $ \write flush -> do
                  let loop = do
                        write ": keepalive\n\n"
                        flush
                        threadDelay 5000
                        loop
                  write (Builder.lazyByteString firstFrame)
                  flush
                  loop `catch` (\(_ :: SomeException) -> putMVar disconnected ())
        _ <- withTestServer app $ \base -> do
          let provider = newAnthropicWithBaseUrl "sk-ant-1" "claude-sonnet-4-5" base
          runResourceT $
            runExceptT $
              runConduit $
                liftChatStreamResource (stream provider [userMessage "Disconnect"] Nothing)
                  .| CL.take 2
        threadDelay 100000
        mbDisc <- tryReadMVar disconnected
        mbDisc @?= Just ()
    , testProperty "splitSseFrames preserves frames under arbitrary chunk splits" $
        \(Positive chunkLenRaw) ->
          let frame1 = "event: ping\ndata: {\"a\":1}"
              frame2 = "event: message_stop\ndata: {\"b\":2}"
              full = frame1 <> "\n\n" <> frame2 <> "\n\n"
              stepSize = max 1 (chunkLenRaw `mod` 13)
              chunks = chunkEvery stepSize full
              (parsed, remBuf) = feedAll chunks
           in parsed === [frame1, frame2] .&&. remBuf === BS.empty
    ]
  where
    chunkEvery n bs
      | BS.null bs = []
      | otherwise = let (h, t) = BS.splitAt n bs in h : chunkEvery n t
    feedAll = go BS.empty []
      where
        go buf acc [] = (concat (reverse acc), buf)
        go buf acc (c : cs) =
          let (fs, buf') = splitSseFrames (buf <> c)
           in go buf' (fs : acc) cs

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Cache.CacheSpec (tests) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Except (runExceptT)
import Data.Aeson (object, (.=))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Cache.Core
import Langchain.Core.Model
  ( ChatModel (..)
  , ContentBlock (..)
  , ImageContent (..)
  , ImageSource (..)
  , Message (..)
  , Role (..)
  , ToolCall (..)
  , assistantMessage
  , extractMessageText
  , userMessage
  )
import Langchain.Provider.Gemini (Gemini (Gemini))
import Langchain.Provider.Ollama (Ollama, newOllamaWithClient)
import Langchain.Provider.OpenAI (OpenAI (OpenAI))
import qualified Ollama.API.Chat as OllamaChat
import Ollama.Client (newClient)
import qualified Ollama.Client.Config as OllamaClientConfig
import Ollama.Types.Common (ModelName (..), Think (..))
import Ollama.Types.Format (Format (..))
import qualified Ollama.Types.Message as OllamaMessage
import Ollama.Types.Options (ModelOptions (..), defaultOptions)
import Ollama.Types.Tool (FunctionDef (..))
import qualified Ollama.Types.Tool as OllamaTool
import Test.Langchain.Provider.Mock (MockModel (..), newMockModel)

testMessages :: [Message]
testMessages = [userMessage "Describe the image"]

baseOllamaRequest :: OllamaChat.ChatRequest
baseOllamaRequest =
  OllamaChat.chatRequest
    (ModelName "ignored-model")
    (OllamaMessage.userMessage "ignored-message" :| [])

newOllamaForEndpoint :: Text -> IO Ollama
newOllamaForEndpoint endpoint = do
  ollamaClient <-
    newClient $
      OllamaClientConfig.defaultConfig
        { OllamaClientConfig.configBaseUrl = endpoint
        }
  pure $ newOllamaWithClient "llama3.2" ollamaClient

assertKeysDiffer :: Text -> Text -> Assertion
assertKeysDiffer first second =
  assertBool "Expected cache keys to differ" (first /= second)

tests :: TestTree
tests =
  testGroup
    "Langchain.Cache.CacheSpec"
    [ testCase "InMemoryCache stores and retrieves cached message" $ do
        cache <- newInMemoryCache
        let msg = assistantMessage "Cached result"
        putCache cache "key1" msg
        res <- getCache cache "key1"
        res @?= Just msg
        clearCache cache
        resAfter <- getCache cache "key1"
        resAfter @?= Nothing
    , testCase "SQLiteCache stores and persists message across queries" $ do
        withSystemTempDirectory "sqlite-cache-test" $ \tmpDir -> do
          let dbPath = tmpDir </> "cache.db"
          cache <- newSQLiteCache dbPath
          let msg = assistantMessage "SQLite Cached"
          putCache cache "keyA" msg
          res <- getCache cache "keyA"
          res @?= Just msg
    , testCase "CachedModel caches response and returns cached on second call" $ do
        _ <- newTVarIO (0 :: Int)
        let mockModel = newMockModel "Dynamic Output"
        cache <- newInMemoryCache
        let cachedModel = withCaching mockModel cache
            msgs = [userMessage "Compute 2+2"]
        res1 <- runExceptT $ invoke cachedModel msgs Nothing
        res2 <- runExceptT $ invoke cachedModel msgs Nothing
        case (res1, res2) of
          (Right m1, Right m2) -> do
            extractMessageText m1 @?= "Dynamic Output"
            extractMessageText m2 @?= "Dynamic Output"
          _ -> assertFailure "Expected successful CachedModel invocations"
    , testCase "cache key is stable for identical inputs" $ do
        let mockModel = newMockModel "Dynamic Output"
        computeCacheKey mockModel Nothing testMessages
          @?= computeCacheKey mockModel Nothing testMessages
    , testCase "cache key distinguishes complete message content" $ do
        let mockModel = newMockModel "Dynamic Output"
            imageMessage =
              Message
                User
                ( TextBlock "Describe the image"
                    :| [ImageBlock $ ImageContent (ImageUrl "https://example.com/image.png") Nothing Nothing]
                )
                Nothing
                Nothing
                Nothing
            toolMessage =
              (userMessage "Describe the image")
                { messageToolCalls = Just [ToolCall "call-1" "function" "describe_image" (object [])]
                }
            baseKey = computeCacheKey mockModel Nothing testMessages
        assertKeysDiffer baseKey $ computeCacheKey mockModel Nothing [imageMessage]
        assertKeysDiffer baseKey $ computeCacheKey mockModel Nothing [toolMessage]
    , testCase "cache key distinguishes mock model identity" $ do
        let first = newMockModel "first response"
            second = MockModel "first response" "other-mock"
        assertKeysDiffer
          (computeCacheKey first Nothing testMessages)
          (computeCacheKey second Nothing testMessages)
    , testCase "cache key distinguishes OpenAI identity and ignores its config" $ do
        let base = OpenAI "key" "gpt-4o" "https://api.openai.com/v1/chat/completions" (Just 0.7)
            otherModel = OpenAI "key" "gpt-4.1" "https://api.openai.com/v1/chat/completions" (Just 0.7)
            otherEndpoint = OpenAI "key" "gpt-4o" "https://example.com/v1/chat/completions" (Just 0.7)
            otherTemperature = OpenAI "key" "gpt-4o" "https://api.openai.com/v1/chat/completions" (Just 0.2)
            baseKey = computeCacheKey base Nothing testMessages
        assertKeysDiffer baseKey $ computeCacheKey otherModel Nothing testMessages
        assertKeysDiffer baseKey $ computeCacheKey otherEndpoint Nothing testMessages
        assertKeysDiffer baseKey $ computeCacheKey otherTemperature Nothing testMessages
        baseKey @?= computeCacheKey base (Just $ object ["unused" .= True]) testMessages
    , testCase "cache key distinguishes Gemini identity and ignores its config" $ do
        let base = Gemini "key" "gemini-2.0-flash"
            otherModel = Gemini "key" "gemini-2.5-pro"
            baseKey = computeCacheKey base Nothing testMessages
        assertKeysDiffer baseKey $ computeCacheKey otherModel Nothing testMessages
        baseKey @?= computeCacheKey base (Just $ object ["unused" .= True]) testMessages
    , testCase "cache key ignores MockModel config" $ do
        let mockModel = newMockModel "Dynamic Output"
        computeCacheKey mockModel Nothing testMessages
          @?= computeCacheKey mockModel (Just ()) testMessages
    , testCase "cache key distinguishes Ollama endpoints and effective config" $ do
        firstEndpoint <- newOllamaForEndpoint "http://ollama-one.example.com:11434"
        secondEndpoint <- newOllamaForEndpoint "http://ollama-two.example.com:11434"
        let baseKey = computeCacheKey firstEndpoint (Just baseOllamaRequest) testMessages
            ignoredFieldsRequest =
              baseOllamaRequest
                { OllamaChat.chatModel = ModelName "another-ignored-model"
                , OllamaChat.chatMessages = OllamaMessage.userMessage "another-ignored-message" :| []
                , OllamaChat.chStream = Just True
                }
            requestsThatChangeOutput =
              [ baseOllamaRequest
                  { OllamaChat.chatTools =
                      Just [OllamaTool.Tool "function" (FunctionDef "get_weather" Nothing Nothing Nothing)]
                  }
              , baseOllamaRequest {OllamaChat.chatFormat = Just JsonFormat}
              , baseOllamaRequest {OllamaChat.chatOptions = Just defaultOptions {optTemperature = Just 0.2}}
              , baseOllamaRequest {OllamaChat.chatKeepAlive = Just "10m"}
              , baseOllamaRequest {OllamaChat.chatThink = Just ThinkEnabled}
              ]
        assertKeysDiffer baseKey $ computeCacheKey secondEndpoint (Just baseOllamaRequest) testMessages
        baseKey @?= computeCacheKey firstEndpoint Nothing testMessages
        baseKey @?= computeCacheKey firstEndpoint (Just ignoredFieldsRequest) testMessages
        mapM_
          (\request -> assertKeysDiffer baseKey $ computeCacheKey firstEndpoint (Just request) testMessages)
          requestsThatChangeOutput
    ]

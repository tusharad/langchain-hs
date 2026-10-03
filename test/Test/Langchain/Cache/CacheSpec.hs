{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Cache.CacheSpec (tests) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Except (runExceptT)
import Data.Aeson (object)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
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
import Test.Langchain.Provider.Mock (MockModel (..), newMockModel)

testMessages :: [Message]
testMessages = [userMessage "Describe the image"]

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
                Map.empty
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
    , testCase "cache key ignores MockModel config" $ do
        let mockModel = newMockModel "Dynamic Output"
        computeCacheKey mockModel Nothing testMessages
          @?= computeCacheKey mockModel (Just ()) testMessages
    ]

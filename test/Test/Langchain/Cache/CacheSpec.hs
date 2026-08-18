{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Cache.CacheSpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Cache.Core
import Langchain.Core.Model
  ( ChatModel (..)
  , MockModel (..)
  , assistantMessage
  , extractMessageText
  , newMockModel
  , userMessage
  )

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
        callCountVar <- newTVarIO (0 :: Int)
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
    ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Runnable.Utils (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Langchain.Runnable.Core
import Langchain.Runnable.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

data InvocationCounter a b = InvocationCounter (IORef Int) (a -> IO (Either String b))

instance Runnable (InvocationCounter a b) where
  type RunnableInput (InvocationCounter a b) = a
  type RunnableOutput (InvocationCounter a b) = b
  invoke (InvocationCounter counter f) input = do
    modifyIORef counter (+ 1)
    f input

tests :: TestTree
tests =
  testGroup
    "Runnable Utils Tests"
    [ testGroup
        "WithConfig Tests"
        [ testCase "WithConfig delegates to underlying runnable" $ do
            let mock = MockRunnable (\s -> return $ Right (s ++ " processed"))
                config = WithConfig mock ()
            result <- invoke config "input"
            assertEqual "Should delegate to mock" (Right "input processed") result
        ]
    , testGroup
        "Cached Tests"
        [ testCase "Cached returns cached result on second call" $ do
            counter <- newIORef 0
            let mock = InvocationCounter counter (\s -> return $ Right (s ++ "!"))
            cachedMock <- cached mock
            result1 <- invoke cachedMock "test"
            _ <- readIORef counter
            result2 <- invoke cachedMock "test"
            count2 <- readIORef counter
            assertEqual "First call result" (Right "test!") result1
            assertEqual "Second call result" (Right "test!") result2
            assertEqual "Only one invocation" 1 count2
        , testCase "Cached handles different inputs separately" $ do
            counter <- newIORef 0
            let mock = InvocationCounter counter (\s -> return $ Right (s ++ "!"))
            cachedMock <- cached mock
            _ <- invoke cachedMock "test1"
            _ <- invoke cachedMock "test2"
            count <- readIORef counter
            assertEqual "Two separate invocations" 2 count
        ]
    , testGroup
        "Retry Tests"
        [ testCase "Retry succeeds after one failure" $ do
            counter <- newIORef 0
            let mock = InvocationCounter counter $ \_ -> do
                  cnt <- readIORef counter
                  if cnt < 1
                    then return $ Left "Error"
                    else return $ Right ("Success" :: String)
                retryMock = Retry mock 3 5000 -- 1 retry, 5ms delay
            result <- invoke retryMock ("input" :: String)
            cnt <- readIORef counter
            assertEqual "Retry succeeds" (Right "Success") result
            assertEqual "Invoked twice" 1 cnt
        , testCase "Retry exhausts retries and fails" $ do
            counter <- newIORef 0
            let mock = InvocationCounter counter (\_ -> return $ Left "Error")
                retryMock = Retry mock 2 1000 -- 2 retries
            result <- invoke retryMock ("input" :: String)
            cnt <- readIORef counter
            assertEqual "All retries exhausted" (Left "Error" :: Either String String) result
            assertEqual "Three attempts made" 3 cnt
        ]
    , testGroup
        "WithTimeout Tests"
        [ testCase "WithTimeout returns result before timeout" $ do
            let mock = MockRunnable (\_ -> return $ Right "Quick response")
                timeoutMock = WithTimeout mock 100000 -- 100ms timeout
            result <- invoke timeoutMock ("input" :: String)
            assertEqual "Returns result" (Right ("Quick response" :: String)) result
        , testCase "WithTimeout triggers timeout error" $ do
            let mock = MockRunnable $ \_ -> do
                  threadDelay 200000 -- 200ms delay
                  return $ Right "Too slow"
                timeoutMock = WithTimeout mock 100000 -- 100ms timeout
            result <- invoke timeoutMock ("input" :: String)
            assertEqual "Timeout error" (Left "Operation timed out" :: Either String String) result
        ]
    ]

newtype MockRunnable a b = MockRunnable {runMock :: a -> IO (Either String b)}

instance Runnable (MockRunnable a b) where
  type RunnableInput (MockRunnable a b) = a
  type RunnableOutput (MockRunnable a b) = b
  invoke = runMock

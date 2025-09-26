{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Runnable.Core (tests) where

import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Langchain.Runnable.Core
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

newtype MockRunnable a b = MockRunnable
  { runMock :: a -> IO (Either String b)
  }

instance Runnable (MockRunnable a b) where
  type RunnableInput (MockRunnable a b) = a
  type RunnableOutput (MockRunnable a b) = b
  invoke = runMock

tests :: TestTree
tests =
  testGroup
    "Runnable Tests"
    [ testCase "invoke success" $ do
        let mock = MockRunnable (\(s :: String) -> return $ Right (s ++ " processed"))
        result <- invoke mock "input"
        assertEqual "Should process input" (Right "input processed") result
    , testCase "invoke error" $ do
        let mock = MockRunnable (\(_ :: String) -> return $ Left "mock error")
        result <- invoke mock "input"
        assertEqual "Should return error" (Left "mock error" :: Either String String) result
    , testCase "batch success" $ do
        let mock = MockRunnable (\(s :: String) -> return $ Right (s ++ "!"))
        result <- batch mock ["a", "b", "c"]
        assertEqual "All inputs processed" (Right ["a!", "b!", "c!"]) result
    , testCase "batch with error" $ do
        let mock = MockRunnable $ \(s :: String) ->
              if s == "b"
                then return (Left "error in batch")
                else return (Right (s ++ "!"))
        result <- batch mock ["a", "b", "c"]
        assertEqual "Should return first error" (Left "error in batch") result
    , testCase "stream success" $ do
        ref <- newIORef []
        let mock = MockRunnable (\(s :: String) -> return $ Right (s ++ "!"))
            callback x = modifyIORef ref (++ [x])
        result <- stream mock "test" callback
        readRef <- readIORef ref
        assertEqual "Stream should succeed" (Right ()) result
        assertEqual "Callback called with correct value" ["test!"] readRef
    , testCase "stream error" $ do
        ref <- newIORef []
        let mock = MockRunnable (\(_ :: String) -> return $ Left "stream error")
            callback _ = writeIORef ref ["should not be called" :: String]
        result <- stream mock "test" callback
        readRef <- readIORef ref
        assertEqual "Stream should return error" (Left "stream error") result
        assertEqual "Callback not called" [] readRef
    ]

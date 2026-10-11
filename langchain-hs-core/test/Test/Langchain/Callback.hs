{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Callback (tests) where

import Control.Concurrent.STM (readTVarIO)
import Data.Time.Clock (getCurrentTime)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Callback

tests :: TestTree
tests =
  testGroup
    "Langchain.Callback"
    [ testCase "CallbackManager registers handler and dispatches events" $ do
        mgr <- newCallbackManager
        (handler, logsVar) <- newLoggingCallbackHandler "TestHandler"
        registerHandler mgr handler

        now <- getCurrentTime
        dispatchEvent mgr (OnLLMStart "qwen2.5:7b" ["Hello"] now)
        dispatchEvent mgr (OnLLMEnd "qwen2.5:7b" "Hi there!" 1500 now)

        logged <- readTVarIO logsVar
        length logged @?= 2
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Agent.MiddlewareSpec (tests) where

import Data.Aeson (object, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.Middleware
import Langchain.Agent.ReAct (AgentStep (..))
import Langchain.Core.Model (Role (..), ToolCall (..), userMessage)

tests :: TestTree
tests =
  testGroup
    "Langchain.Agent.MiddlewareSpec"
    [ testCase "defaultMiddleware is identity for beforeStep" $ do
        let msgs = [userMessage "Hello"]
        res <- beforeStep defaultMiddleware msgs
        res @?= msgs
    , testCase "defaultMiddleware is identity for afterStep" $ do
        let step = AgentFinish (userMessage "Done")
        res <- afterStep defaultMiddleware step
        res @?= step
    , testCase "defaultMiddleware is identity for beforeToolCall and afterToolCall" $ do
        let tc = ToolCall "call_1" "function" "calc" (object [])
        resTC <- beforeToolCall defaultMiddleware tc
        resTC @?= tc
        resOut <- afterToolCall defaultMiddleware tc "result_output"
        resOut @?= "result_output"
    , testCase "chainMiddlewares applies transformations in sequential order" $ do
        let mw1 = defaultMiddleware {afterToolCall = \_ out -> pure (out <> " [m1]")}
            mw2 = defaultMiddleware {afterToolCall = \_ out -> pure (out <> " [m2]")}
            chained = chainMiddlewares [mw1, mw2]
            tc = ToolCall "call_1" "function" "calc" (object [])
        res <- afterToolCall chained tc "initial"
        res @?= "initial [m1] [m2]"
    , testCase "chainMiddlewares beforeStep modifies message list" $ do
        let mw1 = defaultMiddleware {beforeStep = \msgs -> pure (userMessage "System Header" : msgs)}
            chained = chainMiddlewares [mw1]
            msgs = [userMessage "Query"]
        res <- beforeStep chained msgs
        res @?= [userMessage "System Header", userMessage "Query"]
    , testCase "loggingMiddleware executes without exceptions" $ do
        let msgs = [userMessage "Log Test"]
            tc = ToolCall "call_2" "function" "logTool" (object [])
            step = AgentFinish (userMessage "Finished")
        _ <- beforeStep loggingMiddleware msgs
        _ <- afterStep loggingMiddleware step
        _ <- beforeToolCall loggingMiddleware tc
        resOut <- afterToolCall loggingMiddleware tc "42"
        resOut @?= "42"
    ]

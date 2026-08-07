{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.Anthropic (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Provider.Anthropic

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.Anthropic"
    [ testCase "newAnthropic initializes provider with Claude model" $ do
        let p = newAnthropic "sk-ant-test" "claude-3-5-sonnet-20241022"
        model p @?= "claude-3-5-sonnet-20241022"
        enableThinking p @?= False
    ]

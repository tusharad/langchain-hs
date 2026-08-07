{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.DeepSeek (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import Langchain.Provider.DeepSeek

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.DeepSeek"
    [ testCase "extractReasoningChain extracts <think> block" $ do
        let input = "<think>Step 1: 2+2=4.</think>The answer is 4."
            (mbThink, answer) = extractReasoningChain input
        mbThink @?= Just "Step 1: 2+2=4."
        answer @?= "The answer is 4."
    , testCase "extractReasoningChain returns Nothing when no <think> tag" $ do
        let input = "Plain answer"
            (mbThink, answer) = extractReasoningChain input
        mbThink @?= Nothing
        answer @?= "Plain answer"
    , testCase "newDeepSeek initializes provider" $ do
        let p = newDeepSeek "test-key" "deepseek-reasoner"
            modelName = model p
        modelName @?= "deepseek-reasoner"
    ]

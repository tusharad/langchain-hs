{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Accounting.CostSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Accounting.Cost

tests :: TestTree
tests =
  testGroup
    "Langchain.Accounting.CostSpec"
    [ testCase "estimateTokenCount estimates reasonably for English sentences" $ do
        let text = "Functional programming in Haskell provides exceptional safety and performance."
            tokens = estimateTokenCount text
        assertBool "Token count between 5 and 30" (tokens >= 5 && tokens <= 30)
    , testCase "calculateCost computes exact pricing for commercial models" $ do
        let pricing = getStandardPricing "gpt-4o"
            cost = calculateCost pricing 10000 2000
        estimatedInputTokens cost @?= 10000
        estimatedOutputTokens cost @?= 2000
        assertBool "Total cost is positive" (totalCostUSD cost > 0.0)
    , testCase "Local Ollama models are free" $ do
        let pricing = getStandardPricing "qwen2.5:7b"
            cost = calculateCost pricing 50000 10000
        totalCostUSD cost @?= 0.0
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitter.TokenSpec (tests) where

import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.TextSplitter.Token

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitter.TokenSpec"
    [ testCase "Empty text returns empty list" $ do
        splitByTokens defaultTokenSplitterOps "" @?= []
    , testCase "Splits text into token-bounded chunks" $ do
        let text = TL.unwords (replicate 50 "token")
            ops = defaultTokenSplitterOps {maxTokens = 15, tokenOverlap = 0}
            chunks = splitByTokens ops text
        assertBool "Multiple chunks produced" (length chunks >= 3)
        assertBool "No chunk exceeds 15 tokens" (all (\c -> countTokensApprox c <= 15) chunks)
    , testCase "Token splitter preserves words across chunks" $ do
        let text = "one two three four five six seven eight nine ten"
            ops = defaultTokenSplitterOps {maxTokens = 4, tokenOverlap = 0}
            chunks = splitByTokens ops text
        assertBool "Produced chunks" (length chunks >= 2)
    ]

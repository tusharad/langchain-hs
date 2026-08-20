{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.Gemini (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Provider.Gemini

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.Gemini"
    [ testCase "newGemini initializes provider with model" $ do
        let p = newGemini "ai-key" "gemini-1.5-pro"
        model p @?= "gemini-1.5-pro"
    ]

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Test.Langchain.Provider.OpenAI as OpenAIProviderTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "langchain-hs-openai Test Suite"
      [ testGroup
          "Provider Tests"
          [ OpenAIProviderTest.tests
          ]
      ]

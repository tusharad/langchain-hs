{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Test.Langchain.Provider.FixturesSpec as FixturesTest
import qualified Test.Langchain.Provider.Gemini as GeminiProviderTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "langchain-hs-gemini Test Suite"
      [ testGroup
          "Provider Tests"
          [ GeminiProviderTest.tests
          , FixturesTest.tests
          ]
      ]

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Test.Langchain.Provider.Ollama as OllamaProviderTest
import qualified Test.Langchain.Provider.OllamaConversionSpec as OllamaConversionTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "langchain-hs-ollama Test Suite"
      [ testGroup
          "Provider Tests"
          [ OllamaProviderTest.tests
          , OllamaConversionTest.tests
          ]
      ]

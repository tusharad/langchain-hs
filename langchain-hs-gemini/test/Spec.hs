{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Test.Gemini.Client.InteractionsSpec as InteractionsSpec
import qualified Test.Gemini.Client.TypesSpec as TypesSpec
import qualified Test.Langchain.Provider.GeminiSpec as GeminiSpec
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "langchain-hs-gemini Test Suite"
      [ TypesSpec.tests
      , InteractionsSpec.tests
      , GeminiSpec.tests
      ]

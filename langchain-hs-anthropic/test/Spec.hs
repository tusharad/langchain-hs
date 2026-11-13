module Main (main) where

import Test.Tasty

import qualified Test.Langchain.Provider.AnthropicSpec as AnthropicSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "langchain-hs-anthropic"
    [ AnthropicSpec.tests
    ]

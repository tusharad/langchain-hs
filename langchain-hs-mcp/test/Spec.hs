{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Test.Langchain.MCP.McpSpec as McpTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "langchain-hs-mcp Test Suite"
      [ testGroup
          "MCP Tests"
          [ McpTest.tests
          ]
      ]

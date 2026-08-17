{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty

import qualified Test.Cortex.BattleTestSpec as BattleTest
import qualified Test.Cortex.BrainSpec as BrainTest
import qualified Test.Cortex.CognitiveSpec as CognitiveTest
import qualified Test.Cortex.FlowSpec as FlowTest
import qualified Test.Cortex.ResearchSpec as ResearchTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "Cortex-Agent Test Suite"
      [ testGroup
          "Unit & Component Tests"
          [ BrainTest.tests
          , CognitiveTest.tests
          , ResearchTest.tests
          , FlowTest.tests
          ]
      , testGroup
          "Live Battle-Tests (Ollama: Qwen 3.5 9b & Llama 3.2)"
          [ BattleTest.tests
          ]
      ]

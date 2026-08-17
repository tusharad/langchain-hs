{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.ExampleSelector.ExampleSelectorSpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.ExampleSelector.Similarity

tests :: TestTree
tests =
  testGroup
    "Langchain.ExampleSelector.ExampleSelectorSpec"
    [ testCase "selectByLength filters examples to fit within budget" $ do
        let ex1 = Map.fromList [("input", "2+2"), ("output", "4")] -- length ~ 10
            ex2 = Map.fromList [("input", "short"), ("output", "yes")] -- length ~ 13
            ex3 = Map.fromList [("input", "a very long complex input string that exceeds budget"), ("output", "result")]
            examples = [ex1, ex2, ex3]
            selected = selectByLength 50 examples
        length selected @?= 2
        head selected @?= ex1
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitter.CodeSpec (tests) where

import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.TextSplitter.Code

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitter.CodeSpec"
    [ testCase "Splits Haskell source code on declaration boundaries" $ do
        let hsCode =
              "module MyModule where\n\ndata Person = Person { name :: String }\n\ndata Animal = Dog | Cat\n\nmyFunc :: Int -> Int\nmyFunc x = x + 1"
            ops = CodeSplitterOps Haskell 50 0
            chunks = splitCode ops hsCode
        assertBool "Multiple chunks produced" (length chunks >= 2)
    , testCase "Splits Python source code on def/class boundaries" $ do
        let pyCode =
              "class Calculator:\n    def add(self, a, b):\n        return a + b\n\ndef main():\n    calc = Calculator()\n    print(calc.add(2, 3))"
            ops = CodeSplitterOps Python 60 0
            chunks = splitCode ops pyCode
        assertBool "Produced chunks for Python" (length chunks >= 2)
    , testCase "Splits Rust code on fn and struct boundaries" $ do
        let rsCode =
              "struct Point {\n    x: f64,\n    y: f64,\n}\n\nfn calculate_distance(p1: Point, p2: Point) -> f64 {\n    0.0\n}"
            ops = CodeSplitterOps Rust 50 0
            chunks = splitCode ops rsCode
        assertBool "Produced chunks for Rust" (length chunks >= 2)
    ]

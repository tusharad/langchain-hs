{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Tool.Core (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Tool (Tool (..), toolExecute)
import Langchain.Tool.Calculator

tests :: TestTree
tests =
  testGroup
    "Tool Tests"
    [ testCalculatorTool
    ]

testCalculatorTool :: TestTree
testCalculatorTool =
  testGroup
    "Langchain.Tool.Calculator"
    [ testCase "Evaluates addition" $
        evaluateExpr "2 + 3" @?= Right 5.0
    , testCase "Evaluates multiplication" $
        evaluateExpr "3 * 4" @?= Right 12.0
    , testCase "calculatorTool computes 2 + 2" $ do
        res <- toolExecute calculatorTool (object ["expression" .= ("2 + 2" :: Text)])
        res @?= Right "4.0"
    ]

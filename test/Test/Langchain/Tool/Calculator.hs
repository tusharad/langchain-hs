{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Tool.Calculator (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Langchain.Core.Tool (toolExecute)
import Langchain.Tool.Calculator
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Tool.Calculator"
    [ testCase "evaluateExpr evaluates addition correctly" $ do
        evaluateExpr "2 + 2" @?= Right 4.0
    , testCase "evaluateExpr evaluates multiplication correctly" $ do
        evaluateExpr "3 * 4" @?= Right 12.0
    , testCase "calculatorTool returns 4.0 for '2 + 2'" $ do
        res <- toolExecute calculatorTool (object ["expression" .= ("2 + 2" :: Text)])
        res @?= Right "4.0"
    ]

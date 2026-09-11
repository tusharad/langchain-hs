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
    [ testCase "calculatorTool evaluates expression via Tool interface" $ do
        res <- toolExecute calculatorTool (object ["expression" .= ("2 + 2" :: Text)])
        res @?= Right "4.0"
    , testCase "calculatorTool handles multiplication" $ do
        res <- toolExecute calculatorTool (object ["expression" .= ("3 * 4" :: Text)])
        res @?= Right "12.0"
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.OutputParser.Core (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import Langchain.Error (LangchainError)
import Langchain.OutputParser.Core

tests :: TestTree
tests =
  testGroup
    "OutputParser Tests"
    [ testCase "Text parser should return input verbatim" $
        (parse "raw output" :: Either LangchainError Text) @?= Right "raw output"
    , testCase "Bool parser should parse affirmative representations" $ do
        (parse "true" :: Either LangchainError Bool) @?= Right True
        (parse "True" :: Either LangchainError Bool) @?= Right True
        (parse "yes" :: Either LangchainError Bool) @?= Right True
        (parse "1" :: Either LangchainError Bool) @?= Right True
    , testCase "Bool parser should parse negative representations" $ do
        (parse "false" :: Either LangchainError Bool) @?= Right False
        (parse "False" :: Either LangchainError Bool) @?= Right False
        (parse "no" :: Either LangchainError Bool) @?= Right False
        (parse "0" :: Either LangchainError Bool) @?= Right False
    , testCase "Bool parser should fail on invalid input" $
        case parse "not a boolean" :: Either LangchainError Bool of
          Left _ -> assertBool "Should be Left" True
          Right _ -> assertFailure "Should have failed parsing"
    , testCase "CommaSeparatedList parser should parse empty string" $
        parse "" @?= Right (CommaSeparatedList [""])
    , testCase "CommaSeparatedList parser should parse single item" $
        parse "item" @?= Right (CommaSeparatedList ["item"])
    , testCase "CommaSeparatedList parser should parse multiple items and trim whitespace" $
        parse " item1 , item2 , item3 " @?= Right (CommaSeparatedList ["item1", "item2", "item3"])
    , testCase "Int and Integer parser should parse valid numbers" $ do
        (parse "42" :: Either LangchainError Int) @?= Right 42
        (parse " -100 " :: Either LangchainError Int) @?= Right (-100)
        (parse "99999999999999999999" :: Either LangchainError Integer) @?= Right 99999999999999999999
    , testCase "Double parser should parse floating point numbers" $ do
        (parse "3.14159" :: Either LangchainError Double) @?= Right 3.14159
        (parse "-0.5" :: Either LangchainError Double) @?= Right (-0.5)
    , testCase "Numeric parsers should fail on non-numeric input" $ do
        case parse "not a number" :: Either LangchainError Int of
          Left _ -> assertBool "Should be Left" True
          Right _ -> assertFailure "Should have failed"
        case parse "abc" :: Either LangchainError Double of
          Left _ -> assertBool "Should be Left" True
          Right _ -> assertFailure "Should have failed"
    ]

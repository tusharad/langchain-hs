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
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.OutputParser.Core (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson
import Data.Text (Text)
import Langchain.OutputParser.Core

data Person = Person
  { name :: Text
  , age :: Int
  }
  deriving (Show, Eq)

instance FromJSON Person where
  parseJSON = withObject "Person" $ \v ->
    Person
      <$> v .: "name"
      <*> v .: "age"

tests :: TestTree
tests =
  testGroup
    "OutputParser Tests"
    [ testCase "Bool parser should parse 'true'" $
        (parse "true" :: Either String Bool) @?= Right True
    , testCase "Bool parser should parse 'True'" $
        (parse "True" :: Either String Bool) @?= Right True
    , testCase "Bool parser should parse 'TRUE'" $
        (parse "TRUE" :: Either String Bool) @?= Right True
    , testCase "Bool parser should parse 'true' with whitespace" $
        (parse "  true  " :: Either String Bool) @?= Right True
    , testCase "Bool parser should parse 'false'" $
        (parse "false" :: Either String Bool) @?= Right False
    , testCase "Bool parser should parse 'False'" $
        (parse "False" :: Either String Bool) @?= Right False
    , testCase "Bool parser should parse 'FALSE'" $
        (parse "FALSE" :: Either String Bool) @?= Right False
    , testCase "Bool parser should parse 'false' with whitespace" $
        (parse "  false  " :: Either String Bool) @?= Right False
    , testCase "Bool parser should fail on invalid input" $
        case parse "not a boolean" :: Either String Bool of
          Left _ -> assertBool "Should be Left" True
          Right _ -> assertFailure "Should have failed parsing"
    , testCase "CommaSeparatedList parser should parse empty string" $
        parse "" @?= Right (CommaSeparatedList [""])
    , testCase "CommaSeparatedList parser should parse single item" $
        parse "item" @?= Right (CommaSeparatedList ["item"])
    , testCase "CommaSeparatedList parser should parse multiple items" $
        parse "item1,item2,item3" @?= Right (CommaSeparatedList ["item1", "item2", "item3"])
    , testCase "CommaSeparatedList parser should trim whitespace" $
        parse " item1 , item2 , item3 " @?= Right (CommaSeparatedList ["item1", "item2", "item3"])
    , testCase "JSONOutputStructure parser should parse valid JSON" $
        parse "{\"name\":\"John\",\"age\":30}"
          @?= Right (JSONOutputStructure (Person "John" 30))
    , testCase "JSONOutputStructure parser should fail on invalid JSON" $
        case parse "{not valid json}" :: Either String (JSONOutputStructure Person) of
          Left _ -> assertBool "Should be Left" True
          Right _ -> assertFailure "Should have failed parsing"
    , testCase "NumberSeparatedList parser should parse numbered list" $
        parse "1. First item\n2. Second item\n3. Third item"
          @?= Right (NumberSeparatedList ["First item", "Second item", "Third item"])
    , testCase "NumberSeparatedList parser should handle whitespace" $
        parse "1.   First item  \n  2.  Second item\n3. Third item"
          @?= Right (NumberSeparatedList ["First item", "Second item", "Third item"])
    , testCase "NumberSeparatedList parser should handle multi-digit numbers" $
        parse "10. First item\n11. Second item\n12. Third item"
          @?= Right (NumberSeparatedList ["First item", "Second item", "Third item"])
    , testCase "NumberSeparatedList parser should handle text before numbered list" $
        parse "Here is a list:\n1. First item\n2. Second item"
          @?= Right (NumberSeparatedList ["First item", "Second item"])
    , testCase "NumberSeparatedList parser should handle whitespace between number and dot" $
        parse "1 . First item\n2 . Second item"
          @?= Right (NumberSeparatedList ["First item", "Second item"])
    , testCase "NumberSeparatedList parser should fail if no numbers are found" $
        case parse "No numbers here, just text" :: Either String NumberSeparatedList of
          Left _ -> assertBool "Should be Left" True
          Right _ -> assertFailure "Should have failed parsing"
    ]

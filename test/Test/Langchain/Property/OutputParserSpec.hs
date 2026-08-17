{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Property.OutputParserSpec (tests) where

import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Langchain.Core.Error (LangchainError)
import Langchain.OutputParser.Core

data PersonTest = PersonTest
  { name :: Text
  , age :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype SafeIdentifier = SafeIdentifier Text
  deriving (Show, Eq)

instance Arbitrary SafeIdentifier where
  arbitrary = SafeIdentifier . T.pack <$> listOf1 (elements ['a' .. 'z'])

tests :: TestTree
tests =
  testGroup
    "Langchain.Property.OutputParserSpec (QuickCheck)"
    [ testProperty "Bool parser parses true correctly (case-insensitive)" $
        \() ->
          parse "true" === Right True
            .&&. parse "TRUE" === Right True
            .&&. parse "The answer is True!" === Right True
    , testProperty "Bool parser parses false correctly" $
        \() ->
          parse "false" === Right False
            .&&. parse "FALSE" === Right False
            .&&. parse "The condition is False." === Right False
    , testProperty "CommaSeparatedList parses comma separated items" $
        \(SafeIdentifier item1) (SafeIdentifier item2) (SafeIdentifier item3) ->
          let textInput = item1 <> ", " <> item2 <> ", " <> item3
              expected = CommaSeparatedList [item1, item2, item3]
           in parse textInput === Right expected
    , testProperty "NumberSeparatedList parses numbered list items" $
        \(SafeIdentifier item1) (SafeIdentifier item2) ->
          let textInput = "1. " <> item1 <> "\n2. " <> item2
              expected = NumberSeparatedList [item1, item2]
           in parse textInput === Right expected
    , testProperty "JSONOutputStructure parses valid JSON data types" $
        \(SafeIdentifier pName) (Positive pAge) ->
          let person = PersonTest pName pAge
              jsonStr = T.pack $ LBSC.unpack (encode person)
           in case (parse jsonStr :: Either LangchainError (JSONOutputStructure PersonTest)) of
                Right (JSONOutputStructure parsedPerson) -> parsedPerson === person
                Left _ -> property False
    ]

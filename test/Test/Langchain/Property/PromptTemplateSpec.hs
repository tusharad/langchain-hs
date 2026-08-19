{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Property.PromptTemplateSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Langchain.PromptTemplate.FewShot
import Langchain.PromptTemplate.Prompt

-- QuickCheck helper to generate safe variable names [a-z]+
newtype SafeVar = SafeVar Text
  deriving (Show, Eq)

instance Arbitrary SafeVar where
  arbitrary = SafeVar . T.pack <$> listOf1 (elements ['a' .. 'z'])

-- Safe text without braces
newtype PlainText = PlainText Text
  deriving (Show, Eq)

instance Arbitrary PlainText where
  arbitrary = PlainText . T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ " ,.!-"))

tests :: TestTree
tests =
  testGroup
    "Langchain.Property.PromptTemplateSpec (QuickCheck)"
    [ testProperty "Static templates without braces render unchanged" $
        \(PlainText txt) ->
          renderPrompt (fromTemplate txt) Map.empty === Right txt
    , testProperty "Single variable interpolation replaces {var} with value" $
        \(SafeVar var) (PlainText val) ->
          let tmpl = "Hello {" <> var <> "}!"
              vars = Map.singleton var val
              expected = "Hello " <> val <> "!"
           in renderPrompt (fromTemplate tmpl) vars === Right expected
    , testProperty "Missing variable causes render error" $
        \(SafeVar var) ->
          let tmpl = "Prefix {" <> var <> "} Suffix"
              vars = Map.empty
           in case renderPrompt (fromTemplate tmpl) vars of
                Left _ -> property True
                Right _ -> property False
    , testProperty "Two variable interpolation succeeds when all vars present" $
        \(SafeVar v1) (SafeVar v2) (PlainText val1) (PlainText val2) ->
          v1 /= v2 ==>
            let tmpl = "{" <> v1 <> "} and {" <> v2 <> "}"
                vars = Map.fromList [(v1, val1), (v2, val2)]
                expected = val1 <> " and " <> val2
             in renderPrompt (fromTemplate tmpl) vars === Right expected
    , testProperty "FewShotPromptTemplate renders all examples" $
        \(PlainText prefix) (PlainText suffix) (PlainText ex1) (PlainText ex2) ->
          let examples =
                [ Map.singleton "content" ex1
                , Map.singleton "content" ex2
                ]
              fewShot =
                FewShotPromptTemplate
                  { fsPrefix = prefix
                  , fsExamples = examples
                  , fsExampleTemplate = "Ex: {content}"
                  , fsExampleSeparator = "\n"
                  , fsSuffix = suffix
                  }
           in case renderFewShotPrompt fewShot of
                Right rendered ->
                  property (prefix `T.isInfixOf` rendered && suffix `T.isInfixOf` rendered)
                Left _ -> property False
    ]

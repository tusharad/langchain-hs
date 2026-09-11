{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.FewShotSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.PromptTemplate.FewShot

tests :: TestTree
tests =
  testGroup
    "FewShotPromptTemplate"
    [ testCase "correctly formats a few-shot prompt" $
        let expected =
              "Examples of {type}:\nInput: Hello\nOutput: Bonjour\n\nInput: Goodbye\nOutput: Au revoir\nNow translate: {query}"
         in renderFewShotPrompt fewShotTemplate @?= Right expected
    , testCase "handles empty examples list" $
        let emptyExamples = fewShotTemplate {fsExamples = []}
         in renderFewShotPrompt emptyExamples @?= Right "Examples of {type}:\n\nNow translate: {query}"
    , testCase "handles empty prefix and suffix" $
        let noPreSuf = fewShotTemplate {fsPrefix = "", fsSuffix = ""}
         in renderFewShotPrompt noPreSuf
              @?= Right "Input: Hello\nOutput: Bonjour\n\nInput: Goodbye\nOutput: Au revoir"
    , testCase "returns an error when example variables are missing" $
        let badExamples =
              fewShotTemplate
                { fsExamples = [Map.fromList [("wrong", "value")]]
                , fsExampleTemplate = "{input} translates to {output}"
                }
         in case renderFewShotPrompt badExamples of
              Left err ->
                "input" `T.isInfixOf` T.pack (show err)
                  @? "Expected error to contain 'input'"
              Right _ ->
                assertFailure
                  "Expected an error for missing example variable"
    , testCase "correctly uses the example separator" $
        let customSep = fewShotTemplate {fsExampleSeparator = " ### "}
         in renderFewShotPrompt customSep
              @?= Right
                "Examples of {type}:\nInput: Hello\nOutput: Bonjour ### Input: Goodbye\nOutput: Au revoir\nNow translate: {query}"
    , testCase "renderFewShotPromptWithVars interpolates full template" $ do
        let inputVars = Map.fromList [("type", "Spanish"), ("query", "Thank you")]
            expected =
              "Examples of Spanish:\nInput: Hello\nOutput: Bonjour\n\nInput: Goodbye\nOutput: Au revoir\nNow translate: Thank you"
        renderFewShotPromptWithVars fewShotTemplate inputVars @?= Right expected
    ]
  where
    fewShotTemplate =
      FewShotPromptTemplate
        { fsPrefix = "Examples of {type}:\n"
        , fsExamples =
            [ Map.fromList [("input", "Hello"), ("output", "Bonjour")]
            , Map.fromList [("input", "Goodbye"), ("output", "Au revoir")]
            ]
        , fsExampleTemplate = "Input: {input}\nOutput: {output}"
        , fsExampleSeparator = "\n\n"
        , fsSuffix = "\nNow translate: {query}"
        }

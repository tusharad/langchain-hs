{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate (tests) where

import qualified Data.Map.Strict as HM
import qualified Data.Text as T
import Langchain.PromptTemplate
import Langchain.Runnable.Core (invoke)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "PromptTemplate Tests"
    [ testGroup
        "PromptTemplate"
        [ testCase "correctly interpolates all variables" $
            renderPrompt template vars @?= Right "Hello, Alice! Welcome to Wonderland."
        , testCase "handles templates with no variables" $
            let noVarTemplate = PromptTemplate "Hello, world!"
             in renderPrompt noVarTemplate HM.empty @?= Right "Hello, world!"
        , testCase "handles templates with repeated variables" $
            let repeatTemplate = PromptTemplate "{name} likes {food}. {name} eats {food} every day."
                repeatVars = HM.fromList [("name", "Bob"), ("food", "pizza")]
             in renderPrompt repeatTemplate repeatVars @?= Right "Bob likes pizza. Bob eats pizza every day."
        , testCase "returns an error for missing variables" $
            let missingVars = HM.fromList [("name", "Charlie")]
             in case renderPrompt template missingVars of
                  Left err -> "place" `T.isInfixOf` T.pack err @? "Expected error to contain 'place'"
                  Right _ -> assertFailure "Expected an error for missing variable"
                  {- TODO: Need to take care of incomplete brace cases
                  , testCase "handles unclosed braces" $
                      let invalidTemplate = PromptTemplate "Hello, {name! Welcome to {place}."
                       in case renderPrompt invalidTemplate vars of
                            Left err -> err @?= "Unclosed brace"
                            Right _ -> assertFailure "Expected an error for unclosed brace"
                  , testCase "handles complex nesting of placeholders" $
                      let complexTemplate = PromptTemplate "{{name}} is not a placeholder but {name} is."
                       in renderPrompt complexTemplate vars @?= Right "{Alice} is not a placeholder but Alice is."
                       -}
        ]
    , testCase "Runnable instance for PromptTemplate - invoke with variables" $ do
        let template1 = PromptTemplate "Hello, {name}!"
            vars1 = HM.fromList [("name", "Dave")]
        result <- invoke template1 vars1
        result @?= Right "Hello, Dave!"
    , testGroup
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
                    { fsExamples = [HM.fromList [("wrong", "value")]]
                    , fsExampleTemplate = "{input} translates to {output}"
                    }
             in case renderFewShotPrompt badExamples of
                  Left err ->
                    "input" `T.isInfixOf` T.pack err
                      @? "Expected error to contain 'input'"
                  Right _ ->
                    assertFailure
                      "Expected an error for missing example variable"
        , testCase "correctly uses the example separator" $
            let customSep = fewShotTemplate {fsExampleSeparator = " ### "}
             in renderFewShotPrompt customSep
                  @?= Right
                    "Examples of {type}:\nInput: Hello\nOutput: Bonjour ### Input: Goodbye\nOutput: Au revoir\nNow translate: {query}"
        ]
    ]
  where
    template = PromptTemplate "Hello, {name}! Welcome to {place}."
    vars = HM.fromList [("name", "Alice"), ("place", "Wonderland")]
    fewShotTemplate =
      FewShotPromptTemplate
        { fsPrefix = "Examples of {type}:\n"
        , fsExamples =
            [ HM.fromList [("input", "Hello"), ("output", "Bonjour")]
            , HM.fromList [("input", "Goodbye"), ("output", "Au revoir")]
            ]
        , fsExampleTemplate = "Input: {input}\nOutput: {output}"
        , fsExampleSeparator = "\n\n"
        , fsSuffix = "\nNow translate: {query}"
        }

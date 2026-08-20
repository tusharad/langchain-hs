{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.PromptSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.PromptTemplate.Prompt

tests :: TestTree
tests =
  testGroup
    "PromptTemplate"
    [ testCase "correctly interpolates all variables" $
        renderPrompt greetingTemplate vars @?= Right "Hello, Alice! Welcome to Wonderland."
    , testCase "handles templates with no variables" $
        let noVarTemplate = fromTemplate "Hello, world!"
         in renderPrompt noVarTemplate Map.empty @?= Right "Hello, world!"
    , testCase "handles templates with repeated variables" $
        let repeatTemplate = fromTemplate "{name} likes {food}. {name} eats {food} every day."
            repeatVars = Map.fromList [("name", "Bob"), ("food", "pizza")]
         in renderPrompt repeatTemplate repeatVars @?= Right "Bob likes pizza. Bob eats pizza every day."
    , testCase "returns an error for missing variables" $
        let missingVars = Map.fromList [("name", "Charlie")]
         in case renderPrompt greetingTemplate missingVars of
              Left err -> "place" `T.isInfixOf` T.pack (show err) @? "Expected error to contain 'place'"
              Right _ -> assertFailure "Expected an error for missing variable"
    , testCase "renders escaped f-string braces" $
        let promptTemplate = fromTemplate "Hello {{name}}, {name}!"
         in renderPrompt promptTemplate (Map.singleton "name" "Alice") @?= Right "Hello {name}, Alice!"
    , testCase "renders f-string format specs" $
        let promptTemplate = fromTemplate "Hello, {name:~u}!"
         in renderPrompt promptTemplate (Map.singleton "name" "Alice") @?= Right "Hello, ALICE!"
    , testCase "infers f-string variables without escaped braces" $
        let promptTemplate = fromTemplate "Hello {{name}}, {name}!"
         in inputVariables promptTemplate @?= ["name"]
    , testCase "rejects f-string positional fields" $
        assertRenderErrorContains "Positional arguments are not supported" $
          renderPrompt (fromTemplate "Hello, {0}!") (Map.singleton "0" "Alice")
    , testCase "rejects f-string attribute access" $
        assertRenderErrorContains "Attribute access is not supported" $
          renderPrompt (fromTemplate "Hello, {user.name}!") (Map.singleton "user.name" "Alice")
    , testCase "rejects nested f-string replacement fields" $
        assertRenderErrorContains "Nested replacement fields are not allowed" $
          renderPrompt
            (fromTemplate "Hello, {name:{width}}!")
            (Map.fromList [("name", "Alice"), ("width", "10")])
    , testCase "renders mustache sections" $
        let mustacheTemplate = fromTemplateWithFormat "{{#name}}Hello, {{name}}!{{/name}}" Mustache Map.empty
            mustacheVars = Map.singleton "name" "Alice"
         in renderPrompt mustacheTemplate mustacheVars @?= Right "Hello, Alice!"
    , testCase "returns an error for missing mustache variables" $
        let mustacheTemplate = fromTemplateWithFormat "Hello, {{name}}!" Mustache Map.empty
         in case renderPrompt mustacheTemplate Map.empty of
              Left err -> "name" `T.isInfixOf` T.pack (show err) @? "Expected error to contain 'name'"
              Right _ -> assertFailure "Expected an error for missing mustache variable"
    , testCase "renders jinja2 conditionals" $
        let jinjaTemplate =
              fromTemplateWithFormat
                "{% if enabled %}Hello, {{ name }}!{% else %}Disabled{% endif %}"
                Jinja2
                Map.empty
            jinjaVars = Map.fromList [("enabled", "true"), ("name", "Alice")]
         in renderPrompt jinjaTemplate jinjaVars @?= Right "Hello, Alice!"
    , testCase "renders jinja2 filters" $
        let jinjaTemplate = fromTemplateWithFormat "Hello, {{ name | upper }}!" Jinja2 Map.empty
            jinjaVars = Map.singleton "name" "Alice"
         in renderPrompt jinjaTemplate jinjaVars @?= Right "Hello, ALICE!"
    , testCase "infers jinja2 variables from conditionals" $
        let jinjaTemplate = fromTemplateWithFormat "{% if enabled %}Hello, {{ name }}!{% endif %}" Jinja2 Map.empty
         in inputVariables jinjaTemplate @?= ["enabled", "name"]
    , testCase "returns an error for missing jinja2 variables" $
        let jinjaTemplate = fromTemplateWithFormat "Hello, {{ name }}!" Jinja2 Map.empty
         in case renderPrompt jinjaTemplate Map.empty of
              Left err -> "name" `T.isInfixOf` T.pack (show err) @? "Expected error to contain 'name'"
              Right _ -> assertFailure "Expected an error for missing jinja2 variable"
    ]
  where
    greetingTemplate = fromTemplate "Hello, {name}! Welcome to {place}."
    vars = Map.fromList [("name", "Alice"), ("place", "Wonderland")]

assertRenderErrorContains :: (Show err) => T.Text -> Either err T.Text -> Assertion
assertRenderErrorContains expected result =
  case result of
    Left err -> expected `T.isInfixOf` T.pack (show err) @? "Expected error to contain expected text"
    Right _ -> assertFailure "Expected render error"

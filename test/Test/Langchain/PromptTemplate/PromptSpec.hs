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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      Langchain.PromptTemplate
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

This module provides types and functions for working with prompt templates in Langchain.
Prompt templates are used to structure inputs for language models, allowing for dynamic
insertion of variables into predefined text formats. They are essential for creating
flexible and reusable prompts that can be customized based on input data.

The main types are:

* 'PromptTemplate': A simple template with placeholders for variables.
* 'FewShotPromptTemplate': A template that includes few-shot examples for better context,
  useful in scenarios like few-shot learning.

These types are designed to be compatible with the Langchain Python library's prompt template
functionality: [Langchain PromptTemplate](https://python.langchain.com/docs/concepts/prompt_templates/).

== Examples

See the documentation for 'renderPrompt' and 'renderFewShotPrompt' for usage examples.
-}
module Langchain.PromptTemplate
  ( -- * Core Types
    PromptTemplate (..)
  , FewShotPromptTemplate (..)

    -- * Rendering Functions
  , renderPrompt
  , renderFewShotPrompt
  ) where

import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Runnable.Core (Runnable (..))

-- TODO: Add Mechanism for custom example selector

{- | Represents a prompt template with a template string.
The template string can contain placeholders of the form {key},
where key is a sequence of alphanumeric characters and underscores.
-}
newtype PromptTemplate = PromptTemplate
  { templateString :: Text
  }
  deriving (Show, Eq)

{- | Render a prompt template with the given variables.
Returns either an error message if a variable is missing or the rendered template.

=== Using 'renderPrompt'

To render a prompt template with variables:

@
let template = PromptTemplate "Hello, {name}! Welcome to {place}."
vars = HM.fromList [("name", "Alice"), ("place", "Wonderland")]
result <- renderPrompt template vars
-- Result: Right "Hello, Alice! Welcome to Wonderland."
@

If a variable is missing:

@
let vars = HM.fromList [("name", "Alice")]
result <- renderPrompt template vars
-- Result: Left "Missing variable: place"
@
-}
renderPrompt :: PromptTemplate -> HM.Map Text Text -> Either String Text
renderPrompt (PromptTemplate template) vars = interpolate vars template

{- | Represents a few-shot prompt template with examples.
This type allows for creating prompts that include example inputs and outputs,
which can be useful for few-shot learning scenarios.
-}
data FewShotPromptTemplate = FewShotPromptTemplate
  { fsPrefix :: Text
  -- ^ Text before the examples
  , fsExamples :: [HM.Map Text Text]
  -- ^ List of example variable maps
  , fsExampleTemplate :: Text
  -- ^ Template for formatting each example
  , fsExampleSeparator :: Text
  -- ^ Separator between formatted examples
  , fsSuffix :: Text
  -- ^ Text after the examples, with placeholders
  }
  deriving (Show, Eq)

{- | Render a few-shot prompt template with the given input variables.
Returns either an error message if interpolation fails or the fully rendered prompt.

=== Using 'renderFewShotPrompt'

To render a few-shot prompt template:

@
let fewShotTemplate = FewShotPromptTemplate
      { fsPrefix = "Examples of {type}:\n"
      , fsExamples =
          [ HM.fromList [("input", "Hello"), ("output", "Bonjour")]
          , HM.fromList [("input", "Goodbye"), ("output", "Au revoir")]
          ]
      , fsExampleTemplate = "Input: {input}\nOutput: {output}\n"
      , fsExampleSeparator = "\n"
      , fsSuffix = "Now translate: {query}"
      }
result <- renderFewShotPrompt fewShotTemplate
-- Result: Right "Examples of {type}:\nInput: Hello\nOutput: Bonjour\n\nInput: Goodbye\nOutput: Au revoir\nNow translate: {query}"
@
-}
renderFewShotPrompt :: FewShotPromptTemplate -> Either String Text
renderFewShotPrompt FewShotPromptTemplate {..} = do
  -- Format each example using the example template
  formattedExamples <-
    mapM
      (`interpolate` fsExampleTemplate)
      fsExamples
  -- Join the formatted examples with the separator
  let examplesText = T.intercalate fsExampleSeparator formattedExamples
  -- Combine prefix, examples, and suffix
  return $ fsPrefix <> examplesText <> fsSuffix

{- | Interpolate variables into a template string.
Placeholders are of the form {key}, where key is a sequence of alphanumeric characters and underscores.
-}
interpolate :: HM.Map Text Text -> Text -> Either String Text
interpolate vars = go
  where
    go :: Text -> Either String Text
    go t =
      case T.breakOn "{" t of
        (before, after) | T.null after -> Right before
        (before, after') ->
          case T.breakOn "}" (T.drop 1 after') of
            (_, after'') | T.null after'' -> Left "Unclosed brace"
            (key, after''') ->
              let key' = T.strip key
               in case HM.lookup key' vars of
                    Just val -> do
                      rest <- go (T.drop 1 after''')
                      return $ before <> val <> rest
                    Nothing -> Left $ "Missing variable: " <> T.unpack key'

instance Runnable PromptTemplate where
  type RunnableInput PromptTemplate = HM.Map Text Text
  type RunnableOutput PromptTemplate = Text

  invoke template variables = pure $ renderPrompt template variables

{-
instance Runnable FewShotPromptTemplate where
  type RunnableInput FewShotPromptTemplate = Maybe [Text]
  type RunnableOutput FewShotPromptTemplate = Text

  invoke t m = pure $ renderFewShotPrompt t m
-}

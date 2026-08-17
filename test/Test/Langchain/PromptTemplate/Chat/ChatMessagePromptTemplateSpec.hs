{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.ChatMessagePromptTemplateSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.PromptTemplate (PromptTemplate (..))
import Langchain.PromptTemplate.Chat

tests :: TestTree
tests =
  testGroup
    "ChatMessagePromptTemplate"
    [ testCase "from_template_file creates a role chat message prompt template" $ do
        actual <- chatMessagePromptTemplateFromTemplateFile "test/data/prompt_file.txt" "human"
        let expected =
              ChatMessagePromptTemplate
                { chatMessagePromptTemplatePrompt = PromptTemplate "Question: {question}\nAnswer:"
                , chatMessagePromptTemplateInputVariables = ["question"]
                , chatMessagePromptTemplateRole = "human"
                }
        actual @?= expected
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.ChatMessagePromptTemplateSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.PromptTemplate (PromptTemplate (..))
import Langchain.PromptTemplate.Chat (BaseStringMessagePromptTemplate (..))
import Langchain.PromptTemplate.Chat.ChatMessagePromptTemplate

tests :: TestTree
tests =
  testGroup
    "ChatMessagePromptTemplate"
    [ testCase "fromTemplateFile creates a role chat message prompt template" $ do
        actual <- fromTemplateFile "test/data/prompt_file.txt" "human" :: IO ChatMessagePromptTemplate
        let expected =
              ChatMessagePromptTemplate
                { chatMessagePromptTemplatePrompt = PromptTemplate "Question: {question}\nAnswer:"
                , chatMessagePromptTemplateInputVariables = ["question"]
                , chatMessagePromptTemplateRole = "human"
                }
        actual @?= expected
    ]

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.ChatMessagePromptTemplateSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.PromptTemplate.Chat (BaseStringMessagePromptTemplate (..))
import Langchain.PromptTemplate.Chat.ChatMessagePromptTemplate
import Langchain.PromptTemplate.Prompt (fromTemplate)

tests :: TestTree
tests =
  testGroup
    "ChatMessagePromptTemplate"
    [ testCase "fromTemplateFile creates a role chat message prompt template" $ do
        actual <- fromTemplateFile "test/data/prompt_file.txt" "human" :: IO ChatMessagePromptTemplate
        let expected =
              ChatMessagePromptTemplate
                { prompt =
                    fromTemplate "Question: {question}\nAnswer:"
                , inputVariables = ["question"]
                , role = "human"
                }
        actual @?= expected
    ]

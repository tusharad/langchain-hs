{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.OpenAI (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Provider.OpenAI

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.OpenAI"
    [ testCase "newOpenAI initializes default provider" $ do
        let p = newOpenAI "sk-test" "gpt-4o"
        model p @?= "gpt-4o"
        baseUrl p @?= "https://api.openai.com/v1/chat/completions"
    , testCase "openAICompatible initializes custom endpoint" $ do
        let p = openAICompatible "sk-test" "custom-llm" "https://custom-ai.example.com/v1/chat/completions"
        model p @?= "custom-llm"
        baseUrl p @?= "https://custom-ai.example.com/v1/chat/completions"
    ]

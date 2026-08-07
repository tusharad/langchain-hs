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
        let p = openAICompatible "sk-test" "deepseek-chat" "https://api.deepseek.com/v1/chat/completions"
        model p @?= "deepseek-chat"
        baseUrl p @?= "https://api.deepseek.com/v1/chat/completions"
    ]

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
        baseUrl p @?= "https://api.openai.com"
    , testCase "openAICompatible initializes custom endpoint" $ do
        let p = openAICompatible "sk-test" "custom-llm" "https://custom-ai.example.com"
        model p @?= "custom-llm"
        baseUrl p @?= "https://custom-ai.example.com"
    , testCase "normalizeBaseUrl strips endpoint paths for servant compatibility" $ do
        normalizeBaseUrl "https://api.openai.com" @?= "https://api.openai.com"
        normalizeBaseUrl "https://api.openai.com/" @?= "https://api.openai.com"
        normalizeBaseUrl "https://api.openai.com/v1" @?= "https://api.openai.com"
        normalizeBaseUrl "https://api.openai.com/v1/" @?= "https://api.openai.com"
        normalizeBaseUrl "https://api.openai.com/v1/chat/completions" @?= "https://api.openai.com"
        normalizeBaseUrl "https://openrouter.ai/api" @?= "https://openrouter.ai/api"
        normalizeBaseUrl "https://openrouter.ai/api/v1" @?= "https://openrouter.ai/api"
        normalizeBaseUrl "https://openrouter.ai/api/v1/chat/completions" @?= "https://openrouter.ai/api"
        normalizeBaseUrl "http://localhost:11434/v1" @?= "http://localhost:11434"
    ]

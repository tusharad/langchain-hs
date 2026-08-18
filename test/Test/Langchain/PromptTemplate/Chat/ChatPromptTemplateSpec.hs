{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.ChatPromptTemplateSpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.PromptTemplate (PromptTemplate (..), PromptTemplateOptions (..))
import Langchain.PromptTemplate.Chat.ChatPromptTemplate (ChatPromptTemplate (..))
import qualified Langchain.PromptTemplate.Chat.ChatPromptTemplate as ChatPromptTemplate
import Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate (HumanMessagePromptTemplate (..))
import qualified Langchain.PromptTemplate.Chat.HumanMessagePromptTemplate as HumanMessagePromptTemplate

tests :: TestTree
tests =
  testGroup
    "ChatPromptTemplate"
    [ testCase "fromTemplate creates a chat prompt template" $ do
        let actual = ChatPromptTemplate.fromTemplate "hi {foo} {bar}"
            expected =
              ChatPromptTemplate
                { messages =
                    [ HumanMessagePromptTemplate.fromTemplate
                        "hi {foo} {bar}"
                    ]
                , inputVariables = ["foo", "bar"]
                }
        actual @?= expected
    , testCase "fromTemplate creates a chat prompt template with partials" $ do
        let actual =
              ChatPromptTemplate.fromTemplateWithOptions
                "hi {foo} {bar}"
                (PromptTemplateOptions (Map.singleton "foo" "jim"))
            expectedPrompt =
              PromptTemplate
                { template = "hi {foo} {bar}"
                , inputVariables = ["bar"]
                , partialVariables = Map.singleton "foo" "jim"
                }
        case messages actual of
          [outputPrompt] ->
            outputPrompt
              @?= HumanMessagePromptTemplate
                { prompt = expectedPrompt
                }
          actualMessages -> assertFailure $ "Expected one message, got " <> show (length actualMessages)
    ]

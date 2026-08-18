{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.ChatPromptTemplateSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model.Types (Role (..), extractMessageText, userMessage)
import Langchain.PromptTemplate (PromptTemplate (..), PromptTemplateOptions (..))
import qualified Langchain.PromptTemplate as PromptTemplate
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
                    [ ChatPromptTemplate.HumanMessagePrompt $
                        HumanMessagePromptTemplate.fromTemplate "hi {foo} {bar}"
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
          [ChatPromptTemplate.HumanMessagePrompt outputPrompt] ->
            outputPrompt
              @?= HumanMessagePromptTemplate
                { prompt = expectedPrompt
                }
          actualMessages -> assertFailure $ "Expected one message, got " <> show (length actualMessages)
    , testCase "formatPrompt formats all chat prompt messages" $ do
        let actual = ChatPromptTemplate.formatPrompt chatPromptTemplate promptVariables
        case actual of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue -> do
            let promptMessages = ChatPromptTemplate.toMessages promptValue
            length promptMessages @?= 4
            map extractMessageText promptMessages
              @?= [ "Here's some context: context"
                  , "Hello foo, I'm bar. Thanks for the context"
                  , "I'm an AI. I'm foo. I'm bar."
                  , "I'm a generic message. I'm foo. I'm bar."
                  ]
            ChatPromptTemplate.toString promptValue
              @?= T.intercalate
                "\n"
                [ "System: Here's some context: context"
                , "Human: Hello foo, I'm bar. Thanks for the context"
                , "AI: I'm an AI. I'm foo. I'm bar."
                , "Human: I'm a generic message. I'm foo. I'm bar."
                ]
        ChatPromptTemplate.format chatPromptTemplate promptVariables
          @?= Right
            ( T.intercalate
                "\n"
                [ "System: Here's some context: context"
                , "Human: Hello foo, I'm bar. Thanks for the context"
                , "AI: I'm an AI. I'm foo. I'm bar."
                , "Human: I'm a generic message. I'm foo. I'm bar."
                ]
            )
    , testCase "formatPrompt preserves static messages" $ do
        let actual =
              ChatPromptTemplate.fromMessages $
                chatPromptMessages <> [ChatPromptTemplate.StaticMessage (userMessage "foo")]
        case actual of
          ChatPromptTemplate {inputVariables = actualInputVariables} ->
            actualInputVariables @?= ["context", "foo", "bar"]
        length (messages actual) @?= 5
        case ChatPromptTemplate.formatPrompt actual withMessagesVariables of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            last (ChatPromptTemplate.toMessages promptValue) @?= userMessage "foo"
    ]
  where
    promptVariables = Map.fromList [("foo", "foo"), ("bar", "bar"), ("context", "context")]

    withMessagesVariables =
      Map.fromList [("context", "see"), ("foo", "this"), ("bar", "magic")]

    chatPromptTemplate =
      ChatPromptTemplate
        { messages = chatPromptMessages
        , inputVariables = ["foo", "bar", "context"]
        }

    chatPromptMessages =
      [ ChatPromptTemplate.SystemMessagePrompt $
          PromptTemplate.fromTemplate "Here's some context: {context}"
      , ChatPromptTemplate.HumanMessagePrompt $
          HumanMessagePromptTemplate
            { prompt =
                PromptTemplate
                  { template = "Hello {foo}, I'm {bar}. Thanks for the {context}"
                  , inputVariables = ["foo", "bar", "context"]
                  , partialVariables = Map.empty
                  }
            }
      , ChatPromptTemplate.AIMessagePrompt $
          PromptTemplate
            { template = "I'm an AI. I'm {foo}. I'm {bar}."
            , inputVariables = ["foo", "bar"]
            , partialVariables = Map.empty
            }
      , ChatPromptTemplate.ChatMessagePrompt User $
          PromptTemplate
            { template = "I'm a generic message. I'm {foo}. I'm {bar}."
            , inputVariables = ["foo", "bar"]
            , partialVariables = Map.empty
            }
      ]

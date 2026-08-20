{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.MessagesPlaceholderSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (errorMessage)
import Langchain.Core.Model.Types (Message, assistantMessage, systemMessage, userMessage)
import Langchain.PromptTemplate.Chat (BaseMessagePromptTemplate (..))
import Langchain.PromptTemplate.Chat.MessagesPlaceholder
  ( MessagesPlaceholder
  , MessagesPlaceholderOptions (..)
  , messagesPlaceholder
  , messagesPlaceholderOptions
  , messagesPlaceholderWithOptions
  )

tests :: TestTree
tests =
  testGroup
    "MessagesPlaceholder"
    [ testCase "required placeholder requires its variable" $ do
        let result = formatMessages (messagesPlaceholder "history") emptyInputs
        case result of
          Left err ->
            "history" `T.isInfixOf` errorMessage err
              @? "Expected error to mention missing history"
          Right _ -> assertFailure "Expected missing history to fail"
    , testCase "optional placeholder formats to an empty list when omitted" $
        formatMessages optionalPlaceholder emptyInputs
          @?= Right []
    , testCase "optional placeholder accepts messages" $
        formatMessages
          optionalPlaceholder
          ( inputs
              [ systemMessage "You are an AI assistant."
              , userMessage "Hello!"
              ]
          )
          @?= Right
            [ systemMessage "You are an AI assistant."
            , userMessage "Hello!"
            ]
    , testCase "placeholder without a message limit returns the whole history" $
        let history = map assistantMessage ["1", "2", "3"]
         in formatMessages
              (messagesPlaceholder "history")
              (inputs history)
              @?= Right history
    , testCase "placeholder with n_messages returns the last messages" $
        let history = map assistantMessage ["1", "2", "3"]
            prompt =
              messagesPlaceholderWithOptions $
                (messagesPlaceholderOptions "history") {nMessages = Just 2}
         in formatMessages
              prompt
              (inputs history)
              @?= Right [assistantMessage "2", assistantMessage "3"]
    , testCase "placeholder rejects non-positive n_messages" $
        let history = map assistantMessage ["1", "2", "3"]
            prompt =
              messagesPlaceholderWithOptions $
                (messagesPlaceholderOptions "history") {nMessages = Just 0}
         in case formatMessages prompt (inputs history) of
              Left err ->
                "n_messages" `T.isInfixOf` errorMessage err
                  @? "Expected error to mention n_messages"
              Right _ -> assertFailure "Expected non-positive n_messages to fail"
    ]

optionalPlaceholder :: MessagesPlaceholder
optionalPlaceholder =
  messagesPlaceholderWithOptions $
    (messagesPlaceholderOptions "history") {optional = True}

emptyInputs :: Map.Map T.Text [Message]
emptyInputs = Map.empty

inputs :: [Message] -> Map.Map T.Text [Message]
inputs history = Map.fromList [("history", history)]

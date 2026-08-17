{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.MessagesPlaceholderSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (errorMessage)
import Langchain.Core.Model.Types (Role (..), assistantMessage, systemMessage, userMessage)
import Langchain.PromptTemplate.Chat (BaseMessagePromptTemplate (..))
import Langchain.PromptTemplate.Chat.MessagesPlaceholder

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
        formatMessages (optionalMessagesPlaceholder "history") emptyInputs
          @?= Right []
    , testCase "optional placeholder converts mixed message representations" $
        formatMessages
          (optionalMessagesPlaceholder "history")
          ( inputs
              [ PlaceholderRoleMessage System "You are an AI assistant."
              , PlaceholderHumanText "Hello!"
              ]
          )
          @?= Right
            [ systemMessage "You are an AI assistant."
            , userMessage "Hello!"
            ]
    , testCase "placeholder without a message limit returns the whole history" $
        let history = map (PlaceholderMessage . assistantMessage) ["1", "2", "3"]
         in formatMessages
              (messagesPlaceholder "history")
              (inputs history)
              @?= Right (map assistantMessage ["1", "2", "3"])
    , testCase "placeholder with n_messages returns the last messages" $
        let history = map (PlaceholderMessage . assistantMessage) ["1", "2", "3"]
         in case messagesPlaceholderWithLimit "history" 2 of
              Left err ->
                assertFailure $
                  "Expected valid n_messages, got: " <> T.unpack (errorMessage err)
              Right prompt ->
                formatMessages
                  prompt
                  (inputs history)
                  @?= Right [assistantMessage "2", assistantMessage "3"]
    ]

emptyInputs :: Map.Map T.Text [MessagePlaceholderInput]
emptyInputs = Map.empty

inputs :: [MessagePlaceholderInput] -> Map.Map T.Text [MessagePlaceholderInput]
inputs history = Map.fromList [("history", history)]

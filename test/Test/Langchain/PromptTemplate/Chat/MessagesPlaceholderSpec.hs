{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.MessagesPlaceholderSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (errorMessage)
import Langchain.Core.Model.Types (Role (..), assistantMessage, systemMessage, userMessage)
import Langchain.PromptTemplate.Chat

tests :: TestTree
tests =
  testGroup
    "MessagesPlaceholder"
    [ testCase "required placeholder requires its variable" $ do
        let result = formatMessagesPlaceholder (messagesPlaceholder "history") Map.empty
        case result of
          Left err ->
            "history" `T.isInfixOf` errorMessage err
              @? "Expected error to mention missing history"
          Right _ -> assertFailure "Expected missing history to fail"
    , testCase "optional placeholder formats to an empty list when omitted" $
        formatMessagesPlaceholder (optionalMessagesPlaceholder "history") Map.empty
          @?= Right []
    , testCase "optional placeholder converts mixed message representations" $
        formatMessagesPlaceholder
          (optionalMessagesPlaceholder "history")
          ( Map.fromList
              [ ( "history"
                , [ PlaceholderRoleMessage System "You are an AI assistant."
                  , PlaceholderHumanText "Hello!"
                  ]
                )
              ]
          )
          @?= Right
            [ systemMessage "You are an AI assistant."
            , userMessage "Hello!"
            ]
    , testCase "placeholder without a message limit returns the whole history" $
        let history = map (PlaceholderMessage . assistantMessage) ["1", "2", "3"]
         in formatMessagesPlaceholder
              (messagesPlaceholder "history")
              (Map.fromList [("history", history)])
              @?= Right (map assistantMessage ["1", "2", "3"])
    , testCase "placeholder with n_messages returns the last messages" $
        let history = map (PlaceholderMessage . assistantMessage) ["1", "2", "3"]
         in case messagesPlaceholderWithLimit "history" 2 of
              Left err ->
                assertFailure $
                  "Expected valid n_messages, got: " <> T.unpack (errorMessage err)
              Right prompt ->
                formatMessagesPlaceholder
                  prompt
                  (Map.fromList [("history", history)])
                  @?= Right [assistantMessage "2", assistantMessage "3"]
    ]

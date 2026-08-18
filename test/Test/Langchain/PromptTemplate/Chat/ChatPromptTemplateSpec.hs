{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.ChatPromptTemplateSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model.Types (Role (..), extractMessageText, textMessage, userMessage)
import Langchain.PromptTemplate (PromptTemplateOptions (..))
import Langchain.PromptTemplate.Chat.ChatPromptTemplate
  ( ChatPromptInput (..)
  , ChatPromptMessage
  , ChatPromptTemplate (..)
  , append
  , extend
  , format
  , formatPrompt
  , fromMessages
  , fromTemplate
  , fromTemplateWithOptions
  , invoke
  , message
  , messagesPlaceholder
  , partial
  , templateMessage
  , toMessages
  , toString
  )

tests :: TestTree
tests =
  testGroup
    "ChatPromptTemplate"
    [ fromTemplateTests
    , fromMessagesTests
    , formatPromptTests
    , partialTests
    , appendExtendTests
    , invokeTests
    ]

fromTemplateTests :: TestTree
fromTemplateTests =
  testGroup
    "fromTemplate"
    [ testCase "creates a chat prompt template" $ do
        let actual = fromTemplate "hi {foo} {bar}"
            expected =
              ChatPromptTemplate
                { messages =
                    [templateMessage User "hi {foo} {bar}"]
                , inputVariables = ["foo", "bar"]
                }
        actual @?= expected
    , testCase "creates a chat prompt template with partials" $ do
        let actual =
              fromTemplateWithOptions
                "hi {foo} {bar}"
                (PromptTemplateOptions (Map.singleton "foo" "jim"))
        inputVariables actual @?= ["bar"]
        case formatPrompt actual (Map.singleton "bar" "bob") of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue -> toMessages promptValue @?= [userMessage "hi jim bob"]
    ]

fromMessagesTests :: TestTree
fromMessagesTests =
  testGroup
    "fromMessages"
    [ testCase "preserves static messages" $ do
        let actual =
              fromMessages $
                chatPromptMessages <> [message (userMessage "foo")]
        case actual of
          ChatPromptTemplate {inputVariables = actualInputVariables} ->
            actualInputVariables @?= ["context", "foo", "bar"]
        length (messages actual) @?= 5
        case formatPrompt actual withMessagesVariables of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            last (toMessages promptValue) @?= userMessage "foo"
    ]

formatPromptTests :: TestTree
formatPromptTests =
  testGroup
    "formatPrompt / format"
    [ testCase "formats all chat prompt messages" $ do
        let actual = formatPrompt chatPromptTemplate promptVariables
        case actual of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue -> do
            let promptMessages = toMessages promptValue
            length promptMessages @?= 4
            map extractMessageText promptMessages
              @?= [ "Here's some context: context"
                  , "Hello foo, I'm bar. Thanks for the context"
                  , "I'm an AI. I'm foo. I'm bar."
                  , "I'm a generic message. I'm foo. I'm bar."
                  ]
            toString promptValue @?= expectedFormattedPrompt
        format chatPromptTemplate promptVariables @?= Right expectedFormattedPrompt
    ]

partialTests :: TestTree
partialTests =
  testGroup
    "partial"
    [ testCase "formats chat messages with stored variables" $ do
        let template1 =
              fromMessages
                [ templateMessage System "You are an AI assistant named {name}."
                , templateMessage User "Hi I'm {user}"
                , templateMessage Assistant "Hi there, {user}, I'm {name}."
                , templateMessage User "{input}"
                ]
            template2 =
              partial
                template1
                (Map.fromList [("user", "Lucy"), ("name", "R2D2")])
            variables = Map.singleton "input" "hello"
            expected =
              [ textMessage System "You are an AI assistant named R2D2."
              , userMessage "Hi I'm Lucy"
              , textMessage Assistant "Hi there, Lucy, I'm R2D2."
              , userMessage "hello"
              ]
            expectedString =
              T.intercalate
                "\n"
                [ "System: You are an AI assistant named R2D2."
                , "Human: Hi I'm Lucy"
                , "AI: Hi there, Lucy, I'm R2D2."
                , "Human: hello"
                ]

        case formatPrompt template1 variables of
          Left _ -> pure ()
          Right promptValue ->
            assertFailure $ "Expected missing variable error, got " <> show promptValue

        case formatPrompt template2 variables of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue -> toMessages promptValue @?= expected
        format template2 variables @?= Right expectedString
    ]

appendExtendTests :: TestTree
appendExtendTests =
  testGroup
    "append / extend"
    [ testCase "appends template messages" $ do
        let template =
              fromMessages
                [templateMessage System "You are helpful."]
            template' = append template (templateMessage User "{question}")

        case formatPrompt template' (Map.singleton "question" "What is AI?") of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are helpful."
                  , userMessage "What is AI?"
                  ]
    , testCase "appends and extends messages" $ do
        let message1 = textMessage System "foo"
            message2 = userMessage "bar"
            message3 = userMessage "baz"
            baseTemplate = fromMessages [message message1]
            template' = append (append baseTemplate (message message2)) (message message3)
            template'' = extend template' [message message2, message message3]
            template''' = append template'' (templateMessage System "hello!")

        length (messages template') @?= 3
        length (messages template'') @?= 5
        messages template''
          @?= [ message message1
              , message message2
              , message message3
              , message message2
              , message message3
              ]
        case formatPrompt template''' Map.empty of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            last (toMessages promptValue) @?= textMessage System "hello!"
    ]

invokeTests :: TestTree
invokeTests =
  testGroup
    "invoke"
    [ testCase "formats chat prompt template messages" $ do
        let invokeTemplate =
              fromMessages
                [ templateMessage System "You are {name}."
                , templateMessage User "{question}"
                ]
            variables = ChatPromptVariables $ Map.fromList [("name", "Alice"), ("question", "Hello?")]

        case invoke invokeTemplate variables of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are Alice."
                  , userMessage "Hello?"
                  ]
    , testCase "accepts message list input for a single messages placeholder" $ do
        let placeholderTemplate =
              fromMessages
                [messagesPlaceholder "history"]
            input = ChatPromptMessageList [userMessage "Hi there"]

        case invoke placeholderTemplate input of
          Left err -> assertFailure $ "Expected placeholder prompt value, got " <> show err
          Right promptValue -> toMessages promptValue @?= [userMessage "Hi there"]
    , testCase "rejects list input for mixed templates" $ do
        let mixedPrompt =
              fromMessages
                [ templateMessage System "You are a {foo}"
                , messagesPlaceholder "history"
                ]
            listInput = ChatPromptMessageList [userMessage "Hi there"]
        case invoke mixedPrompt listInput of
          Left _ -> pure ()
          Right promptValue ->
            assertFailure $ "Expected list input validation error, got " <> show promptValue
    ]

promptVariables :: Map.Map Text Text
promptVariables = Map.fromList [("foo", "foo"), ("bar", "bar"), ("context", "context")]

withMessagesVariables :: Map.Map Text Text
withMessagesVariables =
  Map.fromList [("context", "see"), ("foo", "this"), ("bar", "magic")]

chatPromptTemplate :: ChatPromptTemplate
chatPromptTemplate =
  ChatPromptTemplate
    { messages = chatPromptMessages
    , inputVariables = ["foo", "bar", "context"]
    }

chatPromptMessages :: [ChatPromptMessage]
chatPromptMessages =
  [ templateMessage System "Here's some context: {context}"
  , templateMessage User "Hello {foo}, I'm {bar}. Thanks for the {context}"
  , templateMessage Assistant "I'm an AI. I'm {foo}. I'm {bar}."
  , templateMessage User "I'm a generic message. I'm {foo}. I'm {bar}."
  ]

expectedFormattedPrompt :: Text
expectedFormattedPrompt =
  T.intercalate
    "\n"
    [ "System: Here's some context: context"
    , "Human: Hello foo, I'm bar. Thanks for the context"
    , "AI: I'm an AI. I'm foo. I'm bar."
    , "Human: I'm a generic message. I'm foo. I'm bar."
    ]

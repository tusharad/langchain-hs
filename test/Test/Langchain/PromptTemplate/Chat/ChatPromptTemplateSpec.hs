{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PromptTemplate.Chat.ChatPromptTemplateSpec (tests) where

import Data.Aeson (decode, encode, object, (.=))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model.Types
  ( ContentBlock (..)
  , ImageContent (..)
  , ImageSource (..)
  , Message (..)
  , Role (..)
  , extractMessageText
  , textMessage
  , userMessage
  )
import Langchain.PromptTemplate.Chat.ChatPromptTemplate
  ( ChatPromptInput (..)
  , ChatPromptMessage
  , ChatPromptTemplate (..)
  , ContentPromptBlock (..)
  , PartialValue (..)
  , append
  , contentMessage
  , extend
  , format
  , formatPrompt
  , fromMessages
  , fromTemplate
  , fromTemplateWithOptions
  , invoke
  , message
  , messagesPlaceholder
  , messagesPlaceholderWithOptions
  , partial
  , templateMessage
  , templateMessageWithFormat
  , toMessages
  , toString
  )
import Langchain.PromptTemplate.Chat.MessagesPlaceholder
  ( MessagesPlaceholder (..)
  , MessagesPlaceholderOptions (..)
  )
import Langchain.PromptTemplate.Prompt (PromptTemplateOptions (..))
import Langchain.PromptTemplate.String (TemplateFormat (..))

tests :: TestTree
tests =
  testGroup
    "ChatPromptTemplate"
    [ fromTemplateTests
    , fromMessagesTests
    , richContentTests
    , formatPromptTests
    , missingVariableTests
    , partialTests
    , appendExtendTests
    , invokeTests
    , serializationTests
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
    , testCase "formats mustache role messages" $ do
        let template =
              fromMessages
                [ templateMessageWithFormat System Mustache "You are a helpful AI bot. Your name is {{name}}."
                , templateMessageWithFormat User Mustache "Hello, how are you doing?"
                , templateMessageWithFormat Assistant Mustache "I'm doing well, thanks!"
                , templateMessageWithFormat User Mustache "{{user_input}}"
                ]
            variables = Map.fromList [("name", "Bob"), ("user_input", "What is your name?")]

        case formatPrompt template variables of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are a helpful AI bot. Your name is Bob."
                  , userMessage "Hello, how are you doing?"
                  , textMessage Assistant "I'm doing well, thanks!"
                  , userMessage "What is your name?"
                  ]
    , testCase "formats jinja2 role messages" $ do
        let template =
              fromMessages
                [ templateMessageWithFormat System Jinja2 "You are a helpful AI bot. Your name is {{ name }}."
                , templateMessageWithFormat User Jinja2 "Hello, how are you doing?"
                , templateMessageWithFormat Assistant Jinja2 "I'm doing well, thanks!"
                , templateMessageWithFormat User Jinja2 "{{ user_input }}"
                ]
            variables = Map.fromList [("name", "Bob"), ("user_input", "What is your name?")]

        case formatPrompt template variables of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are a helpful AI bot. Your name is Bob."
                  , userMessage "Hello, how are you doing?"
                  , textMessage Assistant "I'm doing well, thanks!"
                  , userMessage "What is your name?"
                  ]
    , testCase "formats mustache typed role messages" $ do
        let template =
              fromMessages
                [ templateMessageWithFormat System Mustache "You are {{name}}."
                , templateMessageWithFormat User Mustache "{{question}}"
                ]
            variables = Map.fromList [("name", "Bob"), ("question", "Hello?")]

        case formatPrompt template variables of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are Bob."
                  , userMessage "Hello?"
                  ]
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

missingVariableTests :: TestTree
missingVariableTests =
  testGroup
    "missing variables"
    [ testCase "fails for missing FString variables in chat messages" $ do
        let template = fromMessages [templateMessage User "Hi {foo}"]
        assertMissingVariable "Parameter not found: foo" (formatPrompt template Map.empty)
    , testCase "fails for missing Mustache variables in chat messages" $ do
        let template = fromMessages [templateMessageWithFormat User Mustache "Hi {{foo}}"]
        assertMissingVariable "Missing variable: foo" (formatPrompt template Map.empty)
    , testCase "fails for missing Jinja2 variables in chat messages" $ do
        let template = fromMessages [templateMessageWithFormat User Jinja2 "Hi {{ foo }}"]
        assertMissingVariable "Missing variable: foo" (formatPrompt template Map.empty)
    , testCase "fails for missing FString variables in multipart text blocks" $ do
        let template = fromMessages [contentMessage User [TextPromptBlock FString "Hi {foo}"]]
        assertMissingVariable "Parameter not found: foo" (formatPrompt template Map.empty)
    , testCase "fails for missing FString variables in image url blocks" $ do
        let template =
              fromMessages
                [ contentMessage
                    User
                    [ ImagePromptBlock FString $
                        ImageContent (ImageUrl "https://example.com/{foo}") Nothing Nothing
                    ]
                ]
        assertMissingVariable "Parameter not found: foo" (formatPrompt template Map.empty)
    , testCase "fails for missing FString variables in image detail blocks" $ do
        let template =
              fromMessages
                [ contentMessage
                    User
                    [ ImagePromptBlock FString $
                        ImageContent (ImageUrl "https://example.com/image.png") (Just "{foo}") Nothing
                    ]
                ]
        assertMissingVariable "Parameter not found: foo" (formatPrompt template Map.empty)
    , testCase "fails for missing FString variables in image metadata blocks" $ do
        let template =
              fromMessages
                [ contentMessage
                    User
                    [ ImagePromptBlock FString $
                        ImageContent
                          (ImageUrl "https://example.com/image.png")
                          Nothing
                          (Just $ object ["cache_control" .= object ["type" .= ("{foo}" :: Text)]])
                    ]
                ]
        assertMissingVariable "Parameter not found: foo" (formatPrompt template Map.empty)
    ]

richContentTests :: TestTree
richContentTests =
  testGroup
    "rich content"
    [ testCase "formats multipart text blocks" $ do
        let template =
              fromMessages
                [ templateMessage System "You are an AI assistant named {name}."
                , contentMessage
                    User
                    [TextPromptBlock FString "What's in this image?", TextPromptBlock FString "Oh nvm"]
                ]

        case formatPrompt template (Map.singleton "name" "R2D2") of
          Left err -> assertFailure $ "Expected multipart text prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are an AI assistant named R2D2."
                  , Message User (TextBlock "What's in this image?" :| [TextBlock "Oh nvm"]) Nothing Nothing Nothing
                  ]
    , testCase "formats templated multipart text blocks" $ do
        let template =
              fromMessages
                [ templateMessage System "You are an AI assistant named {name}."
                , contentMessage
                    User
                    [TextPromptBlock FString "What's in this {object_name}?", TextPromptBlock FString "Oh nvm"]
                ]
            variables = Map.fromList [("name", "R2D2"), ("object_name", "image")]

        case formatPrompt template variables of
          Left err -> assertFailure $ "Expected templated multipart text prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are an AI assistant named R2D2."
                  , Message User (TextBlock "What's in this image?" :| [TextBlock "Oh nvm"]) Nothing Nothing Nothing
                  ]
    , testCase "formats system template with partial variables" $ do
        let graphCreatorContent = "\n    Your instructions are:\n    {instructions}\n    History:\n    {history}\n    "
            template =
              partial
                (fromMessages [templateMessage System graphCreatorContent])
                (Map.singleton "instructions" (PartialText "{}"))

        case formatPrompt template (Map.singleton "history" "history") of
          Left err -> assertFailure $ "Expected system partial prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage
                      System
                      "\n    Your instructions are:\n    {}\n    History:\n    history\n    "
                  ]
    , testCase "formats system multipart text template" $ do
        let graphCreatorContent1 = "\n    This is the prompt for the first test:\n    {variables}\n    "
            graphCreatorContent2 = "\n    This is the prompt for the second test:\n        {variables}\n        "
            template =
              fromMessages
                [ contentMessage
                    System
                    [ TextPromptBlock FString graphCreatorContent1
                    , TextPromptBlock FString graphCreatorContent2
                    ]
                ]

        case formatPrompt template (Map.singleton "variables" "foo") of
          Left err -> assertFailure $ "Expected system multipart text prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ Message
                      System
                      ( TextBlock "\n    This is the prompt for the first test:\n    foo\n    "
                          :| [TextBlock "\n    This is the prompt for the second test:\n        foo\n        "]
                      )
                      Nothing
                      Nothing
                      Nothing
                  ]
    , testCase "formats image_url blocks" $ do
        let base64Image = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAA"
            otherBase64Image = "other_iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAA"
            template =
              fromMessages
                [ templateMessage System "You are an AI assistant named {name}."
                , contentMessage
                    User
                    [ TextPromptBlock FString "What's in this image?"
                    , ImagePromptBlock FString $
                        ImageContent (ImageUrl "data:image/jpeg;base64,{my_image}") Nothing Nothing
                    , ImagePromptBlock FString $ ImageContent (ImageUrl "{my_other_image}") Nothing Nothing
                    , ImagePromptBlock FString $ ImageContent (ImageUrl "{my_other_image}") (Just "medium") Nothing
                    , ImagePromptBlock FString $
                        ImageContent (ImageUrl "https://www.langchain.com/image.png") Nothing Nothing
                    ]
                ]
            variables = Map.fromList [("name", "R2D2"), ("my_image", base64Image), ("my_other_image", otherBase64Image)]

        case formatPrompt template variables of
          Left err -> assertFailure $ "Expected image_url prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are an AI assistant named R2D2."
                  , Message
                      User
                      ( TextBlock "What's in this image?"
                          :| [ ImageBlock $ ImageContent (ImageUrl ("data:image/jpeg;base64," <> base64Image)) Nothing Nothing
                             , ImageBlock $ ImageContent (ImageUrl otherBase64Image) Nothing Nothing
                             , ImageBlock $ ImageContent (ImageUrl otherBase64Image) (Just "medium") Nothing
                             , ImageBlock $ ImageContent (ImageUrl "https://www.langchain.com/image.png") Nothing Nothing
                             ]
                      )
                      Nothing
                      Nothing
                      Nothing
                  ]
    , testCase "formats image_url blocks with detail" $ do
        let templateWith templateFormat urlTemplate =
              fromMessages
                [ contentMessage
                    User
                    [ ImagePromptBlock templateFormat $
                        ImageContent urlTemplate (Just "low") Nothing
                    ]
                ]
            expected =
              [ Message
                  User
                  ( ImageBlock
                      (ImageContent (ImageUrl "data:image/png;base64, base64data") (Just "low") Nothing)
                      :| []
                  )
                  Nothing
                  Nothing
                  Nothing
              ]
            assertFormats template variables =
              case formatPrompt template variables of
                Left err -> assertFailure $ "Expected image_url detail prompt, got " <> show err
                Right promptValue -> toMessages promptValue @?= expected

        assertFormats
          (templateWith FString (ImageUrl "data:{image_type};base64, {image_data}"))
          (Map.fromList [("image_type", "image/png"), ("image_data", "base64data")])
        assertFormats
          (templateWith Mustache (ImageUrl "data:{{image_type}};base64, {{image_data}}"))
          (Map.fromList [("image_type", "image/png"), ("image_data", "base64data")])
        assertFormats
          (templateWith Jinja2 (ImageUrl "data:{{ image_type }};base64, {{ image_data }}"))
          (Map.fromList [("image_type", "image/png"), ("image_data", "base64data")])
    , testCase "rejects nested f-string replacement fields in image_url blocks" $ do
        let template =
              fromMessages
                [ contentMessage
                    User
                    [ ImagePromptBlock FString $
                        ImageContent (ImageUrl "{img:{img.__class__.__name__}}") Nothing Nothing
                    ]
                ]
        case formatPrompt template (Map.singleton "img" "image-url") of
          Left err ->
            "Nested replacement fields are not allowed" `T.isInfixOf` T.pack (show err)
              @? "Expected nested replacement field error"
          Right _ -> assertFailure "Expected nested replacement field error"
    , testCase "formats image data blocks with metadata" $ do
        let metadata = object ["cache_control" .= object ["type" .= ("{cache_type}" :: Text)]]
            template =
              fromMessages
                [ contentMessage
                    User
                    [ ImagePromptBlock FString $
                        ImageContent (ImageBase64 Nothing "{source_data}") Nothing (Just metadata)
                    ]
                ]
            variables = Map.fromList [("cache_type", "ephemeral"), ("source_data", "base64data")]

        case formatPrompt template variables of
          Left err -> assertFailure $ "Expected image data prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ Message
                      User
                      ( ImageBlock
                          ( ImageContent
                              (ImageBase64 Nothing "base64data")
                              Nothing
                              (Just $ object ["cache_control" .= object ["type" .= ("ephemeral" :: Text)]])
                          )
                          :| []
                      )
                      Nothing
                      Nothing
                      Nothing
                  ]
    , testCase "formats mustache image data blocks with metadata" $ do
        let metadata = object ["cache_control" .= object ["type" .= ("{{cache_type}}" :: Text)]]
            template =
              fromMessages
                [ contentMessage
                    User
                    [ ImagePromptBlock Mustache $
                        ImageContent (ImageBase64 Nothing "{{source_data}}") Nothing (Just metadata)
                    ]
                ]
            variables = Map.fromList [("cache_type", "ephemeral"), ("source_data", "base64data")]

        case formatPrompt template variables of
          Left err -> assertFailure $ "Expected mustache image data prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ Message
                      User
                      ( ImageBlock
                          ( ImageContent
                              (ImageBase64 Nothing "base64data")
                              Nothing
                              (Just $ object ["cache_control" .= object ["type" .= ("ephemeral" :: Text)]])
                          )
                          :| []
                      )
                      Nothing
                      Nothing
                      Nothing
                  ]
    , testCase "round-trips rendered image data blocks through json" $ do
        let block = ImageBlock $ ImageContent (ImageUrl "https://example.com/image.png") Nothing Nothing
        decode (encode block) @?= Just block
    , testCase "rejects jinja2 image data blocks" $ do
        let template =
              fromMessages
                [ contentMessage
                    User
                    [ImagePromptBlock Jinja2 $ ImageContent (ImageBase64 Nothing "{{ source_data }}") Nothing Nothing]
                ]
        case formatPrompt template (Map.singleton "source_data" "base64data") of
          Left _ -> pure ()
          Right promptValue -> assertFailure $ "Expected jinja2 data block validation error, got " <> show promptValue
    , testCase "drops empty text blocks after mustache conditionals" $ do
        let template =
              fromMessages
                [ contentMessage
                    User
                    [ TextPromptBlock Mustache "{{#expectedResponse}}{{expectedResponse}}{{/expectedResponse}}"
                    , TextPromptBlock Mustache "Always present"
                    ]
                ]

        case formatPrompt template Map.empty of
          Left err -> assertFailure $ "Expected conditional prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [Message User (TextBlock "Always present" :| []) Nothing Nothing Nothing]
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
                (Map.fromList [("user", PartialText "Lucy"), ("name", PartialText "R2D2")])
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
    , testCase "formats role template messages with partial variables" $ do
        let template =
              fromMessages
                [ templateMessage System "You are {name}, a {role} assistant."
                , templateMessage User "{question}"
                ]
            partialTemplate = partial template (Map.fromList [("name", PartialText "Alice"), ("role", PartialText "helpful")])

        inputVariables partialTemplate @?= ["question"]
        case formatPrompt partialTemplate (Map.singleton "question" "What is Python?") of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "You are Alice, a helpful assistant."
                  , userMessage "What is Python?"
                  ]
    , testCase "infers required variables after partial variables" $ do
        let template =
              fromMessages
                [ templateMessage User "Do something with {question} using {context} giving it like {formatins}"
                ]
            partialTemplate = partial template (Map.singleton "formatins" (PartialText "some structure"))

        inputVariables partialTemplate @?= ["question", "context"]
    , testCase "composes partially initialized messages" $ do
        let prompt =
              partial
                (fromMessages [templateMessage System "Prompt {x} {y}"])
                (Map.singleton "x" (PartialText "1"))
            appendix = fromMessages [templateMessage System "Appendix {z}"]
            composed = extend prompt (messages appendix)

        case formatPrompt composed (Map.fromList [("y", "2"), ("z", "3")]) of
          Left err -> assertFailure $ "Expected formatted prompt, got " <> show err
          Right promptValue ->
            toMessages promptValue
              @?= [ textMessage System "Prompt 1 2"
                  , textMessage System "Appendix 3"
                  ]
    , testCase "formats messages placeholder with partial messages" $ do
        let prompt = fromMessages [messagesPlaceholder "history"]
            partialPrompt = partial prompt (Map.singleton "history" (PartialMessages [textMessage System "foo"]))

        inputVariables partialPrompt @?= []
        case formatPrompt partialPrompt Map.empty of
          Left err -> assertFailure $ "Expected formatted placeholder, got " <> show err
          Right promptValue -> toMessages promptValue @?= [textMessage System "foo"]

        case invoke
          partialPrompt
          (ChatPromptInputs Map.empty (Map.singleton "history" [textMessage System "bar"])) of
          Left err -> assertFailure $ "Expected runtime placeholder override, got " <> show err
          Right promptValue -> toMessages promptValue @?= [textMessage System "bar"]

        let optionalPrompt =
              fromMessages
                [ messagesPlaceholderWithOptions $
                    MessagesPlaceholderOptions "history" True Nothing
                ]
            partialOptionalPrompt = partial optionalPrompt (Map.singleton "history" (PartialMessages [textMessage System "foo"]))

        case formatPrompt optionalPrompt Map.empty of
          Left err -> assertFailure $ "Expected empty optional placeholder, got " <> show err
          Right promptValue -> toMessages promptValue @?= []
        case formatPrompt partialOptionalPrompt Map.empty of
          Left err -> assertFailure $ "Expected formatted optional placeholder, got " <> show err
          Right promptValue -> toMessages promptValue @?= [textMessage System "foo"]
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

serializationTests :: TestTree
serializationTests =
  testGroup
    "serialization"
    [ testCase "round-trips messages placeholder and chat prompt" $ do
        let placeholder = MessagesPlaceholder "bar" False Nothing
            prompt =
              fromMessages
                [ templateMessage System "foo"
                , messagesPlaceholder "bar"
                , templateMessage User "baz"
                ]

        decode (encode placeholder) @?= Just placeholder
        decode (encode prompt) @?= Just prompt
    , testCase "round-trips rich chat prompt template" $ do
        let prompt =
              fromMessages
                [ templateMessage System "You are an AI assistant named {name}."
                , contentMessage
                    System
                    [TextPromptBlock FString "You are an AI assistant named {name}."]
                , templateMessage System "you are {foo}"
                , contentMessage
                    User
                    [ TextPromptBlock FString "hello"
                    , TextPromptBlock FString "What's in this image?"
                    , TextPromptBlock FString "What's in this image?"
                    , ImagePromptBlock FString $
                        ImageContent (ImageUrl "data:image/jpeg;base64,{my_image}") Nothing Nothing
                    , ImagePromptBlock FString $
                        ImageContent (ImageUrl "{my_other_image}") Nothing Nothing
                    , ImagePromptBlock FString $ ImageContent (ImageUrl "{my_other_image}") (Just "medium") Nothing
                    , ImagePromptBlock FString $
                        ImageContent (ImageUrl "https://www.langchain.com/image.png") Nothing Nothing
                    , ImagePromptBlock FString $
                        ImageContent (ImageUrl "data:image/jpeg;base64,foobar") Nothing Nothing
                    ]
                , messagesPlaceholderWithOptions $ MessagesPlaceholderOptions "history" True (Just 3)
                , messagesPlaceholder "chat_history"
                , messagesPlaceholder "more_history"
                ]

        decode (encode prompt) @?= Just prompt
    ]

assertMissingVariable :: (Show err, Show a) => Text -> Either err a -> Assertion
assertMissingVariable expectedFragment result =
  case result of
    Left err ->
      if T.isInfixOf expectedFragment (T.pack (show err))
        then pure ()
        else assertFailure $ "Expected missing variable error, got " <> show err
    Right value ->
      assertFailure $ "Expected missing variable error, got " <> show value

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

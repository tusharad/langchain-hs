{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.Core (tests) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Memory
import Langchain.Model
  ( assistantMessage
  , extractMessageText
  , systemMessage
  , userMessage
  )

tests :: TestTree
tests =
  testGroup
    "Langchain.Memory.Core Tests"
    [ utilityTests
    , windowBufferMemoryTests
    , conversationBufferMemoryTests
    , persistenceTests
    , sessionMemoryTests
    , entityMemoryTests
    , combinedMemoryTests
    , concurrencyTests
    ]

utilityTests :: TestTree
utilityTests =
  testGroup
    "Utility Functions Tests"
    [ testCase "initialMessages creates list with a single system message" $ do
        let result = initialMessages "You are a helpful assistant"
        length result @?= 1
        case result of
          (m : _) -> m @?= systemMessage "You are a helpful assistant"
          [] -> assertFailure "Expected non-empty list"
    , testCase "trimMessages keeps last n messages (non-system)" $ do
        let msgs =
              [ systemMessage "System"
              , userMessage "User1"
              , assistantMessage "AI1"
              , userMessage "User2"
              ]
            trimmed = trimMessages 2 msgs
        trimmed @?= [assistantMessage "AI1", userMessage "User2"]
    , testCase "formatHistory formats roles and messages with default labels" $ do
        let msgs = [userMessage "Hi", assistantMessage "Hello!"]
        formatHistory msgs @?= "Human: Hi\nAI: Hello!"
    ]

windowBufferMemoryTests :: TestTree
windowBufferMemoryTests =
  testGroup
    "WindowBufferMemory Tests"
    [ testCase "messages returns current messages" $ do
        let initialMsgs = [systemMessage "System"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= initialMsgs
    , testCase "addMessage adds message when under capacity" $ do
        let initialMsgs = [systemMessage "System"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          addMessage memory (userMessage "User1")
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> msgs @?= [systemMessage "System", userMessage "User1"]
    , testCase "addMessage trims oldest non-system message when at capacity" $ do
        let initialMsgs =
              [ systemMessage "System"
              , userMessage "User1"
              , assistantMessage "AI1"
              ]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          addMessage memory (userMessage "User2")
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs ->
            msgs @?= [systemMessage "System", assistantMessage "AI1", userMessage "User2"]
    , testCase "clear resets to default system message" $ do
        let initialMsgs =
              [ systemMessage "System"
              , userMessage "User1"
              , assistantMessage "AI1"
              ]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          clear memory
          messages memory
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> do
            length msgs @?= 1
            case msgs of
              (m : _) -> m @?= systemMessage "You are a helpful AI assistant"
              [] -> assertFailure "Expected non-empty messages"
    , testCase "resetWindowBuffer restores initial seeded messages" $ do
        let initialMsgs = [systemMessage "Initial system prompt"]
        memory <- newWindowBufferMemory 3 initialMsgs
        res <- runExceptT $ do
          saveContext memory "User1" "AI1"
          saveContext memory "User2" "AI2"
          resetWindowBuffer memory
          messages memory
        case res of
          Left err -> assertFailure $ "Failed: " ++ show err
          Right msgs -> msgs @?= initialMsgs
    ]

conversationBufferMemoryTests :: TestTree
conversationBufferMemoryTests =
  testGroup
    "ConversationBufferMemory Tests"
    [ testCase "retains unbounded conversation turns" $ do
        mem <- newConversationBufferMemory [systemMessage "System prompt"]
        res <- runExceptT $ do
          saveContext mem "User Turn 1" "AI Turn 1"
          saveContext mem "User Turn 2" "AI Turn 2"
          saveContext mem "User Turn 3" "AI Turn 3"
          messages mem
        case res of
          Left err -> assertFailure $ "Failed: " ++ show err
          Right msgs -> length msgs @?= 7 -- 1 system + 6 turns
    , testCase "loadMemoryVariables produces formatted history map" $ do
        mem <- newConversationBufferMemory []
        res <- runExceptT $ do
          saveContext mem "Hello" "Hi there!"
          loadMemoryVariables mem
        case res of
          Left err -> assertFailure $ "Failed: " ++ show err
          Right vars -> Map.lookup "history" vars @?= Just "Human: Hello\nAI: Hi there!"
    , testCase "resetConversationBuffer restores initial seeded messages" $ do
        let initialMsgs = [systemMessage "Base prompt"]
        mem <- newConversationBufferMemory initialMsgs
        res <- runExceptT $ do
          saveContext mem "Hello" "World"
          resetConversationBuffer mem
          messages mem
        case res of
          Left err -> assertFailure $ "Failed: " ++ show err
          Right msgs -> msgs @?= initialMsgs
    ]

persistenceTests :: TestTree
persistenceTests =
  testGroup
    "Persistence Tests"
    [ testCase "saveMemoryToFile and loadMemoryFromFile preserves full conversation history" $ do
        withSystemTempDirectory "memory-test" $ \tmpDir -> do
          let filePath = tmpDir </> "conversation.json"
          mem1 <- newConversationBufferMemory [systemMessage "Assistant system"]
          res1 <- runExceptT $ do
            saveContext mem1 "What is Haskell?" "A purely functional programming language."
            saveMemoryToFile filePath mem1

          case res1 of
            Left err -> assertFailure $ "Save failed: " ++ show err
            Right () -> do
              mem2 <- newConversationBufferMemory []
              res2 <- runExceptT $ do
                loadMemoryFromFile filePath mem2
                messages mem2
              case res2 of
                Left err -> assertFailure $ "Load failed: " ++ show err
                Right msgs -> do
                  length msgs @?= 3
                  msgs
                    @?= [ systemMessage "Assistant system"
                        , userMessage "What is Haskell?"
                        , assistantMessage "A purely functional programming language."
                        ]
    ]

sessionMemoryTests :: TestTree
sessionMemoryTests =
  testGroup
    "SessionMemoryStore Tests"
    [ testCase "maintains isolated memory per session ID" $ do
        store <- newSessionMemoryStore (\_ -> newConversationBufferMemory [])
        res <- runExceptT $ do
          memAlice <- getSessionMemory store "user-alice"
          memBob <- getSessionMemory store "user-bob"
          saveContext memAlice "Alice question" "Alice answer"
          saveContext memBob "Bob question" "Bob answer"

          msgsAlice <- messages memAlice
          msgsBob <- messages memBob
          pure (msgsAlice, msgsBob)

        case res of
          Left err -> assertFailure $ "Session test failed: " ++ show err
          Right (aliceMsgs, bobMsgs) -> do
            length aliceMsgs @?= 2
            length bobMsgs @?= 2
            aliceMsgs @?= [userMessage "Alice question", assistantMessage "Alice answer"]
            bobMsgs @?= [userMessage "Bob question", assistantMessage "Bob answer"]
    , testCase "session management: list, count, delete, and clear" $ do
        store <- newSessionMemoryStore (\_ -> newWindowBufferMemory 5 [])
        _ <- getSessionMemory store "s1"
        _ <- getSessionMemory store "s2"
        _ <- getSessionMemory store "s3"

        cnt <- sessionCount store
        cnt @?= 3

        sList <- listSessions store
        length sList @?= 3

        deleteSession store "s2"
        cnt2 <- sessionCount store
        cnt2 @?= 2

        clearAllSessions store
        cnt3 <- sessionCount store
        cnt3 @?= 0
    ]

entityMemoryTests :: TestTree
entityMemoryTests =
  testGroup
    "EntityMemory Tests"
    [ testCase "tracks entity facts and formats them into system message" $ do
        mem <- newEntityMemory (Map.fromList [("user_name", "Alice")])
        setEntity mem "favorite_language" "Haskell"

        val <- getEntity mem "favorite_language"
        val @?= Just "Haskell"

        res <- runExceptT $ messages mem
        case res of
          Left err -> assertFailure $ "Failed: " ++ show err
          Right msgs -> do
            length msgs @?= 1
            case msgs of
              (m : _) -> assertBool "Contains entities" ("favorite_language: Haskell" `T.isInfixOf` extractMessageText m)
              [] -> assertFailure "Expected non-empty message"
    , testCase "resetEntitiesToInitial restores original seeded facts" $ do
        mem <- newEntityMemory (Map.fromList [("role", "Engineer")])
        setEntity mem "role" "Manager"
        setEntity mem "location" "Remote"
        resetEntitiesToInitial mem
        val <- getEntity mem "role"
        val @?= Just "Engineer"
        loc <- getEntity mem "location"
        loc @?= Nothing
    , testCase "deleteEntity and clearEntities remove attributes" $ do
        mem <- newEntityMemory (Map.fromList [("a", "1"), ("b", "2")])
        deleteEntity mem "a"
        valA <- getEntity mem "a"
        valA @?= Nothing
        clearEntities mem
        allEnts <- getAllEntities mem
        assertBool "All entities cleared" (Map.null allEnts)
    ]

combinedMemoryTests :: TestTree
combinedMemoryTests =
  testGroup
    "CombinedMemory Tests"
    [ testCase "aggregates messages from multiple memory stores" $ do
        bufMem <- newWindowBufferMemory 5 [systemMessage "System"]
        entMem <- newEntityMemory (Map.fromList [("role", "Developer")])
        let combined = newCombinedMemory [SomeMemory bufMem, SomeMemory entMem]

        res <- runExceptT $ do
          saveContext combined "Hello" "World"
          messages combined

        case res of
          Left err -> assertFailure $ "Combined failed: " ++ show err
          Right msgs -> do
            -- bufMem has 3 messages (system + user + ai), entMem has 1 entity message
            length msgs @?= 4
    ]

concurrencyTests :: TestTree
concurrencyTests =
  testGroup
    "Concurrency Tests"
    [ testCase "100 concurrent writes produce consistent window size" $ do
        let initialMsgs = [systemMessage "System"]
            maxSize = 200
        memory <- newWindowBufferMemory maxSize initialMsgs
        forConcurrently_ [1 .. 100 :: Int] $ \i -> do
          _ <- runExceptT $ addMessage memory (userMessage $ "Msg " <> T.pack (show i))
          pure ()
        result <- runExceptT $ messages memory
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msgs -> length msgs @?= 101
    ]

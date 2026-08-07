{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.TokenBufferMemory (tests) where

import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Error (LangchainError, llmError, toText)
import Langchain.LLM.Core
import Langchain.Memory.Core (BaseMemory (..))
import qualified Langchain.Memory.TokenBufferMemory as TB
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

#if MIN_VERSION_base(4,19,0)
import Data.List (unsnoc)
#else
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
#endif

mkMsg :: Role -> Text -> Message
mkMsg role1 content1 = Message role1 content1 defaultMessageData

tests :: TestTree
tests =
  testGroup
    "TokenBufferMemory Tests"
    [ constructorTests
    , addMessageTests
    , addUserAndAiMessageTests
    , clearTest
    ]

constructorTests :: TestTree
constructorTests =
  testGroup
    "Constructor Tests"
    [ testCase "TokenBufferMemory initializes with system message" $ do
        let mem = TB.TokenBufferMemory 100 (NE.singleton (mkMsg System "You are an AI model"))
        TB.maxTokens mem @?= 100
        TB.tokenBufferMessages mem
          @?= NE.singleton (mkMsg System "You are an AI model")
    ]

addMessageTests :: TestTree
addMessageTests =
  testGroup
    "addMessage logic"
    [ testCase "Add message within token limit" $ do
        let sysMsg = mkMsg System "sys"
            user1 = mkMsg User "12345678" -- 2 tokens
            user2 = mkMsg User "12345678" -- 2 tokens
            initial = TB.TokenBufferMemory 10 (NE.fromList [sysMsg, user1])
        res <- addMessage initial user2
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right mem ->
            NE.toList (TB.tokenBufferMessages mem) @?= [sysMsg, user1, user2]
    , testCase "Evicts oldest non-system message when exceeding limit" $ do
        let sysMsg = mkMsg System "sys!"
            user1 = mkMsg User "12345678"
            user2 = mkMsg User "12345678"
            initial = TB.TokenBufferMemory 4 (NE.fromList [sysMsg, user1])
        res <- addMessage initial user2
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right mem ->
            NE.toList (TB.tokenBufferMessages mem) @?= [sysMsg, user2]
    , testCase "Error when system message and new message exceed limit" $ do
        let sysMsg = mkMsg System "12345678" -- 2 tokens
            userMsg = mkMsg User "12345678" -- 2 tokens
            initial = TB.TokenBufferMemory 3 (NE.fromList [sysMsg]) -- max 3 tokens
        res <- addMessage initial userMsg
        case res of
          Left err ->
            assertBool "Should contain limit error message" ("exceeds limit" `T.isInfixOf` toText err)
          Right _ -> assertFailure "Expected Left due to overflow"
    , testCase "BaseMemory messages retrieves history" $ do
        let initial = TB.TokenBufferMemory 10 (NE.fromList [mkMsg System "init"])
        res <- messages initial
        res @?= Right (NE.fromList [mkMsg System "init"])
    ]

addUserAndAiMessageTests :: TestTree
addUserAndAiMessageTests =
  testGroup
    "addUserMessage and addAiMessage"
    [ testCase "addUserMessage adds User role message" $ do
        let initial = TB.TokenBufferMemory 100 (NE.fromList [mkMsg System ""])
            userContent = "Hello!"
        updated <- addUserMessage initial userContent
        case updated of
          Right mem -> do
            let msgs = NE.toList $ TB.tokenBufferMessages mem
            unsnoc msgs @?= Just ([mkMsg System ""], mkMsg User userContent)
          Left err -> assertFailure $ "Unexpected Left: " ++ show err
    , testCase "addAiMessage adds Assistant role message" $ do
        let initial = TB.TokenBufferMemory 100 (NE.fromList [mkMsg System ""])
            aiContent = "I'm an assistant."
        updated <- addAiMessage initial aiContent
        case updated of
          Right mem -> do
            let msgs = NE.toList $ TB.tokenBufferMessages mem
            unsnoc msgs @?= Just ([mkMsg System ""], mkMsg Assistant aiContent)
          Left err -> assertFailure $ "Unexpected Left: " ++ show err
    ]

clearTest :: TestTree
clearTest =
  testCase "clear resets messages to default system message" $ do
    let initial = TB.TokenBufferMemory 100 (NE.fromList [mkMsg User "old"])
    cleared <- clear initial
    assertBool "Clear should be right" (isRight cleared)
    case cleared of
      Right mem ->
        TB.tokenBufferMessages mem
          @?= NE.singleton (mkMsg System "You are an AI model")
      Left _ -> assertFailure "Clear failed unexpectedly"

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.TokenBufferMemory (tests) where

import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Error (llmError)
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

runAddAndGet :: TB.TokenBufferMemory -> [Message] -> IO ChatMessage
runAddAndGet initial msgs = do
  TB.tokenBufferMessages
    <$> foldl
      ( \mem_ msg -> do
          mem <- mem_
          eRes <- addMessage mem msg
          case eRes of
            Left _ -> pure mem
            Right r -> pure r
      )
      (pure initial)
      msgs

-- Tests
tests :: TestTree
tests =
  testGroup
    "TokenBufferMemory Tests"
    [ countTokensTests
    , addMessageTests
    , addUserAndAiMessageTests
    , clearTest
    ]

countTokensTests :: TestTree
countTokensTests =
  testGroup
    "countTokens"
    [ testCase "Empty message list" $
        TB.countTokens [] @?= 0
    , testCase "Single message" $
        TB.countTokens [mkMsg System "abc"] @?= ceiling (3 / 4 :: Double)
    , testCase "Multiple messages" $
        TB.countTokens [mkMsg User "hello", mkMsg Assistant "world"] @?= ceiling (5 / 4 :: Double) * 2
    ]

addMessageTests :: TestTree
addMessageTests =
  testGroup
    "addMessage"
    [ testCase "Add within limit" $ do
        let initial = TB.TokenBufferMemory 100 (NE.fromList [mkMsg System ""])
            newMsg = mkMsg User "content"
        updated <- runAddAndGet initial [newMsg]
        NE.length updated @?= 2
    , testCase "Exceeding token limit trims old messages" $ do
        -- Total tokens allowed: 6
        -- Each message has 3 characters ⇒ ~1 token each
        let maxTok = 2
            baseMsg = mkMsg System "aaa"
            userMsg = mkMsg User "bbb"
            aiMsg = mkMsg Assistant "ccc"

            initial = TB.TokenBufferMemory maxTok (NE.fromList [baseMsg])

        updated <- runAddAndGet initial [userMsg, aiMsg]
        NE.toList updated @?= [baseMsg, aiMsg] -- first message gets trimmed
    , testCase "New message alone exceeds limit" $ do
        let initial = TB.TokenBufferMemory 1 (NE.fromList [mkMsg System ""])
            bigMsg = mkMsg User (T.replicate 10 "a") -- 10 chars → 2.5 tokens (ceil to 3)
        result <- addMessage initial bigMsg
        assertEqual
          "New message is exceeding limit"
          (Left (llmError "New message is exceeding limit" Nothing Nothing))
          result
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

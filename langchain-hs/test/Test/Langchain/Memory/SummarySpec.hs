{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.SummarySpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Error (LangchainError (..))
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.Summary
import Langchain.Model (extractMessageText, userMessage)
import Test.Langchain.Provider.Mock (newMockErrorModel, newMockModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Memory.SummarySpec"
    [ testCase "SummaryMemory summarizes when exceeding threshold" $ do
        let mockModel = newMockModel "Summarized context of user questions"
        mem <- newSummaryMemory mockModel 3 []
        res <- runExceptT $ do
          addMessage mem (userMessage "Message 1")
          addMessage mem (userMessage "Message 2")
          addMessage mem (userMessage "Message 3")
          addMessage mem (userMessage "Message 4")
          summaryTxt <- getSummary mem
          allMsgs <- messages mem
          pure (summaryTxt, allMsgs)
        case res of
          Left err -> assertFailure ("SummaryMemory failed: " ++ show err)
          Right (sTxt, msgs) -> do
            sTxt @?= "Summarized context of user questions"
            assertBool
              "Messages contains summary in system message"
              (any (\m -> "Summary" `T.isInfixOf` extractMessageText m) msgs)
    , testCase "SummaryMemory retains messages without data loss if summarizer fails" $ do
        let failingModel = newMockErrorModel (LLMError "Rate limit exceeded" Nothing)
        mem <- newSummaryMemory failingModel 2 []
        _ <- runExceptT $ do
          addMessage mem (userMessage "Message 1")
          addMessage mem (userMessage "Message 2")
          addMessage mem (userMessage "Message 3") -- triggers summarizer which fails
        allMsgs <- runExceptT $ messages mem
        case allMsgs of
          Right msgs -> length msgs @?= 3 -- History was NOT deleted!
          Left err -> assertFailure ("Failed to retrieve messages: " ++ show err)
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.SummarySpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (extractMessageText, newMockModel, userMessage)
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.Summary

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
    ]

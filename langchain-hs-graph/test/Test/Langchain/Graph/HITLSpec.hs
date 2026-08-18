{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.HITLSpec (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import Langchain.Core.Error (LangchainError)
import Langchain.Graph.Checkpointer
import Langchain.Graph.HITL
import Langchain.Graph.StateGraph
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.HITLSpec"
    [ testCase "hitlNode triggers HITL interrupt error and saves checkpoint" $ do
        cp <- newMemoryCheckpointer
        let threadId = "thread-hitl"
            inner :: Text -> IO (Either LangchainError Text)
            inner s = pure $ Right s
            node1 = hitlNode cp threadId "approvalNode" inner
        -- Manually test hitlNode action directly
        res <- nodeAction node1 ("initial-state" :: Text)
        case res of
          Left err ->
            assertBool "Is HITL Interrupt" (case isHITLInterrupt err of Just "approvalNode" -> True; _ -> False)
          Right _ -> assertFailure "Expected HITL Interrupt error"
    , testCase "resumeGraph resumes execution from saved state" $ do
        cp <- newMemoryCheckpointer
        let threadId = "thread-resume"
            initialState = "draft-content" :: Text
        _ <- saveCheckpoint cp threadId "node2" initialState

        let action :: Text -> ExceptT LangchainError IO (Either LangchainError Text)
            action s = pure $ Right (s <> " -> approved")
            g =
              addEdge "node2" endNodeId $
                addNode "node2" action $
                  emptyStateGraph replaceFieldReducer
            Right compiled = compileGraph g

        res <-
          runExceptT $ resumeGraph compiled cp threadId "node2" "node2" (\s -> s <> " [human-reviewed]")
        res @?= Right "draft-content [human-reviewed] -> approved"
    ]

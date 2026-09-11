{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.MultiAgentSpec (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel (..), assistantMessage)
import Langchain.Graph.MultiAgent
import Langchain.Graph.StateGraph
import Test.Tasty
import Test.Tasty.HUnit

newtype MockModel = MockModel Text

instance ChatModel MockModel where
  type ModelConfig MockModel = ()
  invoke (MockModel resp) _ _ = pure $ assistantMessage resp
  stream = undefined

newMockModel :: Text -> MockModel
newMockModel = MockModel

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.MultiAgentSpec"
    [ testCase "supervisorNode routes based on mock LLM response" $ do
        let mockModel = newMockModel "researcher"
            node =
              supervisorNode
                mockModel
                "supervisor"
                [("researcher", "researchNode")]
                id
                (\target s -> s <> ":" <> target)
        res <- runExceptT $ nodeAction node ("task-input" :: Text)
        res @?= Right (Right ("task-input:researchNode" :: Text))
    , testCase "embedSubGraphNode executes nested graph" $ do
        let action :: Text -> ExceptT LangchainError IO (Either LangchainError Text)
            action s = pure $ Right (s <> " [sub-processed]")
            subG =
              addEdge startNodeId "sub1" $
                addEdge "sub1" endNodeId $
                  addNode "sub1" action $
                    emptyStateGraph replaceFieldReducer
        case compileGraph subG of
          Left err -> assertFailure $ "Failed to compile sub-graph: " ++ show err
          Right compiledSub -> do
            let parentN = embedSubGraphNode "subGraphNode" compiledSub id (\p s -> p <> " | " <> s)
            res <- runExceptT $ nodeAction parentN ("parent-input" :: Text)
            res @?= Right (Right ("parent-input | parent-input [sub-processed]" :: Text))
    ]

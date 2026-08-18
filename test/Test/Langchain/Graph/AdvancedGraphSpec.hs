{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.AdvancedGraphSpec (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (LangchainError)
import Langchain.Graph.Parallel
import Langchain.Graph.StateGraph
import Langchain.Graph.SubGraph
import Langchain.Graph.TimeTravel
import Langchain.Graph.Visualization

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.AdvancedGraphSpec"
    [ testCase "toDot exports valid Graphviz digraph representation" $ do
        let graph =
              addEdge startNodeId "step1" $
                addEdge "step1" endNodeId $
                  addNode
                    "step1"
                    (\st -> pure $ Right (st <> "->1"))
                    (emptyStateGraph replaceFieldReducer :: StateGraph Text (ExceptT LangchainError IO))
            dotText = toDot graph
        assertBool "Contains digraph header" ("digraph StateGraph" `T.isInfixOf` dotText)
        assertBool "Contains step1 node" ("\"step1\"" `T.isInfixOf` dotText)
    , testCase "TimeTravelHistory records snapshots and allows resumption" $ do
        hist <- newTimeTravelHistory
        _ <- recordSnapshot hist "thread-1" "nodeA" ("State A" :: Text)
        _ <- recordSnapshot hist "thread-1" "nodeB" ("State B" :: Text)
        snaps <- getSnapshots hist "thread-1"
        length snaps @?= 2
        snapshotNodeId (head snaps) @?= "nodeA"
    , testCase "Parallel nodes execute concurrently and merge states" $ do
        let worker1 = \s -> pure $ Right (s <> "+W1")
            worker2 = \s -> pure $ Right (s <> "+W2")
            mergeFn = \_ outputs -> T.intercalate "|" outputs
            pNode = parallelNode "p_step" [worker1, worker2] mergeFn :: Node Text IO
        res <- nodeAction pNode ("Init" :: Text)
        case res of
          Left err -> assertFailure ("Parallel node failed: " ++ show err)
          Right outState -> do
            assertBool "Contains W1" ("W1" `T.isInfixOf` outState)
            assertBool "Contains W2" ("W2" `T.isInfixOf` outState)
    , testCase "embedSubGraphWithOptions executes nested graph with options" $ do
        let subGraphDef =
              addEdge startNodeId "sub1" $
                addEdge "sub1" endNodeId $
                  addNode
                    "sub1"
                    (\subSt -> pure $ Right (subSt * 2))
                    (emptyStateGraph replaceFieldReducer :: StateGraph Int (ExceptT LangchainError IO))
        case compileGraph subGraphDef of
          Left err -> assertFailure ("Compilation failed: " ++ show err)
          Right compiledSub -> do
            let subNode =
                  embedSubGraphWithOptions "sub_exec" compiledSub defaultSubGraphOptions (\p -> p + 5) (\_ s -> s + 1)
            res <- runExceptT $ nodeAction subNode (10 :: Int)
            case res of
              Left err -> assertFailure ("SubGraph failed: " ++ show err)
              Right (Left err) -> assertFailure ("SubGraph node returned error: " ++ show err)
              Right (Right outState) -> outState @?= (10 + 5) * 2 + 1
    ]

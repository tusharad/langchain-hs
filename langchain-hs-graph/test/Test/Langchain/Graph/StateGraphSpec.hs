{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.StateGraphSpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Error (LangchainError, errorMessage)
import Langchain.Graph.StateGraph
import Langchain.Runnable (RunnableTree (..), runPure, (|>>))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.StateGraphSpec"
    [ testCase "compileGraph succeeds on valid node graph" $ do
        let action :: Text -> ExceptT LangchainError IO (Either LangchainError Text)
            action s = pure $ Right (s <> " world")
            g = addNode "node1" action $ emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right _ -> pure ()
    , testCase "runGraph executes static transitions correctly" $ do
        let action :: Text -> ExceptT LangchainError IO (Either LangchainError Text)
            action s = pure $ Right (s <> " world")
            g =
              addEdge "node1" endNodeId $
                addNode "node1" action $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "node1" ("hello" :: Text)
            res @?= Right "hello world"
    , testCase "addTransformNode updates state purely" $ do
        let g =
              addEdge "step1" endNodeId $
                addTransformNode "step1" (<> " [transformed]") $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "step1" ("input" :: Text)
            res @?= Right "input [transformed]"
    , testCase "addRunnableTreeNode executes pipeline AST within graph node" $ do
        let pipeline :: RunnableTree (ExceptT LangchainError IO) Text Text
            pipeline = runPure (<> " -> p1") |>> runPure (<> " -> p2")
            g =
              addEdge "runnableNode" endNodeId $
                addRunnableTreeNode "runnableNode" pipeline id (\_ out -> out) $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "runnableNode" ("start" :: Text)
            res @?= Right "start -> p1 -> p2"
    , testCase "recursionLimit terminates cyclic graph with error" $ do
        let g =
              setRecursionLimit 5 $
                addEdge "loop" "loop" $
                  addTransformNode "loop" (\n -> (n + 1 :: Int)) $
                    emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "loop" (0 :: Int)
            case res of
              Left err ->
                assertBool
                  "Contains recursion limit message"
                  ("recursion limit exceeded" `T.isInfixOf` errorMessage err)
              Right val -> assertFailure $ "Expected recursion error, but got: " ++ show val
    , testCase "addSubGraphNode embeds and executes sub-graph cleanly" $ do
        let subG =
              addEdge "sub1" endNodeId $
                addTransformNode "sub1" (<> "_sub") $
                  emptyStateGraph replaceFieldReducer
        case compileGraph subG of
          Left err -> assertFailure $ "Sub-graph compile failed: " ++ show err
          Right compiledSub -> do
            let parentG =
                  addEdge "parent1" endNodeId $
                    addSubGraphNode "parent1" "sub1" compiledSub id (\_ subOut -> "parent:" <> subOut) $
                      emptyStateGraph replaceFieldReducer
            case compileGraph parentG of
              Left err -> assertFailure $ "Parent compile failed: " ++ show err
              Right compiledParent -> do
                res <- runExceptT $ runGraph compiledParent "parent1" ("init" :: Text)
                res @?= Right "parent:init_sub"
    , testCase "setStepLogger logs transition messages" $ do
        logsVar <- newTVarIO []
        let logger msg = atomically $ modifyTVar' logsVar (msg :)
            g =
              setStepLogger logger $
                addEdge "a" "b" $
                  addEdge "b" endNodeId $
                    addTransformNode "a" (<> "_A") $
                      addTransformNode "b" (<> "_B") $
                        emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compile failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "a" ("start" :: Text)
            res @?= Right "start_A_B"
            logs <- readTVarIO logsVar
            assertBool "Recorded log messages" (length logs >= 4)
            assertBool "Logged entering node 'a'" (any ("Entering node 'a'" `T.isInfixOf`) logs)
    ]

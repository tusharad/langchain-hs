{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.CompilationSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (errorMessage)
import Langchain.Graph.StateGraph

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.CompilationSpec"
    [ testCase "Empty graph fails compilation" $ do
        let g = emptyStateGraph replaceFieldReducer :: StateGraph T.Text IO
        case compileGraph g of
          Left err -> assertBool "Error indicates empty nodes" ("at least one node" `T.isInfixOf` errorMessage err)
          Right _ -> assertFailure "Expected empty graph compilation failure"
    , testCase "Single node graph compiles and runs to end" $ do
        let g =
              addEdge "process" endNodeId $
                addNode "process" (\s -> pure $ Right (s <> "_processed")) $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure ("Compilation failed: " ++ show err)
          Right cg -> do
            res <- runExceptT $ runGraph cg "process" ("item" :: T.Text)
            res @?= Right "item_processed"
    , testCase "3-node linear pipeline compiles and preserves state flow" $ do
        let g =
              addEdge "n1" "n2" $
                addEdge "n2" "n3" $
                  addEdge "n3" endNodeId $
                    addNode "n1" (\s -> pure $ Right (s <> " -> step1")) $
                      addNode "n2" (\s -> pure $ Right (s <> " -> step2")) $
                        addNode "n3" (\s -> pure $ Right (s <> " -> step3")) $
                          emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure ("Compilation failed: " ++ show err)
          Right cg -> do
            res <- runExceptT $ runGraph cg "n1" ("start" :: T.Text)
            res @?= Right "start -> step1 -> step2 -> step3"
    , testCase "Conditional edge routes dynamically based on condition" $ do
        let routeFn s = pure $ Right $ if "urgent" `T.isInfixOf` s then "fastTrack" else "normalTrack"
            g =
              addConditionalEdge "dispatch" routeFn $
                addEdge "fastTrack" endNodeId $
                  addEdge "normalTrack" endNodeId $
                    addNode "dispatch" (pure . Right) $
                      addNode "fastTrack" (\s -> pure $ Right (s <> " [FAST]")) $
                        addNode "normalTrack" (\s -> pure $ Right (s <> " [NORMAL]")) $
                          emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure ("Compilation failed: " ++ show err)
          Right cg -> do
            resFast <- runExceptT $ runGraph cg "dispatch" "urgent invoice"
            resFast @?= Right "urgent invoice [FAST]"
            resNormal <- runExceptT $ runGraph cg "dispatch" "general query"
            resNormal @?= Right "general query [NORMAL]"
    , testCase "Node overwrite replaces node function in state graph" $ do
        let g =
              addEdge "n1" endNodeId $
                addNode "n1" (\s -> pure $ Right (s <> " v2")) $
                  addNode "n1" (\s -> pure $ Right (s <> " v1")) $
                    emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure ("Compilation failed: " ++ show err)
          Right cg -> do
            res <- runExceptT $ runGraph cg "n1" ("base" :: T.Text)
            res @?= Right "base v2"
    ]

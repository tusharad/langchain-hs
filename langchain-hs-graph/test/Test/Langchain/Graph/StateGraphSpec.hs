{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.StateGraphSpec (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import Langchain.Error (LangchainError)
import Langchain.Graph.StateGraph
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
    ]

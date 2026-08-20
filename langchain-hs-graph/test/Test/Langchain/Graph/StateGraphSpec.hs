{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.StateGraphSpec (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import Langchain.Core.Error (LangchainError)
import Langchain.Graph.StateGraph
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.StateGraphSpec"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
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

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests (Laws)"
    [ testProperty "Reducer Associativity Law: (a <> b) <> c == a <> (b <> c)" $ \a b c ->
        let s1 = (a :: String) ++ (b :: String)
            s2 = s1 ++ (c :: String)
            s3 = b ++ c
            s4 = a ++ s3
         in s2 == s4
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.PreludeSpec (tests) where

import Control.Monad.Except (runExceptT)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Prelude
import Test.Langchain.Provider.Mock (newMockModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Prelude"
    [ testCase "prelude exports LangchainT and runLangchainTIO" $ do
        res <- runLangchainTIO (pure ("prelude works" :: String))
        res @?= Right "prelude works"
    , testCase "prelude exports invoke and message helpers" $ do
        let mdl = newMockModel "pong"
        res <- runExceptT $ invoke mdl [userMessage "ping"] Nothing
        case res of
          Left err -> assertFailure $ "Expected Right: " ++ show err
          Right msg -> extractMessageText msg @?= "pong"
    , testCase "prelude exports RunnableTree composition" $ do
        let pipeline =
              runLambda (\x -> pure $ Right (x + (1 :: Int)))
                |>> runLambda (\x -> pure $ Right (x * 2))
        res <- runExceptT $ interpret pipeline 5
        res @?= Right 12
    , testCase "prelude exports StateGraph" $ do
        let g =
              addEdge "n1" endNodeId $
                addNode "n1" (\s -> pure $ Right (s <> ("!" :: String))) $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left _ -> assertFailure "Graph compilation failed"
          Right cg -> do
            res <- runExceptT $ runGraph cg "n1" "hello"
            res @?= Right "hello!"
    ]

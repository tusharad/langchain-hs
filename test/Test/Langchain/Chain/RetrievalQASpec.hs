{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Chain.RetrievalQASpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as HM
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Chain.RetrievalQA
import Langchain.Core.Model
  ( extractMessageText
  )
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Provider.Mock (newMockModel)
import Langchain.Retriever.Core (Retriever (..))

data TestRetriever = TestRetriever
  deriving (Show, Eq)

instance Retriever TestRetriever where
  getRelevantDocuments _ q =
    pure [Document (TL.fromStrict $ "Haskell context for " <> q) HM.empty]

tests :: TestTree
tests =
  testGroup
    "Langchain.Chain.RetrievalQA"
    [ testCase "runRetrievalQA retrieves documents and invokes model" $ do
        let mockModel = newMockModel "Haskell is a purely functional programming language."
            retriever_ = TestRetriever
            qa = newRetrievalQA mockModel retriever_
        res <- runExceptT $ runRetrievalQA qa "What is Haskell?"
        case res of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
          Right msg -> extractMessageText msg @?= "Haskell is a purely functional programming language."
    ]

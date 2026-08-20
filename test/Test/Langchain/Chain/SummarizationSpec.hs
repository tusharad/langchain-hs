{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Chain.SummarizationSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Chain.Summarization
import Langchain.Core.Model (newMockModel)
import Langchain.DocumentLoader.Core (Document (..))

tests :: TestTree
tests =
  testGroup
    "Langchain.Chain.SummarizationSpec"
    [ testCase "runSummarizationChain invokes model on concatenated documents" $ do
        let model = newMockModel "Concise summary of functional programming."
            chain = newSummarizationChain model StuffSummary
            docs =
              [ Document {pageContent = "Haskell is pure.", metadata = Map.empty}
              , Document {pageContent = "Immutability guarantees safety.", metadata = Map.empty}
              ]
        res <- runExceptT $ runSummarizationChain chain docs
        res @?= Right "Concise summary of functional programming."
    ]

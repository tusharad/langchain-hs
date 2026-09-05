{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Chain.ChainsSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Chain.MapReduce
import Langchain.Core.Model (extractMessageText)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Provider.Mock (newMockModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Chain.ChainsSpec"
    [ testCase "MapReduceChain maps and reduces across documents" $ do
        let mockModel = newMockModel "Synthesized summary"
            docs = [Document "Doc A" Map.empty, Document "Doc B" Map.empty]
            chain = newMapReduceChain mockModel
        res <- runExceptT $ runMapReduceChain chain docs Map.empty
        case res of
          Left err -> assertFailure ("MapReduceChain failed: " ++ show err)
          Right msg -> extractMessageText msg @?= "Synthesized summary"
    ]

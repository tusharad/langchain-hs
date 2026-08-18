{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Chain.ChainsSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Chain.Conversational
import Langchain.Chain.MapReduce
import Langchain.Chain.Sequential
import Langchain.Chain.StuffDocuments
import Langchain.Core.Model (extractMessageText, newMockModel)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Memory.Core (newWindowBufferMemory)
import Langchain.PromptTemplate (PromptTemplate (..))

tests :: TestTree
tests =
  testGroup
    "Langchain.Chain.ChainsSpec"
    [ testCase "SequentialChain threads variables across steps" $ do
        let step1 =
              ChainStep
                "addGreeting"
                (\vars -> pure $ Map.insert "greeting" ("Hello, " <> Map.findWithDefault "World" "name" vars) vars)
            step2 =
              ChainStep
                "addExcitement"
                (\vars -> pure $ Map.insert "finalMsg" (Map.findWithDefault "" "greeting" vars <> "!!!") vars)
            chain = newSequentialChain [step1, step2]
            initVars = Map.singleton "name" "Haskell"
        res <- runExceptT $ runSequentialChain chain initVars
        case res of
          Left _ -> assertFailure "SequentialChain failed: "
          Right outVars -> do
            Map.lookup "greeting" outVars @?= Just "Hello, Haskell"
            Map.lookup "finalMsg" outVars @?= Just "Hello, Haskell!!!"
    , testCase "ConversationalChain accumulates history with memory" $ do
        let mockModel = newMockModel "I am an AI assistant."
        mem <- newWindowBufferMemory 10 []
        let chain = newConversationalChain mockModel mem Nothing
        res <- runExceptT $ runConversationalChain chain "Hi there!"
        case res of
          Left err -> assertFailure ("ConversationalChain failed: " ++ show err)
          Right reply -> reply @?= "I am an AI assistant."
    , testCase "StuffDocumentsChain concatenates documents into prompt" $ do
        let mockModel = newMockModel "Answer based on context"
            docs = [Document "Doc 1 content" Map.empty, Document "Doc 2 content" Map.empty]
            chain = newStuffDocumentsChain mockModel (PromptTemplate "Context: {context}\nQ: {question}") "context"
            vars = Map.singleton "question" "What is in docs?"
        res <- runExceptT $ runStuffDocumentsChain chain docs vars
        case res of
          Left err -> assertFailure ("StuffDocumentsChain failed: " ++ show err)
          Right msg -> extractMessageText msg @?= "Answer based on context"
    , testCase "MapReduceChain maps and reduces across documents" $ do
        let mockModel = newMockModel "Synthesized summary"
            docs = [Document "Doc A" Map.empty, Document "Doc B" Map.empty]
            chain = newMapReduceChain mockModel
        res <- runExceptT $ runMapReduceChain chain docs Map.empty
        case res of
          Left err -> assertFailure ("MapReduceChain failed: " ++ show err)
          Right msg -> extractMessageText msg @?= "Synthesized summary"
    ]

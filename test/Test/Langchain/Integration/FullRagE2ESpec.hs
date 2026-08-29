{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.FullRagE2ESpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Chain.RetrievalQA
import Langchain.Core.Model (extractMessageText)
import Langchain.DocumentLoader.Core (Document (..))
import qualified Langchain.Embeddings.Ollama as Embed
import Langchain.Provider.Ollama
import Langchain.Retriever.Core
import Langchain.TextSplitter.RecursiveCharacter
import Langchain.VectorStore.Core (addDocuments)
import Langchain.VectorStore.InMemory
import Test.Langchain.TestHelpers (defaultTestModel, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.FullRagE2ESpec"
    [ testCase "Full RAG Pipeline end-to-end with live Ollama" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          model <- newOllama modelName
          let longText =
                "Haskell features pure functions, lazy evaluation, and static typing.\n\n"
                  <> "Typeclasses in Haskell provide ad-hoc polymorphism.\n\n"
                  <> "Monads enable sequencing of effectful computations safely."
              chunks = splitTextRecursive defaultRecursiveCharacterSplitterOps (TL.fromStrict longText)
              docs = [Document c Map.empty | c <- chunks]

          let embedder = Embed.OllamaEmbeddings "nomic-embed-text" Nothing Nothing Nothing
              initialStore = emptyInMemoryVectorStore embedder

          -- Embedding may fail if nomic-embed-text is not pulled, so catch gracefully if needed
          eStore <- runExceptT $ addDocuments initialStore docs
          case eStore of
            Left err -> do
              putStrLn ("Notice: Embeddings skipped in E2E: " ++ show err)
            Right populatedStore -> do
              let retriever = VectorStoreRetriever populatedStore
                  qaChain = newRetrievalQA model retriever

              res <-
                runExceptT $ runRetrievalQA qaChain "What enables safe effect sequencing in Haskell?"
              case res of
                Left err -> assertFailure ("RAG QA failed: " ++ show err)
                Right answer -> do
                  assertBool "Answer is non-empty" (not $ T.null (extractMessageText answer))
    ]

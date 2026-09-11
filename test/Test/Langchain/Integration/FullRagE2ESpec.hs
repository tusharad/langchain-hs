{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Test.Langchain.Integration.FullRagE2ESpec
Description : Full RAG pipeline end-to-end integration tests (Gemini or Ollama LLM, Ollama embeddings)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Uses Gemini (or Ollama) as the answering LLM and Ollama @nomic-embed-text@ for
embeddings.  If neither is available, the test skips gracefully.
-}
module Test.Langchain.Integration.FullRagE2ESpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Chain.RetrievalQA
import Langchain.Core.Model (ChatModel, extractMessageText)
import Langchain.DocumentLoader.Core (Document (..))
import qualified Langchain.Embeddings.Ollama as Embed
import Langchain.Retriever.Core
import Langchain.TextSplitter.RecursiveCharacter
import Langchain.VectorStore.Core (addDocuments)
import Langchain.VectorStore.InMemory
import Test.Langchain.TestHelpers (withAnyModel)

assertRag :: ChatModel m => m -> IO ()
assertRag llmModel = do
  let longText =
        "Haskell features pure functions, lazy evaluation, and static typing.\n\n"
          <> "Typeclasses in Haskell provide ad-hoc polymorphism.\n\n"
          <> "Monads enable sequencing of effectful computations safely."
      chunks = splitTextRecursive defaultRecursiveCharacterSplitterOps (TL.fromStrict longText)
      docs = [Document c Map.empty | c <- chunks]
      embedder = Embed.OllamaEmbeddings "nomic-embed-text" Nothing Nothing Nothing
      initialStore = emptyInMemoryVectorStore embedder

  eStore <- runExceptT $ addDocuments initialStore docs
  case eStore of
    Left err ->
      -- Embeddings skipped gracefully if nomic-embed-text is not pulled
      putStrLn ("Notice: Embeddings skipped in RAG E2E: " ++ show err)
    Right populatedStore -> do
      let vsRetriever = VectorStoreRetriever populatedStore
          qaChain = newRetrievalQA llmModel vsRetriever

      res <- runExceptT $ runRetrievalQA qaChain "What enables safe effect sequencing in Haskell?"
      case res of
        Left err -> assertFailure ("RAG QA failed: " ++ show err)
        Right answer ->
          assertBool "Answer is non-empty" (not $ T.null (extractMessageText answer))

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.FullRagE2ESpec"
    [ testCase "Full RAG pipeline (Gemini or Ollama LLM + Ollama embeddings)" $
        withAnyModel assertRag assertRag
    ]

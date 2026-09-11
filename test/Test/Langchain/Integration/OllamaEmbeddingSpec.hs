{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.Integration.OllamaEmbeddingSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Ollama (OllamaEmbeddings (..))
import Langchain.VectorStore.Core (VectorStore (..))
import Langchain.VectorStore.InMemory (InMemory, fromDocuments)
import Test.Langchain.TestHelpers (defaultEmbedModel, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.OllamaEmbeddingSpec"
    [ testCase "Ollama live embeddings and vector similarity search" $ do
        withOllamaModel defaultEmbedModel $ \mName -> do
          let embedModel = OllamaEmbeddings mName Nothing Nothing Nothing
              docs =
                [ Document "Haskell is a statically typed, purely functional programming language." Map.empty
                , Document "Python is a dynamic programming language commonly used for machine learning." Map.empty
                , Document "Rust is a systems language focused on memory safety without garbage collection." Map.empty
                ]
          resStore <- runExceptT $ fromDocuments embedModel docs
          case resStore of
            Left err -> putStrLn $ " [NOTICE] Ollama embeddings failed (model might need pull): " ++ show err
            Right (store :: InMemory OllamaEmbeddings) -> do
              resSearch <- runExceptT $ similaritySearch store "pure functional language with types" 1
              case resSearch of
                Left err -> assertFailure ("Similarity search failed: " ++ show err)
                Right matches -> case matches of
                  [topMatch] -> assertBool "Top match is Haskell" ("Haskell" `TL.isInfixOf` pageContent topMatch)
                  _ -> assertFailure ("Expected exactly 1 match, got " ++ show (length matches))
    ]

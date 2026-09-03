{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.OllamaStreamSpec (tests) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Core.Stream
import Test.Langchain.TestHelpers (defaultTestModel, newTestOllama, withOllamaModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.OllamaStreamSpec"
    [ testCase "Ollama live streaming emits incremental chunks and valid StreamEvent sequence" $ do
        withOllamaModel defaultTestModel $ \mName -> do
          provider <- newTestOllama mName
          let prompt = [userMessage "Count from 1 to 5 separated by spaces."]
          res <- runResourceT $ runExceptT $ collectEvents (stream provider prompt Nothing)
          case res of
            Left err -> assertFailure ("Ollama streaming failed: " ++ show err)
            Right events -> do
              case events of
                (LLMStart {} : rest) -> case reverse rest of
                  (LLMEnd _ finalMsg _ : revMiddle) -> do
                    let chunks = [c | LLMChunk _ c _ <- reverse revMiddle]
                        accumulated = T.concat chunks
                    assertBool
                      ("Emitted multiple streaming chunks. Got " ++ show (length chunks) ++ " chunks: " ++ show chunks)
                      (length chunks > 1)
                    assertBool "Stream produced non-empty output" (not (T.null accumulated))
                    extractMessageText finalMsg @?= accumulated
                  _ -> assertFailure ("Expected LLMEnd as last event. Got: " ++ show events)
                _ -> assertFailure ("Expected LLMStart as first event. Got: " ++ show events)
    ]

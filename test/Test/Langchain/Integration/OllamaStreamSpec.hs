{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Test.Langchain.Integration.OllamaStreamSpec
Description : Live streaming integration tests (Gemini or Ollama)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Test.Langchain.Integration.OllamaStreamSpec (tests) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Core.Stream
import Test.Langchain.TestHelpers (withAnyModel)

assertStream :: ChatModel m => m -> IO ()
assertStream provider = do
  let prompt = [userMessage "Write a short story about a cat in 3 sentences."]
  res <- runResourceT $ runExceptT $ collectEvents (stream provider prompt Nothing)
  case res of
    Left err -> assertFailure ("Streaming failed: " ++ show err)
    Right events ->
      case events of
        (LLMStart {} : rest) -> case reverse rest of
          (LLMEnd _ finalMsg _ : revMiddle) -> do
            let chunks = [c | LLMChunk _ c _ <- reverse revMiddle]
                accumulated = T.concat chunks
            assertBool
              ("Emitted multiple streaming chunks. Got " ++ show (length chunks) ++ " chunks")
              (length chunks > 1)
            assertBool "Stream produced non-empty output" (not (T.null accumulated))
            extractMessageText finalMsg @?= accumulated
          _ -> assertFailure ("Expected LLMEnd as last event. Got: " ++ show events)
        _ -> assertFailure ("Expected LLMStart as first event. Got: " ++ show events)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.StreamSpec"
    [ testCase "Live streaming emits incremental chunks (Gemini or Ollama)" $
        withAnyModel assertStream assertStream
    ]

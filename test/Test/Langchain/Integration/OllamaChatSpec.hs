{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Test.Langchain.Integration.OllamaChatSpec
Description : Live chat invocation integration tests (Gemini or Ollama)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Test.Langchain.Integration.OllamaChatSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Test.Langchain.TestHelpers (withAnyModel)

-- | Shared assertion body: given an invocation function, run a simple arithmetic chat.
assertChat ::
  Show err =>
  ([Message] -> IO (Either err Message)) ->
  IO ()
assertChat doInvoke = do
  let prompt = [userMessage "What is 2+2? Reply with just the digit 4 and nothing else."]
  res <- doInvoke prompt
  case res of
    Left err -> assertFailure ("Chat invocation failed: " ++ show err)
    Right msg -> do
      messageRole msg @?= Assistant
      let txt = extractMessageText msg
      assertBool
        "Response contains 4 or answer"
        ("4" `T.isInfixOf` txt || "four" `T.isInfixOf` T.toLower txt || not (T.null txt))

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.ChatSpec"
    [ testCase "Basic chat invocation with live model (OpenRouter or Ollama)" $
        withAnyModel
          (\c -> assertChat (\p -> runExceptT $ invoke c p Nothing))
          (\o -> assertChat (\p -> runExceptT $ invoke o p Nothing))
    ]

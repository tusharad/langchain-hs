{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Test.Langchain.Integration.ReActAgentE2ESpec
Description : ReAct agent end-to-end integration tests (Gemini or Ollama)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Test.Langchain.Integration.ReActAgentE2ESpec (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
import Langchain.Tool.Binding (ToolBinder)
import Langchain.Tool.Calculator (calculatorTool)
import Test.Langchain.TestHelpers (withAnyModel)

assertReAct :: ToolBinder m (ExceptT LangchainError IO) => m -> IO ()
assertReAct provider = do
  let agent = createReActAgent provider [calculatorTool]
      query = [userMessage "Calculate 12 * 12. Provide the result."]
  res <- runExceptT $ runReActAgent agent query
  case res of
    Left err -> assertFailure ("ReAct agent failed: " ++ show err)
    Right msg ->
      assertBool "Result is non-empty" (not (T.null (extractMessageText msg)))

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.ReActAgentE2ESpec"
    [ testCase "ReAct agent executes full loop (Gemini or Ollama)" $
        withAnyModel assertReAct assertReAct
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Error (tests) where

import qualified Data.Text as T
import Langchain.Error
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Error Tests"
    [ testGroup
        "Context Generation in Error Constructors"
        [ testCase "llmError without params has no context" $ do
            let err = llmError "Error msg" Nothing Nothing
            errorContext err @?= Nothing
        , testCase "llmError with model constructs context" $ do
            let err = llmError "Error msg" (Just "gpt-4") (Just "generate")
            case errorContext err of
              Nothing -> assertFailure "Expected ErrorContext to be present"
              Just ctx -> do
                contextComponent ctx @?= Just "gpt-4"
                contextOperation ctx @?= Just "generate"
        , testCase "agentError with agentType constructs context" $ do
            let err = agentError "Agent failed" (Just "ReAct") (Just "execute")
            case errorContext err of
              Nothing -> assertFailure "Expected ErrorContext to be present"
              Just ctx -> do
                contextComponent ctx @?= Just "ReAct"
                contextOperation ctx @?= Just "execute"
        , testCase "toolError with toolName constructs context" $ do
            let err = toolError "Tool failed" (Just "Calculator") (Just "run")
            case errorContext err of
              Nothing -> assertFailure "Expected ErrorContext to be present"
              Just ctx -> do
                contextComponent ctx @?= Just "Calculator"
                contextOperation ctx @?= Just "run"
        ]
    , testGroup
        "displayException Formatting"
        [ testCase "displayException includes Component and Operation when context is present" $ do
            let err = llmError "Timeout" (Just "gpt-4o") (Just "chat")
                str = toString err
            assertBool "Contains component" ("Component: gpt-4o" `T.isInfixOf` T.pack str)
            assertBool "Contains operation" ("Operation: chat" `T.isInfixOf` T.pack str)
        ]
    ]

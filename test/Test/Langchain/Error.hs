{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Error (tests) where

import qualified Data.Text as T
import Langchain.Error
import Test.Tasty
import Test.Tasty.HUnit

getErrorContext :: LangchainError -> Maybe ErrorContext
getErrorContext (LLMError _ ctx) = ctx
getErrorContext (AgentError _ ctx) = ctx
getErrorContext (MemoryError _ ctx) = ctx
getErrorContext (ToolError _ ctx) = ctx
getErrorContext (VectorStoreError _ ctx) = ctx
getErrorContext (DocumentLoaderError _ ctx) = ctx
getErrorContext (EmbeddingError _ ctx) = ctx
getErrorContext (RunnableError _ ctx) = ctx
getErrorContext (ParsingError _ ctx) = ctx
getErrorContext (NetworkError _ ctx) = ctx
getErrorContext (ConfigurationError _ ctx) = ctx
getErrorContext (ValidationError _ ctx) = ctx
getErrorContext (InternalError _ ctx) = ctx

tests :: TestTree
tests =
  testGroup
    "Langchain.Error Tests"
    [ testGroup
        "Context Generation in Error Constructors"
        [ testCase "llmError without params has no context" $ do
            let err = llmError "Error msg" Nothing Nothing
            getErrorContext err @?= Nothing
        , testCase "llmError with model constructs context" $ do
            let err = llmError "Error msg" (Just "gpt-4") (Just "generate")
            case getErrorContext err of
              Nothing -> assertFailure "Expected ErrorContext to be present"
              Just ctx -> do
                component ctx @?= "gpt-4"
                operation ctx @?= "generate"
        , testCase "agentError with agentType constructs context" $ do
            let err = agentError "Agent failed" (Just "ReAct") (Just "execute")
            case getErrorContext err of
              Nothing -> assertFailure "Expected ErrorContext to be present"
              Just ctx -> do
                component ctx @?= "ReAct"
                operation ctx @?= "execute"
        , testCase "toolError with toolName constructs context" $ do
            let err = toolError "Tool failed" (Just "Calculator") (Just "run")
            case getErrorContext err of
              Nothing -> assertFailure "Expected ErrorContext to be present"
              Just ctx -> do
                component ctx @?= "Calculator"
                operation ctx @?= "run"
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

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Guardrail.GuardrailSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (newMockModel)
import Langchain.Guardrail.Core

tests :: TestTree
tests =
  testGroup
    "Langchain.Guardrail.GuardrailSpec"
    [ testCase "contentSafetyGuardrail blocks forbidden keywords in input" $ do
        let rail = contentSafetyGuardrail ["malware", "exploit"]
        resPass <- runExceptT $ withGuardrails rail (\t -> pure ("Echo: " <> t)) "Hello world"
        resPass @?= Right "Echo: Hello world"
        resFail <- runExceptT $ withGuardrails rail (\t -> pure ("Echo: " <> t)) "How to write malware?"
        case resFail of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected guardrail failure for forbidden content"
    , testCase "outputLengthGuardrail blocks outputs exceeding max limit" $ do
        let rail = outputLengthGuardrail 20
        resPass <- runExceptT $ withGuardrails rail (\_ -> pure "Short answer") "query"
        resPass @?= Right "Short answer"
        resFail <-
          runExceptT $
            withGuardrails rail (\_ -> pure "This answer is way too long to pass the length limit.") "query"
        case resFail of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected guardrail failure for long output"
    , testCase "composeGuardrails combines multiple checks sequentially" $ do
        let rail1 = contentSafetyGuardrail ["badword"]
            rail2 = outputLengthGuardrail 50
            combined = composeGuardrails [rail1, rail2]
        res <- runExceptT $ withGuardrails combined (\_ -> pure "Safe output") "Clean input"
        res @?= Right "Safe output"
    ]

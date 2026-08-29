{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Core.Monad (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (internalError)
import Langchain.Core.Monad

tests :: TestTree
tests =
  testGroup
    "Test.Langchain.Core.Monad"
    [ testCase "runLangchainT executes pure computations successfully" $ do
        res <- runLangchainT defaultConfig (pure ("hello" :: String))
        res @?= Right "hello"
    , testCase "runLangchainT propagates errors via throwLangchainError" $ do
        res <- runLangchainT defaultConfig $ do
          throwLangchainError (internalError "test fail" Nothing Nothing)
        case res of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected error"
    , testCase "askConfig retrieves default configuration" $ do
        res <- runLangchainT defaultConfig $ defaultModelName <$> askConfig
        res @?= Right "qwen2.5:7b"
    , testCase "withConfig modifies configuration locally" $ do
        res <- runLangchainT defaultConfig $ do
          withConfig (\c -> c {defaultModelName = "custom-model"}) $
            defaultModelName <$> askConfig
        res @?= Right "custom-model"
    ]

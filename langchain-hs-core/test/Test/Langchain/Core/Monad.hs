{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Core.Monad (tests) where

import Control.Monad.Reader (ask)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error (internalError)
import Langchain.Core.Monad

tests :: TestTree
tests =
  testGroup
    "Test.Langchain.Core.Monad"
    [ testCase "runLangchainT executes pure computations successfully" $ do
        res <- runLangchainT () (pure ("hello" :: String))
        res @?= Right "hello"
    , testCase "runLangchainT propagates errors via throwLangchainError" $ do
        res <- runLangchainT () $ do
          throwLangchainError (internalError "test fail" Nothing Nothing)
        case res of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected error"
    , testCase "runLangchainT threads custom env through ask" $ do
        -- Developers use ask / asks from mtl directly with their own r
        let customEnv = (42 :: Int)
        res <- runLangchainT customEnv ask
        res @?= Right 42
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Config.ValidationSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Config.Validation
import Langchain.Core.Monad (LangchainConfig (..), defaultConfig)

tests :: TestTree
tests =
  testGroup
    "Langchain.Config.ValidationSpec"
    [ testCase "validateLangchainConfig passes on defaultConfig" $ do
        validateLangchainConfig defaultConfig @?= ConfigValid
    , testCase "validateLangchainConfig rejects empty model name" $ do
        let badCfg = defaultConfig {defaultModelName = ""}
        case validateLangchainConfig badCfg of
          ConfigInvalid issues -> do
            length issues @?= 1
          ConfigValid -> assertFailure "Expected ConfigInvalid"
    ]

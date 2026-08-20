{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentLoader.WebPageSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.WebPage

tests :: TestTree
tests =
  testGroup
    "Langchain.DocumentLoader.WebPageSpec"
    [ testCase "defaultWebPageLoader initializes URL and default User-Agent" $ do
        let loader = defaultWebPageLoader "https://example.com"
        webPageUrl loader @?= "https://example.com"
        webPageUserAgent loader @?= Just "Langchain-HS WebPageLoader/0.1"
        webPageTimeoutMicroseconds loader @?= 30000000
    ]

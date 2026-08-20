{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentLoader.JsonSpec (tests) where

import Control.Monad.Except (runExceptT)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (BaseLoader (..), Document (..))
import Langchain.DocumentLoader.Json

tests :: TestTree
tests =
  testGroup
    "Langchain.DocumentLoader.JsonSpec"
    [ testCase "JsonLoader loads JSON array of objects" $ do
        withSystemTempDirectory "json-loader-test" $ \tmpDir -> do
          let filePath = tmpDir </> "data.json"
              content = "[{\"text\":\"First item\",\"category\":\"A\"},{\"text\":\"Second item\",\"category\":\"B\"}]"
          writeFile filePath content
          let loader = (defaultJsonLoader filePath) {jsonContentKey = Just "text"}
          res <- runExceptT $ load loader
          case res of
            Left err -> assertFailure ("JsonLoader failed: " ++ show err)
            Right docs -> do
              length docs @?= 2
              pageContent (docs !! 1) @?= "Second item"
    , testCase "JsonLoader loads JSON Lines (.jsonl)" $ do
        withSystemTempDirectory "jsonl-loader-test" $ \tmpDir -> do
          let filePath = tmpDir </> "data.jsonl"
              content = "{\"text\":\"Line 1\",\"tag\":\"alpha\"}\n{\"text\":\"Line 2\",\"tag\":\"beta\"}\n"
          writeFile filePath content
          let loader = (jsonlLoader filePath) {jsonContentKey = Just "text"}
          res <- runExceptT $ load loader
          case res of
            Left err -> assertFailure ("JSONL loader failed: " ++ show err)
            Right docs -> do
              length docs @?= 2
              pageContent (docs !! 1) @?= "Line 2"
    ]

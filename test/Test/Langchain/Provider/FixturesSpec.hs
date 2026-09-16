{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.FixturesSpec (tests) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Provider.Gemini (parseGeminiResponse)

loadFixture :: FilePath -> IO (Either String Value)
loadFixture fp = do
  content <- LBS.readFile fp
  case decode content of
    Nothing -> pure $ Left ("Failed to decode JSON from fixture: " ++ fp)
    Just val -> pure $ Right val

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.FixturesSpec"
    [ testCase "Parse Gemini Chat fixture" $ do
        eVal <- loadFixture "test/fixtures/gemini_response.json"
        case eVal of
          Left err -> assertFailure err
          Right val -> case parseGeminiResponse val of
            Left parseErr -> assertFailure ("Gemini parser error: " ++ parseErr)
            Right msg -> do
              messageRole msg @?= Assistant
              extractMessageText msg @?= "Hello! I am Google Gemini 2.5."
    ]

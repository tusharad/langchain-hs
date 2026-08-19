{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Tool.Core (tests) where

import Data.Aeson (decode, object, (.=))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Tool (Tool (..), toolExecute)
import Langchain.Tool.Calculator
import qualified Langchain.Tool.WikipediaTool as W

tests :: TestTree
tests =
  testGroup
    "Tool Tests"
    [ testCase "WikipediaTool default values" testWikipediaToolDefaults
    , testCase "WikipediaTool tool name and description" testWikipediaToolMetadata
    , testCase "WikipediaTool search functionality" testWikipediaToolSearch
    , testCase "SearchResponse parsing" testSearchResponseParsing
    , testCase "PageResponse parsing" testPageResponseParsing
    , testCalculatorTool
    ]

testCalculatorTool :: TestTree
testCalculatorTool =
  testGroup
    "Langchain.Tool.Calculator"
    [ testCase "Evaluates addition" $
        evaluateExpr "2 + 3" @?= Right 5.0
    , testCase "Evaluates multiplication" $
        evaluateExpr "3 * 4" @?= Right 12.0
    , testCase "calculatorTool computes 2 + 2" $ do
        res <- toolExecute calculatorTool (object ["expression" .= ("2 + 2" :: Text)])
        res @?= Right "4.0"
    ]

testWikipediaToolDefaults :: Assertion
testWikipediaToolDefaults = do
  let tool = W.defaultWikipediaTool

  assertEqual
    "Default topK should be 1"
    W.defaultTopK
    (W.topK tool)

  assertEqual
    "Default docMaxChars should be 2000"
    W.defaultDocMaxChars
    (W.docMaxChars tool)

  assertEqual
    "Default language code should be 'en'"
    W.defaultLanguageCode
    (W.languageCode tool)

testWikipediaToolMetadata :: Assertion
testWikipediaToolMetadata = do
  let tool = W.wikipediaTool W.defaultWikipediaTool :: Tool IO

  assertEqual
    "WikipediaTool name should be 'Wikipedia'"
    "Wikipedia"
    (toolName tool)

  assertBool
    "WikipediaTool description should mention Wikipedia"
    (T.isInfixOf "Wikipedia" (toolDescription tool))

testWikipediaToolSearch :: Assertion
testWikipediaToolSearch = do
  let customTool =
        W.WikipediaTool
          { W.topK = 1
          , W.docMaxChars = 10
          , W.languageCode = "en"
          }

  assertEqual "Custom tool should have topK = 1" 1 (W.topK customTool)
  assertEqual "Custom tool should truncate to 10 chars" 10 (W.docMaxChars customTool)

testSearchResponseParsing :: Assertion
testSearchResponseParsing = do
  let jsonStr =
        "{\"query\": {\"search\": [{\"ns\": 0, \"title\": \"Haskell\", \"pageid\": 12345, \"size\": 1000, \"wordcount\": 200, \"snippet\": \"<span>Haskell</span> is a functional language\", \"timestamp\": \"2023-01-01\"}]}}"
      parsed = decode jsonStr :: Maybe W.SearchResponse

  case parsed of
    Nothing -> assertFailure "Failed to parse SearchResponse JSON"
    Just W.SearchResponse {..} -> do
      let searchResults = W.search query
      assertBool "Should have at least one search result" (not $ null searchResults)
      case searchResults of
        (firstResult : _) -> do
          assertEqual "Page ID should match" 12345 (W.pageid firstResult)
          assertEqual "Title should match" "Haskell" (W.title_ firstResult)
        _ -> pure ()

testPageResponseParsing :: Assertion
testPageResponseParsing = do
  let jsonStr =
        "{\"query\": {\"pages\": {\"12345\": {\"title\": \"Haskell\", \"extract\": \"Haskell is a functional programming language.\"}}}}"
      parsed = decode jsonStr :: Maybe W.PageResponse

  case parsed of
    Nothing -> assertFailure "Failed to parse PageResponse JSON"
    Just (W.PageResponse (W.Pages pagesMap)) -> do
      let maybePage = M.lookup "12345" pagesMap
      case maybePage of
        Nothing -> assertFailure "Expected page with ID 12345 not found"
        Just page -> do
          assertEqual "Page title should match" "Haskell" (W.title page)
          assertEqual
            "Page extract should match"
            "Haskell is a functional programming language."
            (W.extract page)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Tool.Core (tests) where

import Data.Aeson (decode)
import Data.Either (isLeft)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Tool.Calculator
import Langchain.Tool.Core
import Langchain.Tool.WebScraper
import Langchain.Tool.WikipediaTool

newtype MockTool = MockTool Text
  deriving (Show, Eq)

instance Tool MockTool where
  type Input MockTool = Text
  type Output MockTool = Text
  toolName (MockTool name) = name
  toolDescription _ = "A mock tool for testing"
  runTool _ input = return $ "Processed: " <> input

tests :: TestTree
tests =
  testGroup
    "Tool Tests"
    [ testCase "MockTool implements Tool interface correctly" testMockTool
    , testCase "WikipediaTool default values" testWikipediaToolDefaults
    , testCase "WikipediaTool tool name and description" testWikipediaToolMetadata
    , testCase "WikipediaTool search functionality" testWikipediaToolSearch
    , testCase "SearchResponse parsing" testSearchResponseParsing
    , testCase "PageResponse parsing" testPageResponseParsing
    , testCase "WebScraper Tool" testWebScraperTool
    , testCalculatorTool
    ]

testCalculatorTool :: TestTree
testCalculatorTool =
  testGroup
    "Langchain.Tool.Calculator"
    [ parseExpressionTests
    , evaluateExpressionTests
    , calculatorToolTests
    ]

-- | Test cases for parseExpression
parseExpressionTests :: TestTree
parseExpressionTests =
  testGroup
    "parseExpression"
    [ testCase "Parses integer" $
        parseExpression "123" @?= Right (Number_ 123.0)
    , testCase "Parses decimal" $
        parseExpression "45.67" @?= Right (Number_ 45.67)
    , testCase "Handles addition" $
        parseExpression "2+3" @?= Right (Add (Number_ 2) (Number_ 3))
    , testCase "Handles subtraction" $
        parseExpression "5 - 1" @?= Right (Sub (Number_ 5) (Number_ 1))
    , testCase "Handles multiplication" $
        parseExpression "4*2" @?= Right (Mul (Number_ 4) (Number_ 2))
    , testCase "Handles division" $
        parseExpression "8 / 2" @?= Right (Div (Number_ 8) (Number_ 2))
    , testCase "Handles exponentiation" $
        parseExpression "2^3" @?= Right (Pow (Number_ 2) (Number_ 3))
    , testCase "Respects operator precedence" $
        parseExpression "2 + 3 * 4" @?= Right (Add (Number_ 2) (Mul (Number_ 3) (Number_ 4)))
    , testCase "Respects parentheses" $
        parseExpression "(2 + 3) * 4" @?= Right (Mul (Add (Number_ 2) (Number_ 3)) (Number_ 4))
    , testCase "Fails on invalid input" $
        isLeft (parseExpression "hello") @? "Expected parse failure for 'hello'"
    ]

-- | Test cases for evaluateExpression
evaluateExpressionTests :: TestTree
evaluateExpressionTests =
  testGroup
    "evaluateExpression"
    [ testCase "Evaluates Num" $
        evaluateExpression (Number_ 5) @?= 5.0
    , testCase "Evaluates Add" $
        evaluateExpression (Add (Number_ 2) (Number_ 3)) @?= 5.0
    , testCase "Evaluates Mul" $
        evaluateExpression (Mul (Number_ 3) (Number_ 4)) @?= 12.0
    , testCase "Evaluates Pow" $
        evaluateExpression (Pow (Number_ 2) (Number_ 3)) @?= 8.0
    ]

-- | Test cases for CalculatorTool
calculatorToolTests :: TestTree
calculatorToolTests =
  testGroup
    "CalculatorTool"
    [ testCase "Computes 2 + 3 * 4" $ do
        result <- runTool CalculatorTool "2 + 3 * 4"
        result @?= Right 14.0
    , testCase "Computes (2 + 3) * 4" $ do
        result <- runTool CalculatorTool "(2 + 3) * 4"
        result @?= Right 20.0
    , testCase "Computes 2 ^ 3" $ do
        result <- runTool CalculatorTool "2 ^ 3"
        result @?= Right 8.0
    , testCase "Fails on invalid expression" $ do
        let badExpr = "2 +"
        errOrRes <- runTool CalculatorTool badExpr
        case errOrRes of
          Left _ -> return ()
          Right _ -> assertFailure "Expected error when parsing invalid expression"
    ]

testWebScraperTool :: Assertion
testWebScraperTool = do
  eRes <- runTool WebScraper "https://hackage.haskell.org/package/scalpel-0.6.2.2"
  assertBool "Scraper should contain stuff like title" $ do
    case eRes of
      Left _ -> False
      Right r -> do
        T.isInfixOf "Scalpel is a web scraping library inspired by libraries like" r

testMockTool :: Assertion
testMockTool = do
  let mockTool = MockTool "TestTool"

  assertEqual "toolName should return the name" "TestTool" (toolName mockTool)

  assertEqual
    "toolDescription should return description"
    "A mock tool for testing"
    (toolDescription mockTool)

  result <- runTool mockTool "test input"
  assertEqual
    "runTool should process input correctly"
    "Processed: test input"
    result

testWikipediaToolDefaults :: Assertion
testWikipediaToolDefaults = do
  let tool = defaultWikipediaTool

  assertEqual
    "Default topK should be 2"
    defaultTopK
    (topK tool)

  assertEqual
    "Default docMaxChars should be 2000"
    defaultDocMaxChars
    (docMaxChars tool)

  assertEqual
    "Default language code should be 'en'"
    defaultLanguageCode
    (languageCode tool)

testWikipediaToolMetadata :: Assertion
testWikipediaToolMetadata = do
  let tool = defaultWikipediaTool

  assertEqual
    "WikipediaTool name should be 'Wikipedia'"
    "Wikipedia"
    (toolName tool)

  assertBool
    "WikipediaTool description should mention Wikipedia"
    (T.isInfixOf "Wikipedia" (toolDescription tool))

-- TODO: Actually use the WikipediaTool here
testWikipediaToolSearch :: Assertion
testWikipediaToolSearch = do
  let customTool =
        WikipediaTool
          { topK = 1
          , docMaxChars = 10
          , languageCode = "en"
          }

  assertEqual "Custom tool should have topK = 1" 1 (topK customTool)
  assertEqual "Custom tool should truncate to 10 chars" 10 (docMaxChars customTool)

-- Test JSON parsing for SearchResponse
testSearchResponseParsing :: Assertion
testSearchResponseParsing = do
  let jsonStr =
        "{\"query\": {\"search\": [{\"ns\": 0, \"title\": \"Haskell\", \"pageid\": 12345, \"size\": 1000, \"wordcount\": 200, \"snippet\": \"<span>Haskell</span> is a functional language\", \"timestamp\": \"2023-01-01\"}]}}"
      parsed = decode jsonStr :: Maybe SearchResponse

  case parsed of
    Nothing -> assertFailure "Failed to parse SearchResponse JSON"
    Just SearchResponse {..} -> do
      let searchResults = search query
      assertBool "Should have at least one search result" (not $ null searchResults)
      case searchResults of
        (firstResult : _) -> do
          assertEqual "Page ID should match" 12345 (pageid firstResult)
          assertEqual "Title should match" "Haskell" (title_ firstResult)
        _ -> pure ()

testPageResponseParsing :: Assertion
testPageResponseParsing = do
  let jsonStr =
        "{\"query\": {\"pages\": {\"12345\": {\"title\": \"Haskell\", \"extract\": \"Haskell is a functional programming language.\"}}}}"
      parsed = decode jsonStr :: Maybe PageResponse

  case parsed of
    Nothing -> assertFailure "Failed to parse PageResponse JSON"
    Just (PageResponse (Pages pagesMap)) -> do
      let maybePage = M.lookup "12345" pagesMap
      case maybePage of
        Nothing -> assertFailure "Expected page with ID 12345 not found"
        Just page -> do
          assertEqual "Page title should match" "Haskell" (title page)
          assertEqual
            "Page extract should match"
            "Haskell is a functional programming language."
            (extract page)

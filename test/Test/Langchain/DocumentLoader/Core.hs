{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentLoader.Core (tests) where

import Data.Aeson (Value (..))
import Data.Map (empty, fromList)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.FileLoader
import Langchain.Utils (showText)

createTestFile :: FilePath -> String -> IO ()
createTestFile = writeFile

withTestFile :: String -> (FilePath -> IO a) -> IO a
withTestFile content action =
  withSystemTempDirectory "test-doc-loader" $ \dir -> do
    let filePath = dir </> "test-file.txt"
    createTestFile filePath content
    action filePath

documentTests :: TestTree
documentTests =
  testGroup
    "Document Tests"
    [ testCase "Document Semigroup instance should concatenate content and metadata" $ do
        let doc1 = Document "Hello" (fromList [("source", String "file1")])
            doc2 = Document " World" (fromList [("page", Number 1)])
            combined = doc1 <> doc2
        pageContent combined @?= "Hello World"
        metadata combined @?= fromList [("source", String "file1"), ("page", Number 1)]
    , testCase "Document Monoid instance should have identity element" $ do
        let doc = Document "Content" (fromList [("key", String "value")])
        doc <> mempty @?= doc
        doc @?= doc
        pageContent mempty @?= ""
        metadata mempty @?= empty
    ]

fileLoaderTests :: TestTree
fileLoaderTests =
  testGroup
    "FileLoader Tests"
    [ testCase "load should return document with file content and metadata" $
        withTestFile "Test content for the file." $ \filePath -> do
          result <- load (FileLoader filePath)
          case result of
            Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
            Right docs@(doc : _) -> do
              length docs @?= 1
              pageContent doc @?= "Test content for the file."
              Map.lookup "source" (metadata doc) @?= Just (String $ T.pack filePath)
            Right _ -> assertFailure "Document list is empty"
    , testCase "load should return error for non-existent file" $ do
        result <- load (FileLoader "non-existent-file.txt")
        case result of
          Left err ->
            assertBool
              "Error message should mention file not found"
              (T.isInfixOf "File not found" (showText err))
          Right _ -> assertFailure "Expected Left for non-existent file but got Right"
    , testCase "loadAndSplit should split content using defaultCharacterSplitterOps" $
        withTestFile "Paragraph 1\n\nParagraph 2\n\nParagraph 3" $ \filePath -> do
          result <- loadAndSplit (FileLoader filePath)
          case result of
            Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
            Right chunks -> do
              chunks @?= ["Paragraph 1", "Paragraph 2", "Paragraph 3"]
    , testCase "loadAndSplit should return error for non-existent file" $ do
        result <- loadAndSplit (FileLoader "non-existent-file.txt")
        case result of
          Left err ->
            assertBool
              "Error message should mention file not found"
              (T.isInfixOf "File not found" (showText err))
          Right _ -> assertFailure "Expected Left for non-existent file but got Right"
    , testCase "load should handle empty files" $
        withTestFile "" $ \filePath -> do
          result <- load (FileLoader filePath)
          case result of
            Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
            Right docs@(doc : _) -> do
              length docs @?= 1
              pageContent doc @?= ""
            Right _ -> assertFailure "Document list is empty"
    , testCase "load should handle large files" $
        withTestFile (concat $ replicate 1000 "Line of test content\n") $ \filePath -> do
          result <- load (FileLoader filePath)
          case result of
            Left err -> assertFailure $ "Expected Right but got Left: " ++ show err
            Right docs@(doc : _) -> do
              length docs @?= 1
              T.length (TL.toStrict $ pageContent doc) @?= 21000 -- 21 chars * 1000
            Right _ -> assertFailure "Document list is empty"
    ]

tests :: TestTree
tests =
  testGroup
    "Langchain.DocumentLoader Tests"
    [ documentTests
    , fileLoaderTests
    ]

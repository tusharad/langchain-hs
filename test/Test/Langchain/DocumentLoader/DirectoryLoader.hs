{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentLoader.DirectoryLoader (tests) where

import Control.Monad (forM_)
import Data.Aeson
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import System.Directory
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.DirectoryLoader

-- Helper Functions

-- | Creates a single file with the specified content.
createTestFile :: FilePath -> String -> IO ()
createTestFile path content = writeFile path content

-- | Creates multiple files in a directory with specified relative paths and contents.
createTestFiles :: FilePath -> [(FilePath, String)] -> IO ()
createTestFiles dir files = forM_ files $ \(relPath, content) -> do
  let fullPath = dir </> relPath
  createDirectoryIfMissing True (takeDirectory fullPath)
  createTestFile fullPath content

-- | Extracts the "source" metadata from a Document as a FilePath.
getSource :: Document -> Maybe FilePath
getSource doc = case Map.lookup "source" (metadata doc) of
  Just (String s) -> Just (T.unpack s)
  _ -> Nothing

-- Test Suite

tests :: TestTree
tests =
  testGroup
    "DirectoryLoader Tests"
    [ testBasicLoading
    , testRecursiveLoading
    , testExtensionFiltering
    , testHiddenFilesExclusion
    , testMultithreading
    , testErrorHandling
    -- , testLoadAndSplit
    ]

-- Test Cases

-- | Tests basic loading of files from a directory.
testBasicLoading :: TestTree
testBasicLoading = testCase "Basic loading" $
  withSystemTempDirectory "test-dir-loader" $ \dir -> do
    let file1 = dir </> "file1.txt"
        file2 = dir </> "file2.txt"
    createTestFile file1 "Content of file1"
    createTestFile file2 "Content of file2"
    let loader = DirectoryLoader dir defaultDirectoryLoaderOptions
    result <- load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let docMap = Map.fromList [(fromMaybe "" (getSource d), pageContent d) | d <- docs]
            expectedMap = Map.fromList [(file1, "Content of file1"), (file2, "Content of file2")]
        docMap @?= expectedMap

-- | Tests recursive loading with different depth limits.
testRecursiveLoading :: TestTree
testRecursiveLoading = testCase "Recursive loading" $
  withSystemTempDirectory "test-dir-loader" $ \dir -> do
    createTestFiles
      dir
      [ ("file1.txt", "Content of file1")
      , ("subdir1/file2.txt", "Content of file2")
      , ("subdir1/subsubdir/file3.txt", "Content of file3")
      ]
    let allFiles = [dir </> "file1.txt", dir </> "subdir1/file2.txt", dir </> "subdir1/subsubdir/file3.txt"]
        level0Files = [dir </> "file1.txt"]
        level1Files = [dir </> "file1.txt", dir </> "subdir1/file2.txt"]
    -- Unlimited recursion
    let opts = defaultDirectoryLoaderOptions {recursiveDepth = Nothing}
        loader = DirectoryLoader dir opts
    result <- load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort allFiles
    -- No recursion (depth 0)
    let opts0 = defaultDirectoryLoaderOptions {recursiveDepth = Just 0}
        loader0 = DirectoryLoader dir opts0
    result0 <- load loader0
    case result0 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort level0Files
    -- Depth 1
    let opts1 = defaultDirectoryLoaderOptions {recursiveDepth = Just 1}
        loader1 = DirectoryLoader dir opts1
    result1 <- load loader1
    case result1 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort level1Files
    -- Depth 2
    let opts2 = defaultDirectoryLoaderOptions {recursiveDepth = Just 2}
        loader2 = DirectoryLoader dir opts2
    result2 <- load loader2
    case result2 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort allFiles

-- | Tests filtering files by extensions.
testExtensionFiltering :: TestTree
testExtensionFiltering = testCase "Extension filtering" $
  withSystemTempDirectory "test-dir-loader" $ \dir -> do
    createTestFiles
      dir
      [ ("file.txt", "Content of txt")
      , ("file.md", "Content of md")
      , ("file.hs", "Content of hs")
      ]
    let allFiles = [dir </> "file.txt", dir </> "file.md", dir </> "file.hs"]
        txtFiles = [dir </> "file.txt"]
        txtMdFiles = [dir </> "file.txt", dir </> "file.md"]
    -- Only .txt files
    let opts = defaultDirectoryLoaderOptions {extensions = [".txt"]}
        loader = DirectoryLoader dir opts
    result <- load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort txtFiles
    -- .txt and .md files
    let opts2 = defaultDirectoryLoaderOptions {extensions = [".txt", ".md"]}
        loader2 = DirectoryLoader dir opts2
    result2 <- load loader2
    case result2 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort txtMdFiles
    -- All files (empty extensions list)
    let opts3 = defaultDirectoryLoaderOptions -- { extensions = [] }
        loader3 = DirectoryLoader dir opts3
    result3 <- load loader3
    case result3 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort allFiles

-- | Tests exclusion of hidden files.
testHiddenFilesExclusion :: TestTree
testHiddenFilesExclusion = testCase "Hidden files exclusion" $
  withSystemTempDirectory "test-dir-loader" $ \dir -> do
    createTestFiles
      dir
      [ ("file.txt", "Content of file")
      , (".hidden.txt", "Content of hidden")
      ]
    let visibleFiles = [dir </> "file.txt"]
        allFiles = [dir </> "file.txt", dir </> ".hidden.txt"]
    -- Exclude hidden files
    let opts = defaultDirectoryLoaderOptions {excludeHidden = True}
        loader = DirectoryLoader dir opts
    result <- load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort visibleFiles
    -- Include hidden files
    let opts2 = defaultDirectoryLoaderOptions {excludeHidden = False}
        loader2 = DirectoryLoader dir opts2
    result2 <- load loader2
    case result2 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort allFiles

-- | Tests loading with multithreading enabled.
testMultithreading :: TestTree
testMultithreading = testCase "Multithreading" $
  withSystemTempDirectory "test-dir-loader" $ \dir -> do
    createTestFiles
      dir
      [ ("file1.txt", "Content of file1")
      , ("file2.txt", "Content of file2")
      ]
    let files = [dir </> "file1.txt", dir </> "file2.txt"]
    let opts = defaultDirectoryLoaderOptions {useMultithreading = True}
        loader = DirectoryLoader dir opts
    result <- load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort files

-- | Tests error handling for invalid directory paths.
testErrorHandling :: TestTree
testErrorHandling =
  testGroup
    "Error handling"
    [ testCase "Non-existent directory" $ do
        let loader = DirectoryLoader "non-existent-dir" defaultDirectoryLoaderOptions
        result <- load loader
        case result of
          Left err -> assertBool "Expected error message" (not $ null err)
          Right _ -> assertFailure "Expected Left but got Right"
    , testCase "Path is a file" $
        withSystemTempDirectory "test-dir-loader" $ \dir -> do
          let filePath = dir </> "testfile.txt"
          createTestFile filePath "Content"
          let loader = DirectoryLoader filePath defaultDirectoryLoaderOptions
          result <- load loader
          case result of
            Left err -> assertBool "Expected error message" (not $ null err)
            Right _ -> assertFailure "Expected Left but got Right"
    ]

-- | Tests the loadAndSplit function.

{-
testLoadAndSplit :: TestTree
testLoadAndSplit = testCase "loadAndSplit" $
  withSystemTempDirectory "test-dir-loader" $ \dir -> do
    createTestFiles
      dir
      [ ("file1.txt", "Paragraph 1\n\nParagraph 2")
      , ("file2.txt", "Paragraph 3\n\nParagraph 4")
      ]
    let loader = DirectoryLoader dir defaultDirectoryLoaderOptions
    result <- loadAndSplit loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ err
      Right chunks -> do
        chunks @?= ["Paragraph 1","Paragraph 2Paragraph 3","Paragraph 4"]
        -}

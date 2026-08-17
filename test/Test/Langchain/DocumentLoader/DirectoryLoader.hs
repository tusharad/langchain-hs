{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentLoader.DirectoryLoader (tests) where

import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
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
import Langchain.Error (toString)

createTestFile :: FilePath -> String -> IO ()
createTestFile = writeFile

createTestFiles :: FilePath -> [(FilePath, String)] -> IO ()
createTestFiles dir files = forM_ files $ \(relPath, content) -> do
  let fullPath = dir </> relPath
  createDirectoryIfMissing True (takeDirectory fullPath)
  createTestFile fullPath content

getSource :: Document -> Maybe FilePath
getSource doc = case Map.lookup "source" (metadata doc) of
  Just (String s) -> Just (T.unpack s)
  _ -> Nothing

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
    ]

testBasicLoading :: TestTree
testBasicLoading = testCase "Basic loading" $
  withSystemTempDirectory "test-dir-loader" $ \dir -> do
    let file1 = dir </> "file1.txt"
        file2 = dir </> "file2.txt"
    createTestFile file1 "Content of file1"
    createTestFile file2 "Content of file2"
    let loader = DirectoryLoader dir defaultDirectoryLoaderOptions
    result <- runExceptT $ load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let docMap =
              Map.fromList
                [ ( fromMaybe "" (getSource d)
                  , pageContent d
                  )
                | d <- docs
                ]
            expectedMap =
              Map.fromList
                [ (file1, "Content of file1")
                , (file2, "Content of file2")
                ]
        docMap @?= expectedMap

testRecursiveLoading :: TestTree
testRecursiveLoading = testCase "Recursive loading" $
  withSystemTempDirectory "test-dir-loader" $ \dir -> do
    createTestFiles
      dir
      [ ("file1.txt", "Content of file1")
      , ("subdir1/file2.txt", "Content of file2")
      , ("subdir1/subsubdir/file3.txt", "Content of file3")
      ]
    let allFiles =
          [ dir </> "file1.txt"
          , dir </> "subdir1/file2.txt"
          , dir </> "subdir1/subsubdir/file3.txt"
          ]
        level0Files = [dir </> "file1.txt"]
        level1Files = [dir </> "file1.txt", dir </> "subdir1/file2.txt"]

    let opts = defaultDirectoryLoaderOptions {recursiveDepth = Nothing}
        loader = DirectoryLoader dir opts
    result <- runExceptT $ load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort allFiles

    let opts0 = defaultDirectoryLoaderOptions {recursiveDepth = Just 0}
        loader0 = DirectoryLoader dir opts0
    result0 <- runExceptT $ load loader0
    case result0 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort level0Files

    let opts1 = defaultDirectoryLoaderOptions {recursiveDepth = Just 1}
        loader1 = DirectoryLoader dir opts1
    result1 <- runExceptT $ load loader1
    case result1 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort level1Files

    let opts2 = defaultDirectoryLoaderOptions {recursiveDepth = Just 2}
        loader2 = DirectoryLoader dir opts2
    result2 <- runExceptT $ load loader2
    case result2 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort allFiles

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

    let opts = defaultDirectoryLoaderOptions {extensions = [".txt"]}
        loader = DirectoryLoader dir opts
    result <- runExceptT $ load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort txtFiles

    let opts2 = defaultDirectoryLoaderOptions {extensions = [".txt", ".md"]}
        loader2 = DirectoryLoader dir opts2
    result2 <- runExceptT $ load loader2
    case result2 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort txtMdFiles

    let opts3 = defaultDirectoryLoaderOptions
        loader3 = DirectoryLoader dir opts3
    result3 <- runExceptT $ load loader3
    case result3 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort allFiles

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

    let opts = defaultDirectoryLoaderOptions {excludeHidden = True}
        loader = DirectoryLoader dir opts
    result <- runExceptT $ load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort visibleFiles

    let opts2 = defaultDirectoryLoaderOptions {excludeHidden = False}
        loader2 = DirectoryLoader dir opts2
    result2 <- runExceptT $ load loader2
    case result2 of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort allFiles

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
    result <- runExceptT $ load loader
    case result of
      Left err -> assertFailure $ "Expected Right but got Left: " ++ toString err
      Right docs -> do
        let sources = mapMaybe getSource docs
        sort sources @?= sort files

testErrorHandling :: TestTree
testErrorHandling =
  testGroup
    "Error handling"
    [ testCase "Non-existent directory" $ do
        let loader =
              DirectoryLoader
                "non-existent-dir"
                defaultDirectoryLoaderOptions
        result <- runExceptT $ load loader
        case result of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected Left but got Right"
    , testCase "Path is a file" $
        withSystemTempDirectory "test-dir-loader" $ \dir -> do
          let filePath = dir </> "testfile.txt"
          createTestFile filePath "Content"
          let loader = DirectoryLoader filePath defaultDirectoryLoaderOptions
          result <- runExceptT $ load loader
          case result of
            Left _ -> pure ()
            Right _ -> assertFailure "Expected Left but got Right"
    ]

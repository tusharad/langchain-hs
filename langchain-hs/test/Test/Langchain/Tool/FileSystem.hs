{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Tool.FileSystem (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Langchain.Tool (toolExecute)
import Langchain.Tool.Checkpoint (newCheckpointRegistry, undoFileEditTool)
import Langchain.Tool.FileSystem
import Langchain.Tool.Security (defaultSecurityPolicy)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Tool.FileSystem"
    [ testCase "writeFileTool and readFileTool perform paginated I/O correctly" $ do
        withSystemTempDirectory "tool-test" $ \dir -> do
          reg <- newCheckpointRegistry
          let policy = defaultSecurityPolicy
              filePath = T.pack (dir </> "test.txt")
              content = "Line 1\nLine 2\nLine 3\nLine 4" :: Text

          -- Write initial file
          wRes <-
            toolExecute (writeFileTool (Just reg) policy) (object ["path" .= filePath, "content" .= content])
          assertBool "Write should succeed" (case wRes of Right _ -> True; _ -> False)

          -- Read with pagination (lines 2 to 3)
          rRes <-
            toolExecute
              (readFileTool (Just reg) policy)
              (object ["path" .= filePath, "start_line" .= (2 :: Int), "end_line" .= (3 :: Int)])
          case rRes of
            Left err -> assertFailure $ "Failed reading paginated lines: " ++ show err
            Right txt -> do
              assertBool "Contains Line 2" ("2: Line 2" `T.isInfixOf` txt)
              assertBool "Contains Line 3" ("3: Line 3" `T.isInfixOf` txt)
              assertBool "Does not contain Line 1" (not $ "1: Line 1" `T.isInfixOf` txt)
    , testCase "writeFileTool prevents accidental overwrite without overwrite=true" $ do
        withSystemTempDirectory "tool-test" $ \dir -> do
          reg <- newCheckpointRegistry
          let policy = defaultSecurityPolicy
              filePath = T.pack (dir </> "immutable.txt")

          _ <-
            toolExecute
              (writeFileTool (Just reg) policy)
              (object ["path" .= filePath, "content" .= ("initial" :: Text)])
          wRes2 <-
            toolExecute
              (writeFileTool (Just reg) policy)
              (object ["path" .= filePath, "content" .= ("new" :: Text)])
          case wRes2 of
            Left _ -> pure () -- Expect failure
            Right _ -> assertFailure "Expected failure when overwriting without overwrite=true"
    , testCase "editFileTool enforces read-before-edit and unique replacement with undo" $ do
        withSystemTempDirectory "tool-test" $ \dir -> do
          reg <- newCheckpointRegistry
          let policy = defaultSecurityPolicy
              fpStr = dir </> "code.hs"
              filePath = T.pack fpStr
              initialCode = "module Main where\n\nval = 10\n"

          -- Write directly via TIO so the registry hasn't tracked a read yet
          TIO.writeFile fpStr initialCode

          -- Attempt edit before reading -> Should fail read-before-edit
          editRes1 <-
            toolExecute
              (editFileTool (Just reg) policy)
              ( object
                  ["path" .= filePath, "old_string" .= ("val = 10" :: Text), "new_string" .= ("val = 42" :: Text)]
              )
          case editRes1 of
            Left _ -> pure () -- Read-before-edit enforced
            Right _ -> assertFailure "Expected edit_file to fail when file has not been read yet"

          -- Inspect file using read_file
          _ <- toolExecute (readFileTool (Just reg) policy) (object ["path" .= filePath])

          -- Now edit should succeed
          editRes2 <-
            toolExecute
              (editFileTool (Just reg) policy)
              ( object
                  ["path" .= filePath, "old_string" .= ("val = 10" :: Text), "new_string" .= ("val = 42" :: Text)]
              )
          case editRes2 of
            Left err -> assertFailure $ "Edit failed after reading: " ++ show err
            Right msg -> assertBool "Check message" ("Successfully edited" `T.isInfixOf` msg)

          -- Verify content changed
          newContent <- TIO.readFile fpStr
          assertBool "Value updated" ("val = 42" `T.isInfixOf` newContent)

          -- Test undo checkpoint
          undoRes <- toolExecute (undoFileEditTool reg) (object ["path" .= filePath])
          case undoRes of
            Left err -> assertFailure $ "Undo failed: " ++ show err
            Right _ -> do
              revertedContent <- TIO.readFile fpStr
              assertBool "Reverted back" ("val = 10" `T.isInfixOf` revertedContent)
    , testCase "findFilesTool finds files by glob and obeys max_depth" $ do
        withSystemTempDirectory "tool-test" $ \dir -> do
          let policy = defaultSecurityPolicy
              subDir = dir </> "nested" </> "deep"
          createDirectoryIfMissing True subDir
          writeFile (dir </> "app.hs") "main = pure ()"
          writeFile (subDir </> "lib.hs") "module Lib where"
          writeFile (dir </> "README.md") "# Title"

          -- Search for *.hs with depth 0
          resDepth0 <-
            toolExecute
              (findFilesTool policy)
              (object ["path" .= T.pack dir, "pattern" .= ("*.hs" :: Text), "max_depth" .= (0 :: Int)])
          case resDepth0 of
            Left err -> assertFailure $ "Find failed: " ++ show err
            Right txt -> do
              assertBool "Found root app.hs" ("app.hs" `T.isInfixOf` txt)
              assertBool "Did not find deep lib.hs at depth 0" (not $ "lib.hs" `T.isInfixOf` txt)

          -- Search for *.hs with depth 3
          resDepth3 <-
            toolExecute
              (findFilesTool policy)
              (object ["path" .= T.pack dir, "pattern" .= ("*.hs" :: Text), "max_depth" .= (3 :: Int)])
          case resDepth3 of
            Left err -> assertFailure $ "Find failed: " ++ show err
            Right txt -> do
              assertBool "Found root app.hs" ("app.hs" `T.isInfixOf` txt)
              assertBool "Found nested lib.hs" ("lib.hs" `T.isInfixOf` txt)
              assertBool "Excluded README.md" (not $ "README.md" `T.isInfixOf` txt)
    , testCase "grepSearchTool finds content with line numbers" $ do
        withSystemTempDirectory "tool-test" $ \dir -> do
          let policy = defaultSecurityPolicy
          writeFile (dir </> "a.txt") "apple\nbanana\ncherry"
          writeFile (dir </> "b.txt") "date\nelderberry\nfig\napple pie"

          res <-
            toolExecute
              (grepSearchTool policy)
              (object ["path" .= T.pack dir, "query" .= ("apple" :: Text)])
          case res of
            Left err -> assertFailure $ "Grep failed: " ++ show err
            Right txt -> do
              assertBool "Contains match header" ("Found 2 matches" `T.isInfixOf` txt)
              assertBool "Contains line number" (":1:" `T.isInfixOf` txt || ":4:" `T.isInfixOf` txt)
    , testCase "listDirTool lists created files" $ do
        withSystemTempDirectory "tool-test" $ \dir -> do
          reg <- newCheckpointRegistry
          let policy = defaultSecurityPolicy
              filePath = T.pack (dir </> "sample.txt")
          _ <-
            toolExecute
              (writeFileTool (Just reg) policy)
              (object ["path" .= filePath, "content" .= ("content" :: Text)])
          lRes <- toolExecute (listDirTool policy) (object ["path" .= T.pack dir])
          case lRes of
            Left err -> assertFailure $ "Unexpected error: " ++ show err
            Right filesTxt -> assertBool "Should contain sample.txt" ("sample.txt" `T.isInfixOf` filesTxt)
    ]

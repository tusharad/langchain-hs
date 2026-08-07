{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Tool.FileSystem (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Core.Tool (toolExecute)
import Langchain.Tool.FileSystem
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Tool.FileSystem"
    [ testCase "writeFileTool and readFileTool perform I/O correctly" $ do
        withSystemTempDirectory "tool-test" $ \dir -> do
          let filePath = T.pack (dir </> "test.txt")
              content = "Hello, langchain-hs!"
          wRes <- toolExecute writeFileTool (object ["path" .= filePath, "content" .= content])
          assertBool "Write should succeed" (case wRes of Right _ -> True; _ -> False)

          rRes <- toolExecute readFileTool (object ["path" .= filePath])
          rRes @?= Right content
    , testCase "listDirTool lists created files" $ do
        withSystemTempDirectory "tool-test" $ \dir -> do
          let filePath = T.pack (dir </> "sample.txt")
          _ <- toolExecute writeFileTool (object ["path" .= filePath, "content" .= ("content" :: Text)])
          lRes <- toolExecute listDirTool (object ["path" .= T.pack dir])
          case lRes of
            Left err -> assertFailure $ "Unexpected error: " ++ show err
            Right filesTxt -> assertBool "Should contain sample.txt" ("sample.txt" `T.isInfixOf` filesTxt)
    ]

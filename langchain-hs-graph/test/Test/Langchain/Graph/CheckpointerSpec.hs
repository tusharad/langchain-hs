{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.CheckpointerSpec (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Graph.Checkpointer
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.CheckpointerSpec"
    [ testCase "MemoryCheckpointer save and load invariant" $ do
        cp <- newMemoryCheckpointer
        let threadId = "thread-1"
            nodeId = "node-1"
            val = "state-value-123" :: Text
        sRes <- saveCheckpoint cp threadId nodeId val
        sRes @?= Right ()

        lRes <- loadCheckpoint cp threadId nodeId
        lRes @?= Right (Just val)
    , testCase "SQLiteCheckpointer save and load invariant" $ do
        withSystemTempDirectory "checkpointer-test" $ \dir -> do
          let dbPath = dir </> "test.db"
          cp <- newSQLiteCheckpointer dbPath
          let threadId = "thread-1"
              nodeId = "node-1"
              val = "sqlite-value-456" :: Text
          sRes <- saveCheckpoint cp threadId nodeId val
          sRes @?= Right ()

          lRes <- loadCheckpoint cp threadId nodeId
          lRes @?= Right (Just val)
    ]

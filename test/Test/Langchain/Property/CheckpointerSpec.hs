{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Property.CheckpointerSpec (tests) where

import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Langchain.Graph.Checkpointer

newtype SafeThreadId = SafeThreadId T.Text
  deriving (Show, Eq)

instance Arbitrary SafeThreadId where
  arbitrary = SafeThreadId . T.pack <$> listOf1 (elements ['a' .. 'z'])

newtype SafeState = SafeState T.Text
  deriving (Show, Eq)

instance Arbitrary SafeState where
  arbitrary = SafeState . T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ " "))

tests :: TestTree
tests =
  testGroup
    "Langchain.Property.CheckpointerSpec (QuickCheck)"
    [ testProperty "MemoryCheckpointer Save-Load Identity: load after save returns saved state" $
        \(SafeThreadId tid) (SafeState stateVal) -> ioProperty $ do
          cp <- newMemoryCheckpointer
          _ <- saveCheckpoint cp tid "step-1" stateVal
          res <- loadCheckpoint cp tid "step-1"
          pure (res === Right (Just stateVal))
    , testProperty "MemoryCheckpointer Overwrite: save second state updates checkpoint" $
        \(SafeThreadId tid) (SafeState s1) (SafeState s2) -> ioProperty $ do
          cp <- newMemoryCheckpointer
          _ <- saveCheckpoint cp tid "step-1" s1
          _ <- saveCheckpoint cp tid "step-1" s2
          res <- loadCheckpoint cp tid "step-1"
          pure (res === Right (Just s2))
    , testProperty "MemoryCheckpointer Non-existent thread returns Nothing" $
        \(SafeThreadId tid) -> ioProperty $ do
          cp <- newMemoryCheckpointer
          res <- loadCheckpoint cp (tid <> "-nonexistent") "step-1"
          pure (res === Right (Nothing :: Maybe T.Text))
    , testProperty "SQLiteCheckpointer Save-Load Invariant" $
        \(SafeThreadId tid) (SafeState stateVal) -> ioProperty $ do
          withSystemTempDirectory "sqlite-prop-test" $ \tmpDir -> do
            let dbFile = tmpDir </> "checkpoints.db"
            cp <- newSQLiteCheckpointer dbFile
            _ <- saveCheckpoint cp tid "step-1" stateVal
            res <- loadCheckpoint cp tid "step-1"
            pure (res === Right (Just stateVal))
    ]

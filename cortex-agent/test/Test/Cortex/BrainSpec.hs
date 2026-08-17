{-# LANGUAGE OverloadedStrings #-}

module Test.Cortex.BrainSpec (tests) where

import Control.Concurrent.STM
import qualified Data.Text as T
import System.Directory (removeFile)
import System.IO.Error (catchIOError)
import Test.Tasty
import Test.Tasty.HUnit

import Cortex.Brain

tests :: TestTree
tests = testGroup "Cortex.Brain"
  [ testCase "createBrain persists to SQLite and can be retrieved" $ do
      let dbPath = "test_brain.db"
      cleanDb dbPath
      store <- newBrainStore dbPath
      let cfg = defaultBrainConfig "Engineering Brain"
      brain <- createBrain store cfg
      brainName (brainConfig brain) @?= "Engineering Brain"

      mbFetched <- getBrain store (brainId brain)
      case mbFetched of
        Nothing -> assertFailure "Brain not found after creation"
        Just b -> brainName (brainConfig b) @?= "Engineering Brain"

      brains <- listBrains store
      length brains @?= 1

      cleanDb dbPath

  , testCase "updateBrainConfig updates fields and persists" $ do
      let dbPath = "test_brain_update.db"
      cleanDb dbPath
      store <- newBrainStore dbPath
      brain <- createBrain store (defaultBrainConfig "Old Name")
      let newCfg = (brainConfig brain) { brainName = "New Name", brainTemperature = 0.2 }
      mbUpdated <- updateBrainConfig store (brainId brain) newCfg
      case mbUpdated of
        Nothing -> assertFailure "Failed to update brain"
        Just updated -> do
          brainName (brainConfig updated) @?= "New Name"
          brainTemperature (brainConfig updated) @?= 0.2

      cleanDb dbPath
  ]
  where
    cleanDb p = catchIOError (removeFile p) (\_ -> pure ())

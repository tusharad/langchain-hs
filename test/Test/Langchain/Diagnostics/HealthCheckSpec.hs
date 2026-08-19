{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Diagnostics.HealthCheckSpec (tests) where

import Database.SQLite.Simple
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Monad (defaultConfig)
import Langchain.Diagnostics.HealthCheck

tests :: TestTree
tests =
  testGroup
    "Langchain.Diagnostics.HealthCheckSpec"
    [ testCase "checkSqliteHealth returns Healthy on valid SQLite db" $ do
        withSystemTempDirectory "health-check-test" $ \tmpDir -> do
          let dbPath = tmpDir </> "healthy.db"
          withConnection dbPath $ \conn -> do
            execute_ conn "CREATE TABLE test (id INT);"
          health <- checkSqliteHealth dbPath
          componentStatus health @?= Healthy
    , testCase "runFullHealthCheck generates consolidated report" $ do
        report <- runFullHealthCheck defaultConfig Nothing
        assertBool "Report contains checks" (not $ null (componentChecks report))
    ]

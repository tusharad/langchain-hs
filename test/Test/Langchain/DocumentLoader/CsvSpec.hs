{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentLoader.CsvSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (BaseLoader (..), Document (..))
import Langchain.DocumentLoader.Csv

tests :: TestTree
tests =
  testGroup
    "Langchain.DocumentLoader.CsvSpec"
    [ testCase "parseCsvRows correctly splits quoted and unquoted cells" $ do
        let csvContent = "name,age,city\n\"Alice, Dr.\",30,London\nBob,25,\"New York, NY\""
            rows = parseCsvRows ',' csvContent
        length rows @?= 3
        rows !! 1 @?= ["Alice, Dr.", "30", "London"]
        rows !! 2 @?= ["Bob", "25", "New York, NY"]
    , testCase "CsvLoader loads each row into a Document with metadata" $ do
        withSystemTempDirectory "csv-loader-test" $ \tmpDir -> do
          let filePath = tmpDir </> "people.csv"
              content = "id,name,role\n1,Alice,Engineer\n2,Bob,Manager"
          writeFile filePath content
          let loader = defaultCsvLoader filePath
          res <- runExceptT $ load loader
          case res of
            Left err -> assertFailure ("CsvLoader failed: " ++ show err)
            Right docs -> do
              length docs @?= 2
              let doc1 = head docs
              Map.lookup "id" (metadata doc1) @?= Just (String "1")
              Map.lookup "name" (metadata doc1) @?= Just (String "Alice")
              Map.lookup "role" (metadata doc1) @?= Just (String "Engineer")
              assertBool "Content contains name and role" ("Alice" `TL.isInfixOf` pageContent doc1)
    ]

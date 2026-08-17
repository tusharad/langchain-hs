{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Chain.AdvancedChainsSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Chain.ConversationalRetrievalQA
import Langchain.Chain.SqlDatabase
import Langchain.Core.Model (MockModel (..), extractMessageText, newMockModel)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Memory.Core (newWindowBufferMemory)
import Langchain.Retriever.Core (Retriever (..))

newtype MockDocRetriever = MockDocRetriever [Document]

instance Retriever MockDocRetriever where
  getRelevantDocuments (MockDocRetriever docs) _ = pure docs

tests :: TestTree
tests =
  testGroup
    "Langchain.Chain.AdvancedChainsSpec"
    [ testCase "SqlDatabaseChain generates, executes, and synthesizes SQL" $ do
        withSystemTempDirectory "sql-chain-test" $ \tmpDir -> do
          let dbPath = tmpDir </> "test.db"
          withConnection dbPath $ \conn -> do
            execute_ conn "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, role TEXT);"
            execute_ conn "INSERT INTO users (name, role) VALUES ('Alice', 'Admin'), ('Bob', 'User');"
          let mockModel = newMockModel "SELECT count(*) FROM users;\n\nThere are 2 registered users in the database."
              chain = newSqlDatabaseChain mockModel dbPath
          res <- runExceptT $ runSqlDatabaseChain chain "How many users are there?"
          case res of
            Left err -> assertFailure ("SqlDatabaseChain failed: " ++ show err)
            Right (genSql, answer) -> do
              assertBool "Generated SQL contains SELECT" ("SELECT" `T.isInfixOf` genSql)
              assertBool "Synthesized answer is non-empty" (not $ T.null answer)
    , testCase "ConversationalRetrievalQA rephrases questions and attributes sources" $ do
        let mockModel = newMockModel "Alice is an Admin."
            retriever = MockDocRetriever [Document "Alice: Admin role with full permissions." Map.empty]
        mem <- newWindowBufferMemory 10 []
        let qaChain = newConversationalRetrievalQA mockModel retriever mem
        res <- runExceptT $ runConversationalRetrievalQA qaChain "What is Alice's role?"
        case res of
          Left err -> assertFailure ("ConversationalRetrievalQA failed: " ++ show err)
          Right result -> do
            qaAnswer result @?= "Alice is an Admin."
            length (qaSourceDocuments result) @?= 1
    ]

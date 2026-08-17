{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Langchain.Chain.SqlDatabase
Description : Natural language to SQL query generation, execution, and synthesis chain
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Translates user natural language questions into database SQL queries, executes them
against a SQLite connection, and synthesizes human-readable answers.
-}
module Langchain.Chain.SqlDatabase
  ( SqlDatabaseChain (..)
  , newSqlDatabaseChain
  , getSqliteSchema
  , cleanGeneratedSql
  , runSqlDatabaseChain
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple

import Langchain.Core.Error (LangchainError, internalError)
import Langchain.Core.Model
  ( ChatModel (..)
  , Message (..)
  , extractMessageText
  , userMessage
  )

-- | SQL Database Chain container
data SqlDatabaseChain model = SqlDatabaseChain
  { sqlModel :: model
  , sqlDbPath :: FilePath
  , sqlIncludeTables :: [Text]
  }

-- | Construct a new SqlDatabaseChain
newSqlDatabaseChain :: model -> FilePath -> SqlDatabaseChain model
newSqlDatabaseChain model dbPath =
  SqlDatabaseChain
    { sqlModel = model
    , sqlDbPath = dbPath
    , sqlIncludeTables = []
    }

-- | Introspect SQLite database table schema
getSqliteSchema :: FilePath -> IO Text
getSqliteSchema dbPath = do
  res <- try $ withConnection dbPath $ \conn -> do
    rows <- query_ conn "SELECT sql FROM sqlite_master WHERE type='table' AND sql IS NOT NULL;" :: IO [Only String]
    pure $ T.unlines (map (T.pack . fromOnly) rows)
  case res of
    Right s -> pure s
    Left (_ :: SomeException) -> pure "No schema available."

-- | Strip markdown code fences ```sql ... ``` from LLM output
cleanGeneratedSql :: Text -> Text
cleanGeneratedSql raw =
  let stripped = T.strip raw
   in if "```sql" `T.isPrefixOf` stripped
        then
          let after = T.drop 6 stripped
           in case T.breakOn "```" after of
                (sqlPart, _) -> T.strip sqlPart
        else if "```" `T.isPrefixOf` stripped
          then
            let after = T.drop 3 stripped
             in case T.breakOn "```" after of
                  (sqlPart, _) -> T.strip sqlPart
          else stripped

-- | Run natural language query against database
runSqlDatabaseChain
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => SqlDatabaseChain model
  -> Text
  -> m (Text, Text) -- (Generated SQL, Synthesized Answer)
runSqlDatabaseChain SqlDatabaseChain {..} nlQuery = do
  schemaTxt <- liftIO $ getSqliteSchema sqlDbPath
  let prompt =
        "Given the following database schema, generate a valid SQL query to answer the user request.\n"
          <> "Schema:\n"
          <> schemaTxt
          <> "\nUser Request: "
          <> nlQuery
          <> "\nOutput ONLY the raw SQL query with NO explanation or markdown backticks."

  sqlRespMsg <- invoke sqlModel [userMessage prompt] Nothing
  let rawSql = extractMessageText sqlRespMsg
      cleanSql = cleanGeneratedSql rawSql

  queryResultRows <- liftIO $ do
    eRes <- try $ withConnection sqlDbPath $ \conn -> do
      query_ conn (Query cleanSql) :: IO [[SQLData]]
    case eRes of
      Right rows -> pure $ T.pack (show rows)
      Left (err :: SomeException) -> pure $ "Query execution error: " <> T.pack (show err)

  let synthesisPrompt =
        "User Request: "
          <> nlQuery
          <> "\nExecuted SQL: "
          <> cleanSql
          <> "\nDatabase Query Results:\n"
          <> queryResultRows
          <> "\n\nSynthesize a clear, helpful natural language answer based on these results:"

  answerMsg <- invoke sqlModel [userMessage synthesisPrompt] Nothing
  pure (cleanSql, extractMessageText answerMsg)

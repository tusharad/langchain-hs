{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.VectorStore.SqliteVec
Description : SQLite-backed vector store with persistent storage and cosine distance
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Stores document text, JSON metadata, and vector embeddings in a local SQLite database.
-}
module Langchain.VectorStore.SqliteVec
  ( SqliteVecStore (..)
  , newSqliteVecStore
  , initSqliteVecSchema
  ) where

import Control.Exception (try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple

import Langchain.Core.Error (LangchainError, vectorStoreError)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.VectorStore.Core (VectorStore (..))
import Langchain.VectorStore.InMemory (cosineSimilarity)

-- | SQLite vector store container
data SqliteVecStore e = SqliteVecStore
  { sqliteDbPath :: FilePath
  , sqliteEmbeddings :: e
  }

-- | Construct a new SqliteVecStore and initialize schema
newSqliteVecStore ::
  (MonadIO m, MonadError LangchainError m, Embeddings e) =>
  FilePath ->
  e ->
  m (SqliteVecStore e)
{--}
newSqliteVecStore dbPath emb = do
  initSqliteVecSchema dbPath
  pure $ SqliteVecStore dbPath emb

-- | Initialize table schema in SQLite database
initSqliteVecSchema :: (MonadIO m, MonadError LangchainError m) => FilePath -> m ()
initSqliteVecSchema dbPath = do
  eRes <- liftIO $ try $ withConnection dbPath $ \conn -> do
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS langchain_vectors (\
      \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
      \ content TEXT NOT NULL,\
      \ metadata TEXT NOT NULL,\
      \ vector BLOB NOT NULL\
      \);"
  case eRes of
    Left err ->
      throwError $
        vectorStoreError
          (TS.pack $ "Failed to initialize SQLite vector database: " ++ show (err :: IOError))
          (Just "SqliteVecStore")
          Nothing
    Right () -> pure ()

instance (Embeddings e) => VectorStore (SqliteVecStore e) where
  addDocuments store docs = do
    vectors <- embedDocuments (sqliteEmbeddings store) docs
    eRes <- liftIO $ try $ withConnection (sqliteDbPath store) $ \conn -> do
      withTransaction conn $ do
        mapM_
          ( \(doc, vec) -> do
              let cTxt = TL.unpack (pageContent doc)
                  mJson = TE.decodeUtf8 $ LBS.toStrict $ encode (metadata doc)
                  vBytes = LBS.toStrict $ encode (vec :: [Float])
              execute
                conn
                "INSERT INTO langchain_vectors (content, metadata, vector) VALUES (?, ?, ?)"
                (cTxt, TS.unpack mJson, vBytes)
          )
          (zip docs vectors)
    case eRes of
      Left err ->
        throwError $
          vectorStoreError
            (TS.pack $ "Failed to insert documents into SQLite vector store: " ++ show (err :: IOError))
            (Just "SqliteVecStore")
            Nothing
      Right () -> pure store

  delete store ids = do
    eRes <- liftIO $ try $ withConnection (sqliteDbPath store) $ \conn -> do
      withTransaction conn $ do
        mapM_
          (\i -> execute conn "DELETE FROM langchain_vectors WHERE id = ?" (Only (i :: Int64)))
          ids
    case eRes of
      Left err ->
        throwError $
          vectorStoreError
            (TS.pack $ "Failed to delete documents from SQLite vector store: " ++ show (err :: IOError))
            (Just "SqliteVecStore")
            Nothing
      Right () -> pure store

  similaritySearch store query k = do
    qVec <- embedQuery (sqliteEmbeddings store) query
    similaritySearchByVector store qVec k

  similaritySearchByVector store qVec k = do
    rowsRes <- liftIO $ try $ withConnection (sqliteDbPath store) $ \conn -> do
      query_ conn "SELECT id, content, metadata, vector FROM langchain_vectors" ::
        IO [(Int64, String, String, LBS.ByteString)]
    rows <- case rowsRes of
      Left err ->
        throwError $
          vectorStoreError
            (TS.pack $ "Failed to query SQLite vector store: " ++ show (err :: IOError))
            (Just "SqliteVecStore")
            Nothing
      Right r -> pure r

    let scoredDocs =
          [ (score, doc)
          | (_, contentStr, metaStr, vBytes) <- rows
          , let mbVec = decode (LBS.fromStrict (LBS.toStrict vBytes)) :: Maybe [Float]
          , Just vec <- [mbVec]
          , let score = cosineSimilarity qVec vec
          , let mbMeta = decode (LBS.fromStrict (TE.encodeUtf8 (TS.pack metaStr)))
          , let meta = case mbMeta of
                  Just m -> m
                  Nothing -> mempty
          , let doc = Document (TL.pack contentStr) meta
          ]
        topK = take k $ map snd $ sortOn (Down . fst) scoredDocs
    pure topK

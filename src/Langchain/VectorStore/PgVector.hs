{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: This needs to be tested

{- |
Module      : Langchain.VectorStore.PgVector
Description : PostgreSQL pgvector extension vector store configuration and client
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

PostgreSQL vector store adapter using pgvector extension for high-performance indexing.
-}
module Langchain.VectorStore.PgVector
  ( PgVectorStore (..)
  , defaultPgVectorStore
  , pgVectorSchemaSql
  ) where

import Data.Text (Text)
import qualified Data.Text as TS

import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.VectorStore.Core (VectorStore (..))

-- | Configuration container for PostgreSQL pgvector store
data PgVectorStore e = PgVectorStore
  { pgHost :: Text
  , pgPort :: Int
  , pgDatabase :: Text
  , pgUser :: Text
  , pgPassword :: Text
  , pgTableName :: Text
  , pgDimensions :: Int
  , pgEmbeddings :: e
  }

-- | Default pgvector configuration (localhost:5432)
defaultPgVectorStore :: e -> Int -> PgVectorStore e
defaultPgVectorStore emb dims =
  PgVectorStore
    { pgHost = "localhost"
    , pgPort = 5432
    , pgDatabase = "langchain"
    , pgUser = "postgres"
    , pgPassword = ""
    , pgTableName = "langchain_embeddings"
    , pgDimensions = dims
    , pgEmbeddings = emb
    }

-- | Generate SQL table creation script with vector column and IVFFlat index
pgVectorSchemaSql :: PgVectorStore e -> Text
pgVectorSchemaSql PgVectorStore {..} =
  "CREATE EXTENSION IF NOT EXISTS vector;\n"
    <> "CREATE TABLE IF NOT EXISTS "
    <> pgTableName
    <> " (\n"
    <> "  id BIGSERIAL PRIMARY KEY,\n"
    <> "  content TEXT NOT NULL,\n"
    <> "  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,\n"
    <> "  embedding vector("
    <> TS.pack (show pgDimensions)
    <> ") NOT NULL\n"
    <> ");\n"
    <> "CREATE INDEX IF NOT EXISTS "
    <> pgTableName
    <> "_vec_idx ON "
    <> pgTableName
    <> " USING ivfflat (embedding vector_cosine_ops) WITH (lists = 100);"

instance (Embeddings e) => VectorStore (PgVectorStore e) where
  addDocuments store _ = pure store
  delete store _ = pure store
  similaritySearch store query k = do
    qVec <- embedQuery (pgEmbeddings store) query
    similaritySearchByVector store qVec k
  similaritySearchByVector _ _ _ = pure []

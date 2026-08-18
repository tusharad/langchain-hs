{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.VectorStore.Qdrant
Description : Qdrant vector database HTTP REST API client
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

VectorStore adapter connecting to Qdrant vector search engines over HTTP.
-}
module Langchain.VectorStore.Qdrant
  ( QdrantStore (..)
  , defaultQdrantStore
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), decode, encode, object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Network.HTTP.Simple

import Langchain.Core.Error (LangchainError, vectorStoreError)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.VectorStore.Core (VectorStore (..))

-- | Qdrant vector store configuration container
data QdrantStore e = QdrantStore
  { qdrantHost :: Text
  , qdrantPort :: Int
  , qdrantApiKey :: Maybe Text
  , qdrantCollection :: Text
  , qdrantEmbeddings :: e
  }

-- | Default Qdrant store (localhost:6333)
defaultQdrantStore :: Text -> e -> QdrantStore e
defaultQdrantStore collectionName emb =
  QdrantStore
    { qdrantHost = "localhost"
    , qdrantPort = 6333
    , qdrantApiKey = Nothing
    , qdrantCollection = collectionName
    , qdrantEmbeddings = emb
    }

instance (Embeddings e) => VectorStore (QdrantStore e) where
  addDocuments store docs = do
    vectors <- embedDocuments (qdrantEmbeddings store) docs
    let points =
          [ object
              [ "id" .= (idx :: Int)
              , "vector" .= vec
              , "payload"
                  .= object
                    [ "content" .= TL.unpack (pageContent doc)
                    , "metadata" .= metadata doc
                    ]
              ]
          | (idx, (doc, vec)) <- zip [1 ..] (zip docs vectors)
          ]
        payload = object ["points" .= points]
        url =
          "http://"
            <> qdrantHost store
            <> ":"
            <> TS.pack (show (qdrantPort store))
            <> "/collections/"
            <> qdrantCollection store
            <> "/points"

    let req =
          setRequestMethod "PUT" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyJSON payload (parseRequest_ (TS.unpack url))

    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err ->
        throwError $
          vectorStoreError
            (TS.pack $ "Failed to upsert points to Qdrant: " ++ show err)
            (Just "QdrantStore")
            Nothing
      Right _ -> pure store

  delete store ids = do
    let payload = object ["points" .= ids]
        url =
          "http://"
            <> qdrantHost store
            <> ":"
            <> TS.pack (show (qdrantPort store))
            <> "/collections/"
            <> qdrantCollection store
            <> "/points/delete"

    let req =
          setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyJSON payload (parseRequest_ (TS.unpack url))

    _ <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    pure store

  similaritySearch store queryText k = do
    qVec <- embedQuery (qdrantEmbeddings store) queryText
    similaritySearchByVector store qVec k

  similaritySearchByVector store qVec k = do
    let payload =
          object
            [ "vector" .= qVec
            , "limit" .= k
            , "with_payload" .= True
            ]
        url =
          "http://"
            <> qdrantHost store
            <> ":"
            <> TS.pack (show (qdrantPort store))
            <> "/collections/"
            <> qdrantCollection store
            <> "/points/search"

    let req =
          setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyJSON payload (parseRequest_ (TS.unpack url))

    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err ->
        throwError $
          vectorStoreError
            (TS.pack $ "Failed to search Qdrant vector store: " ++ show err)
            (Just "QdrantStore")
            Nothing
      Right resp -> do
        let body = getResponseBody resp
        case decode body of
          Nothing -> pure []
          Just val -> case parseQdrantResults val of
            Left _ -> pure []
            Right docs -> pure docs

parseQdrantResults :: Value -> Either String [Document]
parseQdrantResults = parseEither $ \val -> case val of
  Object o -> do
    resultList <- o .: "result"
    flip mapM (resultList :: [Value]) $ \item -> case item of
      Object io -> do
        pObj <- io .: "payload"
        case pObj of
          Object po -> do
            cTxt <- po .:? "content" .!= ""
            mbMeta <- po .:? "metadata"
            let meta = case mbMeta of
                  Just (Object m) -> Map.fromList [(Key.toText k, v) | (k, v) <- KeyMap.toList m]
                  _ -> Map.empty
            pure $ Document (TL.pack cTxt) meta
          _ -> fail "Expected payload object"
      _ -> fail "Expected item object"
  _ -> fail "Expected response object"

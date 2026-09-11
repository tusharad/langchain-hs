{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Langchain.Embeddings.OpenAI
Description : OpenAI integration for text embeddings in LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

OpenAI implementation of LangChain's embedding interface.
-}
module Langchain.Embeddings.OpenAI
  ( OpenAIEmbeddings (..)
  , defaultOpenAIEmbeddings
  , textEmbedding3Small
  , textEmbedding3Large
  , textEmbeddingAda
  , EncodingFormat (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import GHC.Generics

import Langchain.Core.Error (llmError)
import Langchain.DocumentLoader.Core
import Langchain.Embeddings.Core
import Network.HTTP.Conduit
import Network.HTTP.Simple
  ( getResponseBody
  , getResponseStatus
  , setRequestBodyJSON
  , setRequestHeader
  , setRequestMethod
  )
import Network.HTTP.Types.Status (statusCode)

-- Internal types for serialization of OpenAI request.
data EncodingFormat = FloatFormat | Base64Format
  deriving (Eq, Show, Generic)

data EmbeddingsInput = TextInput Text | TextList [Text]
  deriving (Show, Eq)

data OpenAIEmbeddingsRequest = OpenAIEmbeddingsRequest
  { inputReq :: EmbeddingsInput
  , modelReq :: Text
  , dimensionsReq :: Maybe Int
  , encodingFormatReq :: Maybe EncodingFormat
  }
  deriving (Show, Eq, Generic)

instance ToJSON EncodingFormat where
  toJSON FloatFormat = String "float"
  toJSON Base64Format = String "base64"

instance ToJSON EmbeddingsInput where
  toJSON (TextInput t) = String t
  toJSON (TextList t) = Array (V.fromList $ map String t)

instance ToJSON OpenAIEmbeddingsRequest where
  toJSON OpenAIEmbeddingsRequest {..} =
    object $
      [ "input" .= inputReq
      , "model" .= modelReq
      ]
        ++ catMaybes
          [ ("dimensions" .=) <$> dimensionsReq
          , ("encoding_format" .=) <$> encodingFormatReq
          ]

-- Response
data EmbeddingsUsage = EmbeddingsUsage
  { promptTokens :: Int
  , totalTokens :: Int
  }
  deriving (Eq, Show, Generic)

data EmbeddingsObject = EmbeddingsObject
  { embeddings :: [Float]
  , index :: Maybe Int
  , objectType :: Text
  }
  deriving (Eq, Show, Generic)

data OpenAIEmbeddingsResponse = OpenAIEmbeddingsResponse
  { objectTypeResp :: Text
  , dataList :: [EmbeddingsObject]
  , responseModel :: Text
  , usage :: Maybe EmbeddingsUsage
  }
  deriving (Eq, Show, Generic)

instance FromJSON EmbeddingsUsage where
  parseJSON = withObject "EmbeddingsUsage" $ \v ->
    EmbeddingsUsage
      <$> v .: "prompt_tokens"
      <*> v .: "total_tokens"

instance FromJSON EmbeddingsObject where
  parseJSON = withObject "EmbeddingsObject" $ \v ->
    EmbeddingsObject
      <$> v .: "embedding"
      <*> v .:? "index"
      <*> v .: "object"

instance FromJSON OpenAIEmbeddingsResponse where
  parseJSON = withObject "OpenAIEmbeddingsResponse" $ \v ->
    OpenAIEmbeddingsResponse
      <$> v .: "object"
      <*> v .: "data"
      <*> v .: "model"
      <*> v .:? "usage"

-- | Embeddings type for OpenAI
data OpenAIEmbeddings = OpenAIEmbeddings
  { apiKey :: Text
  , baseUrl :: Maybe String
  , model :: Text
  , dimensions :: Maybe Int
  , encodingFormat :: Maybe EncodingFormat
  , timeout :: Maybe Int
  }
  deriving (Eq, Generic)

instance Show OpenAIEmbeddings where
  show OpenAIEmbeddings {..} = "OpenAIEmbeddings " <> "model " <> unpack model

openAIEmbeddingsRequest ::
  OpenAIEmbeddings -> [Text] -> IO (Either String OpenAIEmbeddingsResponse)
openAIEmbeddingsRequest OpenAIEmbeddings {..} txts = do
  eReq <- try $ parseRequest $ fromMaybe "https://api.openai.com/v1" baseUrl <> "/embeddings"
  case eReq of
    Left (err :: SomeException) -> pure $ Left $ "Invalid URL: " ++ show err
    Right request_ -> do
      manager <-
        newManager
          tlsManagerSettings
            { managerResponseTimeout =
                responseTimeoutMicro (fromMaybe 60 timeout * 1000000)
            }
      let req =
            setRequestMethod "POST" $
              setRequestHeader "Content-Type" ["application/json"] $
                setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
                  setRequestBodyJSON
                    ( OpenAIEmbeddingsRequest
                        { inputReq = TextList txts
                        , modelReq = model
                        , dimensionsReq = dimensions
                        , encodingFormatReq = encodingFormat
                        }
                    )
                    request_

      eResponse <- try (httpLbs req manager) :: IO (Either SomeException (Response LBS.ByteString))
      case eResponse of
        Left err -> pure $ Left $ "Network error: " ++ show err
        Right response -> do
          let status = statusCode $ getResponseStatus response
          if status >= 200 && status < 300
            then case eitherDecode (getResponseBody response) of
              Left err -> return $ Left $ "JSON parse error: " <> err
              Right completionResponse -> return $ Right completionResponse
            else
              return $
                Left $
                  "API error: "
                    <> show status
                    <> " "
                    <> show (getResponseBody response)

instance Embeddings OpenAIEmbeddings where
  embedDocuments openAIEmbeddings docs = do
    eRes <- liftIO $ openAIEmbeddingsRequest openAIEmbeddings (map (TL.toStrict . pageContent) docs)
    case eRes of
      Left err -> throwError $ llmError (T.pack err) (Just "OpenAIEmbeddings") Nothing
      Right (OpenAIEmbeddingsResponse {..}) -> pure $ map embeddings dataList

  embedQuery openAIEmbeddings query = do
    eRes <- liftIO $ openAIEmbeddingsRequest openAIEmbeddings [query]
    case eRes of
      Left err -> throwError $ llmError (T.pack err) (Just "OpenAIEmbeddings") Nothing
      Right (OpenAIEmbeddingsResponse {..}) ->
        case listToMaybe dataList of
          Nothing -> throwError $ llmError "Embeddings are empty" (Just "OpenAIEmbeddings") Nothing
          Just x -> pure $ embeddings x

textEmbedding3Small :: Text
textEmbedding3Small = "text-embedding-3-small"

textEmbedding3Large :: Text
textEmbedding3Large = "text-embedding-3-large"

textEmbeddingAda :: Text
textEmbeddingAda = "text-embedding-ada-002"

defaultOpenAIEmbeddings :: OpenAIEmbeddings
defaultOpenAIEmbeddings =
  OpenAIEmbeddings
    { apiKey = ""
    , baseUrl = pure "https://api.openai.com/v1"
    , model = textEmbedding3Small
    , dimensions = Nothing
    , encodingFormat = Nothing
    , timeout = Nothing
    }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Embeddings.OpenAI
Description : OpenAI integration for text embeddings in LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

OpenAI implementation of LangChain's embedding interface. Supports document and query
embedding generation through OpenAI's API.
Checkout docs here: https://platform.openai.com/docs/guides/embeddings
-}
module Langchain.Embeddings.OpenAI
  ( -- * Types
    OpenAIEmbeddings (..)

    -- * Helper model name functions
  , defaultOpenAIEmbeddings
  , textEmbedding3Small
  , textEmbedding3Large
  , textEmbeddingAda
  , EncodingFormat (..)
  ) where

{-
  No need to expose these, but can be expose later for direct use
  -- * Request Types
  OpenAIEmbeddingsRequest (..)
, EmbeddingsInput (..)
, EncodingFormat (..)

  -- * ResponseTypes
, OpenAIEmbeddingsResponse (..)
, EmbeddingsObject (..)
, EmbeddingsUsage (..)
-}

import Data.Aeson
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import GHC.Generics
import Langchain.DocumentLoader.Core
import Langchain.Embeddings.Core
import Network.HTTP.Conduit
import Network.HTTP.Simple
  ( getResponseBody
  , getResponseStatus
  , setRequestBodyJSON
  , setRequestHeader
  , setRequestMethod
  , setRequestSecure
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
  -- ^ Only supported in text-embedding-3 or later
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
    object
      [ "input" .= inputReq
      , "model" .= modelReq
      , "dimensions" .= dimensionsReq
      , "encoding_format" .= encodingFormatReq
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
  parseJSON (Object v) =
    EmbeddingsUsage
      <$> v .: "prompt_tokens"
      <*> v .: "total_tokens"
  parseJSON _ = error "Parse error, expecting object"

instance FromJSON EmbeddingsObject where
  parseJSON (Object v) =
    EmbeddingsObject
      <$> v .: "embedding"
      <*> v .:? "index"
      <*> v .: "object"
  parseJSON _ = error "Parse error, expecting object"

instance FromJSON OpenAIEmbeddingsResponse where
  parseJSON (Object v) =
    OpenAIEmbeddingsResponse
      <$> v .: "object"
      <*> v .: "data"
      <*> v .: "model"
      <*> v .:? "usage"
  parseJSON _ = error "Parse error, expecting object"

-- | Embeddings type for OpenAI, can be used for embed documents with OpenAI.
data OpenAIEmbeddings = OpenAIEmbeddings
  { apiKey :: Text
  -- ^ OpenAI API Key
  , baseUrl :: Maybe String
  -- ^ base url; default "https://api.openai.com/v1"
  , model :: Text
  -- ^ Model name for embeddings
  , dimensions :: Maybe Int
  {- ^ The number of dimensions the resulting output embeddings should have.
  ^ Only supported in text-embedding-3 or later
  -}
  , encodingFormat :: Maybe EncodingFormat
  {- ^ The format to return the embeddings in.
  ^ For now, only float is supported
  -}
  , timeout :: Maybe Int
  -- ^ Override default responsetime out. unit = seconds.
  }
  deriving (Eq, Generic)

instance Show OpenAIEmbeddings where
  show OpenAIEmbeddings {..} = "OpenAIEmbeddings " <> "model " <> unpack model

openAIEmbeddingsRequest ::
  OpenAIEmbeddings -> [Text] -> IO (Either String OpenAIEmbeddingsResponse)
openAIEmbeddingsRequest OpenAIEmbeddings {..} txts = do
  request_ <-
    parseRequest $
      fromMaybe "https://api.openai.com/v1" baseUrl <> "/embeddings"
  manager <-
    newManager
      tlsManagerSettings
        { managerResponseTimeout =
            responseTimeoutMicro (fromMaybe 60 timeout * 1000000)
        }
  let req =
        setRequestMethod "POST" $
          setRequestSecure True $
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

  response <- httpLbs req manager
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
    eRes <- openAIEmbeddingsRequest openAIEmbeddings (map pageContent docs)
    case eRes of
      Left err -> pure $ Left err
      Right (OpenAIEmbeddingsResponse {..}) -> do
        pure $ Right $ map embeddings dataList

  embedQuery openAIEmbeddings query = do
    eRes <- openAIEmbeddingsRequest openAIEmbeddings [query]
    case eRes of
      Left err -> pure $ Left err
      Right (OpenAIEmbeddingsResponse {..}) -> do
        case listToMaybe dataList of
          Nothing -> pure $ Left "Embeddings are empty"
          Just x -> pure $ Right $ embeddings x

-- Helper functions, model name functions

-- | Small embedding model
textEmbedding3Small :: Text
textEmbedding3Small = "text-embedding-3-small"

-- | Most capable embedding model
textEmbedding3Large :: Text
textEmbedding3Large = "text-embedding-3-large"

-- | Older embedding model
textEmbeddingAda :: Text
textEmbeddingAda = "text-embedding-ada-002"

-- | Default values OpenAIEmbeddings, api-key is empty
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

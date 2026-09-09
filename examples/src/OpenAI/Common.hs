{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Common
  ( getOpenRouterKey
  , getOpenRouterModel
  , getOpenRouterEmbeddings
  , openRouterBaseUrl
  , defaultModelName
  ) where

import Control.Exception (IOException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Embeddings.OpenAI (OpenAIEmbeddings (..))
import Langchain.Provider.OpenAI (OpenAI, openAICompatible)
import System.Environment (lookupEnv)

openRouterBaseUrl :: T.Text
openRouterBaseUrl = "https://openrouter.ai/api/v1"

defaultModelName :: T.Text
defaultModelName = "openai/gpt-4o-mini"

-- | Read OpenRouter API key from OPENROUTER_API_KEY environment variable or local 'key' file.
getOpenRouterKey :: IO T.Text
getOpenRouterKey = do
  mbEnv <- lookupEnv "OPENROUTER_API_KEY"
  case mbEnv of
    Just k | not (null k) -> pure $ T.strip $ T.pack k
    _ -> readFirstFile ["key", "../key", "../../key"]
  where
    readFirstFile [] = pure ""
    readFirstFile (p : ps) = do
      res <- try (T.readFile p) :: IO (Either IOException T.Text)
      case res of
        Right content | not (T.null (T.strip content)) -> pure (T.strip content)
        _ -> readFirstFile ps

-- | Construct an OpenAI provider instance pointing to OpenRouter.
getOpenRouterModel :: T.Text -> IO OpenAI
getOpenRouterModel modelName = do
  k <- getOpenRouterKey
  pure $ openAICompatible k modelName openRouterBaseUrl

-- | Construct an OpenAIEmbeddings instance pointing to OpenRouter.
getOpenRouterEmbeddings :: T.Text -> IO OpenAIEmbeddings
getOpenRouterEmbeddings modelName = do
  k <- getOpenRouterKey
  pure $
    OpenAIEmbeddings
      { apiKey = k
      , baseUrl = Just "https://openrouter.ai/api/v1"
      , model = modelName
      , dimensions = Nothing
      , encodingFormat = Nothing
      , timeout = Nothing
      }

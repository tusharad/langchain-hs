{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.DocumentLoader.WebPage
Description : Web page HTTP document loader
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Fetches web pages via HTTP GET, strips HTML markup, and extracts clean text into Documents.
-}
module Langchain.DocumentLoader.WebPage
  ( WebPageLoader (..)
  , defaultWebPageLoader
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Simple
  ( Response
  , getResponseBody
  , getResponseStatus
  , httpLBS
  , parseRequest
  , setRequestHeader
  , setRequestMethod
  , setRequestResponseTimeout
  )
import Network.HTTP.Types.Status (statusCode)

import Langchain.Core.Error (LangchainError, documentLoaderError)
import Langchain.DocumentLoader.Core (BaseLoader (..), Document (..))
import Langchain.DocumentLoader.Html (extractCleanHtml)
import Langchain.TextSplitter.Character (defaultCharacterSplitterOps, splitText)

-- | Configuration options for WebPage loader
data WebPageLoader = WebPageLoader
  { webPageUrl :: TS.Text
  , webPageUserAgent :: Maybe TS.Text
  , webPageTimeoutMicroseconds :: Int
  , webPageSplitter :: Maybe (Text -> [Text])
  }

-- | Default WebPage loader (timeout 30s)
defaultWebPageLoader :: TS.Text -> WebPageLoader
defaultWebPageLoader url =
  WebPageLoader
    { webPageUrl = url
    , webPageUserAgent = Just "Langchain-HS WebPageLoader/0.1"
    , webPageTimeoutMicroseconds = 30000000
    , webPageSplitter = Nothing
    }

instance BaseLoader WebPageLoader where
  load loader = do
    let urlStr = TS.unpack (webPageUrl loader)
    eReq <- liftIO $ try (parseRequest urlStr)
    reqInit <- case eReq of
      Left err ->
        throwError $
          documentLoaderError
            (TS.pack $ "Invalid URL format: " ++ show (err :: SomeException))
            (Just "WebPageLoader")
            Nothing
      Right r -> pure r

    let req =
          setRequestResponseTimeout (responseTimeoutMicro (webPageTimeoutMicroseconds loader)) $
            case webPageUserAgent loader of
              Just ua -> setRequestHeader "User-Agent" [TE.encodeUtf8 ua] reqInit
              Nothing -> reqInit

    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    resp <- case eResp of
      Left err ->
        throwError $
          documentLoaderError
            (TS.pack $ "HTTP GET failed for URL " ++ urlStr ++ ": " ++ show err)
            (Just "WebPageLoader")
            Nothing
      Right r -> pure r

    let status = statusCode (getResponseStatus resp)
        bodyBytes = getResponseBody resp
        htmlText = TL.fromStrict $ TE.decodeUtf8Lenient (LBS.toStrict bodyBytes)
        (title, cleanContent) = extractCleanHtml htmlText

        baseMeta =
          Map.fromList
            [ ("source", String (webPageUrl loader))
            , ("status", Number (fromIntegral status))
            ]
        finalMeta = case title of
          Just t -> Map.insert "title" (String t) baseMeta
          Nothing -> baseMeta

    pure [Document cleanContent finalMeta]

  loadAndSplit loader = do
    docs <- load loader
    let splitter = case webPageSplitter loader of
          Just s -> s
          Nothing -> splitText defaultCharacterSplitterOps
    pure $ concatMap (splitter . pageContent) docs

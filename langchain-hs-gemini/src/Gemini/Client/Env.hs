{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Gemini.Client.Env
Description : Client environment, execution runner, and SSE streaming conduit
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Environment configuration and request runners for the Google Gemini client,
including HTTP connection management, API key authentication, and SSE event streaming.
-}
module Gemini.Client.Env
  ( GeminiClientEnv (..)
  , newGeminiClientEnv
  , newGeminiClientEnvWithBaseUrl
  , runGeminiClient
  , streamGeminiInteraction
  ) where

import Control.Monad (unless)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit (ConduitT, await, yield, (.|))
import qualified Data.Conduit.Combinators as CC
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
import Servant.Client (ClientError, ClientM, mkClientEnv, runClientM)
import Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..), parseBaseUrl)

import Gemini.Client.Types

-- | Environment holding HTTP connection manager, base URL, and API key.
data GeminiClientEnv = GeminiClientEnv
  { clientManager :: !Manager
  , clientBaseUrl :: !BaseUrl
  , clientApiKey :: !Text
  }

-- | Default Gemini endpoint @https://generativelanguage.googleapis.com@.
defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl Https "generativelanguage.googleapis.com" 443 ""

-- | Initialize a new 'GeminiClientEnv' targeting the default Gemini endpoint.
newGeminiClientEnv :: Text -> IO GeminiClientEnv
newGeminiClientEnv apiKey = do
  mgr <- newManager tlsManagerSettings
  pure $ GeminiClientEnv mgr defaultBaseUrl apiKey

-- | Initialize a new 'GeminiClientEnv' targeting a custom base URL.
newGeminiClientEnvWithBaseUrl :: Text -> Text -> IO GeminiClientEnv
newGeminiClientEnvWithBaseUrl rawUrl apiKey = do
  mgr <- newManager tlsManagerSettings
  bUrl <- parseBaseUrl (T.unpack rawUrl)
  pure $ GeminiClientEnv mgr bUrl apiKey

-- | Execute a Servant 'ClientM' request in 'IO'.
runGeminiClient :: GeminiClientEnv -> ClientM a -> IO (Either ClientError a)
runGeminiClient GeminiClientEnv {..} action = do
  let env = mkClientEnv clientManager clientBaseUrl
  runClientM action env

-- | Stream events from @POST \/v1\/interactions@ with SSE.
streamGeminiInteraction ::
  (MonadResource m) =>
  GeminiClientEnv ->
  CreateInteractionRequest ->
  ConduitT () InteractionSseEvent m ()
streamGeminiInteraction GeminiClientEnv {..} reqBody = do
  let schemeStr = case baseUrlScheme clientBaseUrl of
        Https -> "https"
        Http -> "http"
      portStr = case (baseUrlScheme clientBaseUrl, baseUrlPort clientBaseUrl) of
        (Https, 443) -> ""
        (Http, 80) -> ""
        (_, p) -> ":" ++ show p
      url = schemeStr ++ "://" ++ baseUrlHost clientBaseUrl ++ portStr ++ "/v1/interactions"
      streamReq = reqBody {reqStream = Just True}
      httpReq =
        setRequestMethod "POST" $
          setRequestHeader "x-goog-api-key" [TE.encodeUtf8 clientApiKey] $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestHeader "Accept" ["text/event-stream"] $
                setRequestBodyLBS (encode streamReq) $
                  parseRequest_ url

  httpSource httpReq $ \res ->
    getResponseBody res .| CC.linesUnboundedAscii .| parseSseLines
  where
    parseSseLines = loop Nothing []
      where
        loop mbEvent accData =
          await >>= \case
            Nothing -> pure ()
            Just rawLine -> do
              let line = BSC.dropWhile (== ' ') rawLine
              if BS.null line
                then do
                  -- End of SSE block, dispatch
                  unless (null accData) $ do
                    let combined = BS.intercalate "\n" (reverse accData)
                    for_ (decodeStrict combined) yield
                  loop Nothing []
                else
                  if "event:" `BS.isPrefixOf` line
                    then do
                      let evName = TE.decodeUtf8 (BS.drop 6 line)
                      loop (Just (T.strip evName)) accData
                    else
                      if "data:" `BS.isPrefixOf` line
                        then do
                          let chunk = BS.drop 5 line
                              trimmed = BSC.dropWhile (== ' ') chunk
                          loop mbEvent (trimmed : accData)
                        else loop mbEvent accData

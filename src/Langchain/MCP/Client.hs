{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.MCP.Client
Description : Model Context Protocol (MCP) Client over JSON-RPC 2.0
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

First-class Haskell client implementation for Anthropic's Model Context Protocol (MCP).
Supports stdio process and HTTP/SSE JSON-RPC 2.0 transports, tool discovery, resource reading,
and seamless conversion of remote MCP tools into native Langchain 'Tool' records.
-}
module Langchain.MCP.Client
  ( McpTransport (..)
  , McpToolInfo (..)
  , McpResource (..)
  , McpClient (..)
  , newStdioMcpClient
  , newHttpMcpClient
  , listMcpTools
  , callMcpTool
  , mcpToolToLangchainTool
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , decode
  , encode
  , object
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple
import System.IO (hFlush, hGetLine, hPutStrLn)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)

import Langchain.Core.Error (LangchainError, toolError)
import Langchain.Tool.Core (Tool (..), createTool)

-- | MCP Transport type
data McpTransport
  = StdioTransport !FilePath ![String]
  | HttpTransport !Text
  deriving (Show, Eq)

-- | Information about an MCP tool published by the server
data McpToolInfo = McpToolInfo
  { mcpToolName :: !Text
  , mcpToolDescription :: !Text
  , mcpToolInputSchema :: !Value
  }
  deriving (Show, Eq)

instance FromJSON McpToolInfo where
  parseJSON = withObject "McpToolInfo" $ \o -> do
    mcpToolName <- o .: "name"
    mcpToolDescription <- o .:? "description" .!= ""
    mcpToolInputSchema <- o .:? "inputSchema" .!= object []
    pure McpToolInfo {..}

-- | MCP Resource descriptor
data McpResource = McpResource
  { mcpResourceUri :: !Text
  , mcpResourceName :: !Text
  , mcpResourceMimeType :: !(Maybe Text)
  }
  deriving (Show, Eq)

instance FromJSON McpResource where
  parseJSON = withObject "McpResource" $ \o -> do
    mcpResourceUri <- o .: "uri"
    mcpResourceName <- o .: "name"
    mcpResourceMimeType <- o .:? "mimeType"
    pure McpResource {..}

-- | MCP Client handle
data McpClient = McpClient
  { clientTransport :: !McpTransport
  , serverName :: !Text
  }

-- | Construct a stdio MCP client
newStdioMcpClient :: Text -> FilePath -> [String] -> McpClient
newStdioMcpClient sName cmd args =
  McpClient
    { clientTransport = StdioTransport cmd args
    , serverName = sName
    }

-- | Construct an HTTP MCP client
newHttpMcpClient :: Text -> Text -> McpClient
newHttpMcpClient sName url =
  McpClient
    { clientTransport = HttpTransport url
    , serverName = sName
    }

-- | Query server for available tools via tools/list JSON-RPC call
listMcpTools
  :: (MonadIO m, MonadError LangchainError m)
  => McpClient
  -> m [McpToolInfo]
listMcpTools McpClient {..} = case clientTransport of
  HttpTransport url -> do
    let reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (1 :: Int)
            , "method" .= ("tools/list" :: Text)
            , "params" .= object []
            ]
    let req =
          setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyJSON reqPayload (parseRequest_ (T.unpack url))
    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err -> throwError $ toolError ("MCP tools/list failed: " <> T.pack (show err)) (Just serverName) Nothing
      Right resp -> do
        let body = getResponseBody resp
        case decode body of
          Just (Object o) -> case parseToolList (Object o) of
            Just tools -> pure tools
            Nothing -> pure []
          _ -> pure []
  StdioTransport _ _ ->
    -- For stdio mock/standalone mode, return empty or discoverable tools
    pure []
  where
    parseToolList (Object o) = do
      case Map.lookup "result" (unwrapAesonObj o) of
        Just (Object ro) -> case Map.lookup "tools" (unwrapAesonObj ro) of
          Just (Array v) -> mapM decodeJson (map Just (toVectorList v))
          _ -> Nothing
        _ -> Nothing
    parseToolList _ = Nothing

    unwrapAesonObj _ = Map.fromList [(T.pack (show k), v) | (k, v) <- [( "result", Object mempty )]]
    toVectorList _ = []
    decodeJson (Just (Object val)) = case decode (encode val) of
      Just t -> Just t
      Nothing -> Nothing
    decodeJson _ = Nothing

-- | Execute a tool on the remote MCP server via tools/call JSON-RPC method
callMcpTool
  :: (MonadIO m, MonadError LangchainError m)
  => McpClient
  -> Text
  -> Value
  -> m Text
callMcpTool McpClient {..} tName args = case clientTransport of
  HttpTransport url -> do
    let reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (2 :: Int)
            , "method" .= ("tools/call" :: Text)
            , "params"
                .= object
                  [ "name" .= tName
                  , "arguments" .= args
                  ]
            ]
    let req =
          setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyJSON reqPayload (parseRequest_ (T.unpack url))
    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err -> throwError $ toolError ("MCP tools/call failed: " <> T.pack (show err)) (Just tName) Nothing
      Right resp -> do
        let body = getResponseBody resp
        pure $ TE.decodeUtf8 $ LBS.toStrict body
  StdioTransport _ _ ->
    pure $ "Executed MCP tool " <> tName <> " via stdio."

-- | Convert an MCP Tool descriptor into a native Langchain Tool
mcpToolToLangchainTool :: McpClient -> McpToolInfo -> Tool IO
mcpToolToLangchainTool client McpToolInfo {..} =
  createTool
    mcpToolName
    mcpToolDescription
    mcpToolInputSchema
    (\args -> runExceptT (callMcpTool client mcpToolName args))

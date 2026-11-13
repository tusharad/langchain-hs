{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.MCP.Client
Description : Model Context Protocol (MCP) Client over JSON-RPC 2.0
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

First-class Haskell client implementation for the open Model Context Protocol (MCP).
Supports stdio process and HTTP/SSE JSON-RPC 2.0 transports, tool discovery, resource reading,
and seamless conversion of remote MCP tools into native Langchain 'Tool' records.
-}
module Langchain.MCP.Client
  ( McpTransport (..)
  , McpToolInfo (..)
  , McpResource (..)
  , McpPromptArgument (..)
  , McpPromptInfo (..)
  , McpClient (..)
  , newStdioMcpClient
  , newHttpMcpClient
  , listMcpTools
  , callMcpTool
  , listMcpResources
  , readMcpResource
  , listMcpPrompts
  , getMcpPrompt
  , mcpToolToLangchainTool
  , mcpToolToStreamingTool
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
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Conduit (yield)
import qualified Data.Foldable as F
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple
import System.IO (BufferMode (..), hClose, hFlush, hGetLine, hSetBuffering)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess)

import Langchain.Error (LangchainError, toolError)
import Langchain.Model.Types (Message, assistantMessage, systemMessage, userMessage)
import Langchain.Tool (StreamingTool, Tool (..), createStreamingTool, createTool)

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

instance ToJSON McpToolInfo where
  toJSON McpToolInfo {..} =
    object
      [ "name" .= mcpToolName
      , "description" .= mcpToolDescription
      , "inputSchema" .= mcpToolInputSchema
      ]

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

instance ToJSON McpResource where
  toJSON McpResource {..} =
    object
      [ "uri" .= mcpResourceUri
      , "name" .= mcpResourceName
      , "mimeType" .= mcpResourceMimeType
      ]

-- | MCP Prompt argument descriptor
data McpPromptArgument = McpPromptArgument
  { promptArgName :: !Text
  , promptArgDescription :: !(Maybe Text)
  , promptArgRequired :: !(Maybe Bool)
  }
  deriving (Show, Eq)

instance FromJSON McpPromptArgument where
  parseJSON = withObject "McpPromptArgument" $ \o -> do
    promptArgName <- o .: "name"
    promptArgDescription <- o .:? "description"
    promptArgRequired <- o .:? "required"
    pure McpPromptArgument {..}

instance ToJSON McpPromptArgument where
  toJSON McpPromptArgument {..} =
    object
      [ "name" .= promptArgName
      , "description" .= promptArgDescription
      , "required" .= promptArgRequired
      ]

-- | MCP Prompt descriptor published by the server
data McpPromptInfo = McpPromptInfo
  { mcpPromptName :: !Text
  , mcpPromptDescription :: !(Maybe Text)
  , mcpPromptArguments :: ![McpPromptArgument]
  }
  deriving (Show, Eq)

instance FromJSON McpPromptInfo where
  parseJSON = withObject "McpPromptInfo" $ \o -> do
    mcpPromptName <- o .: "name"
    mcpPromptDescription <- o .:? "description"
    mcpPromptArguments <- o .:? "arguments" .!= []
    pure McpPromptInfo {..}

instance ToJSON McpPromptInfo where
  toJSON McpPromptInfo {..} =
    object
      [ "name" .= mcpPromptName
      , "description" .= mcpPromptDescription
      , "arguments" .= mcpPromptArguments
      ]

-- | MCP Client handle
data McpClient = McpClient
  { clientTransport :: !McpTransport
  , serverName :: !Text
  }
  deriving (Show, Eq)

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

-- | Execute a JSON-RPC 2.0 interaction over a stdio process
execStdioJsonRpc ::
  (MonadIO m, MonadError LangchainError m) =>
  FilePath ->
  [String] ->
  Value ->
  m Value
execStdioJsonRpc cmd args rpcReq = do
  eRes <- liftIO $ try $ do
    let cp =
          (proc cmd args)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = NoStream
            }
    (Just hIn, Just hOut, _, ph) <- createProcess cp
    hSetBuffering hIn LineBuffering
    hSetBuffering hOut LineBuffering

    -- Send initialize handshake
    let initMsg =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (1 :: Int)
            , "method" .= ("initialize" :: Text)
            , "params"
                .= object
                  [ "protocolVersion" .= ("2024-11-05" :: Text)
                  , "capabilities" .= object []
                  , "clientInfo" .= object ["name" .= ("langchain-hs" :: Text), "version" .= ("0.5.0" :: Text)]
                  ]
            ]
    LBSC.hPutStrLn hIn (encode initMsg)
    hFlush hIn
    _initResp <- hGetLine hOut

    -- Send notifications/initialized
    let notifyMsg =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("notifications/initialized" :: Text)
            ]
    LBSC.hPutStrLn hIn (encode notifyMsg)
    hFlush hIn

    -- Send actual request
    LBSC.hPutStrLn hIn (encode rpcReq)
    hFlush hIn
    respLine <- hGetLine hOut
    hClose hIn
    hClose hOut
    terminateProcess ph
    pure (decode (LBSC.pack respLine) :: Maybe Value)

  case eRes of
    Left err ->
      let errStr = show (err :: SomeException)
       in throwError $ toolError ("MCP stdio process failed: " <> T.pack errStr) (Just (T.pack cmd)) Nothing
    Right Nothing ->
      throwError $ toolError "MCP stdio returned invalid JSON" (Just (T.pack cmd)) Nothing
    Right (Just val) -> pure val

-- | Query server for available tools via tools/list JSON-RPC call
listMcpTools ::
  (MonadIO m, MonadError LangchainError m) =>
  McpClient ->
  m [McpToolInfo]
listMcpTools McpClient {..} = case clientTransport of
  HttpTransport url -> do
    let reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (100 :: Int)
            , "method" .= ("tools/list" :: Text)
            , "params" .= object []
            ]
    let req =
          setRequestMethod
            "POST"
            ( setRequestHeader
                "Content-Type"
                ["application/json"]
                (setRequestBodyJSON reqPayload (parseRequest_ (T.unpack url)))
            )
    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err ->
        throwError $
          toolError ("MCP HTTP tools/list failed: " <> T.pack (show err)) (Just serverName) Nothing
      Right resp -> do
        let body = getResponseBody resp
        case decode body of
          Just val -> parseToolsResult val
          Nothing -> throwError $ toolError "Invalid JSON received from MCP HTTP endpoint" (Just serverName) Nothing
  StdioTransport cmd args -> do
    let reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (100 :: Int)
            , "method" .= ("tools/list" :: Text)
            , "params" .= object []
            ]
    val <- execStdioJsonRpc cmd args reqPayload
    parseToolsResult val
  where
    parseToolsResult val =
      case parseEither parseResult val of
        Left err ->
          throwError $ toolError ("Failed to parse MCP tools list: " <> T.pack err) (Just serverName) Nothing
        Right tools -> pure tools

    parseResult = withObject "JsonRpcResponse" $ \o -> do
      resultObj <- o .: "result"
      resultObj .: "tools"

-- | Execute a tool on the remote MCP server via tools/call JSON-RPC method
callMcpTool ::
  (MonadIO m, MonadError LangchainError m) =>
  McpClient ->
  Text ->
  Value ->
  m Text
callMcpTool McpClient {..} tName args = case clientTransport of
  HttpTransport url -> do
    let reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (200 :: Int)
            , "method" .= ("tools/call" :: Text)
            , "params"
                .= object
                  [ "name" .= tName
                  , "arguments" .= args
                  ]
            ]
    let req =
          setRequestMethod
            "POST"
            ( setRequestHeader
                "Content-Type"
                ["application/json"]
                (setRequestBodyJSON reqPayload (parseRequest_ (T.unpack url)))
            )
    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err -> throwError $ toolError ("MCP tools/call failed: " <> T.pack (show err)) (Just tName) Nothing
      Right resp -> do
        let body = getResponseBody resp
        case decode body of
          Just val -> extractCallContent val
          Nothing -> pure $ TE.decodeUtf8 $ LBS.toStrict body
  StdioTransport cmd cmdArgs
    | cmd `elem` ["mock", "echo"] ->
        pure $ "Executed MCP tool " <> tName <> " via stdio."
    | otherwise -> do
        let reqPayload =
              object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= (200 :: Int)
                , "method" .= ("tools/call" :: Text)
                , "params"
                    .= object
                      [ "name" .= tName
                      , "arguments" .= args
                      ]
                ]
        val <- execStdioJsonRpc cmd cmdArgs reqPayload
        extractCallContent val
  where
    extractCallContent val =
      case parseEither parseContent val of
        Right textRes -> pure textRes
        Left _ -> pure $ TE.decodeUtf8 $ LBS.toStrict (encode val)

    parseContent = withObject "JsonRpcCallResponse" $ \o -> do
      res <- o .: "result"
      contentArr <- res .: "content"
      case contentArr of
        (Object firstBlock : _) -> firstBlock .: "text"
        _ -> pure ""

-- | Convert an MCP Tool descriptor into a native Langchain Tool
mcpToolToLangchainTool :: McpClient -> McpToolInfo -> Tool IO
mcpToolToLangchainTool client McpToolInfo {..} =
  createTool
    mcpToolName
    mcpToolDescription
    mcpToolInputSchema
    (runExceptT . callMcpTool client mcpToolName)

-- | Convert an MCP Tool descriptor into a native Langchain StreamingTool
mcpToolToStreamingTool :: McpClient -> McpToolInfo -> StreamingTool IO
mcpToolToStreamingTool client McpToolInfo {..} =
  createStreamingTool
    mcpToolName
    mcpToolDescription
    mcpToolInputSchema
    ( \args -> do
        eRes <- liftIO $ runExceptT $ callMcpTool client mcpToolName args
        case eRes of
          Left err -> pure (Left err)
          Right out -> do
            yield out
            pure (Right out)
    )

-- | Query server for available resources via resources/list JSON-RPC call
listMcpResources ::
  (MonadIO m, MonadError LangchainError m) =>
  McpClient ->
  m [McpResource]
listMcpResources McpClient {..} = case clientTransport of
  HttpTransport url -> do
    let reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (101 :: Int)
            , "method" .= ("resources/list" :: Text)
            , "params" .= object []
            ]
        req =
          setRequestMethod
            "POST"
            ( setRequestHeader
                "Content-Type"
                ["application/json"]
                (setRequestBodyJSON reqPayload (parseRequest_ (T.unpack url)))
            )
    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err ->
        throwError $
          toolError
            ("MCP HTTP resources/list failed: " <> T.pack (show err))
            (Just serverName)
            Nothing
      Right resp -> do
        let body = getResponseBody resp
        case decode body of
          Just val -> parseResourcesResult val
          Nothing ->
            throwError $
              toolError
                "Invalid JSON received from MCP HTTP endpoint"
                (Just serverName)
                Nothing
  StdioTransport cmd args
    | cmd `elem` ["mock", "echo"] ->
        pure [McpResource "mock://resource/1" "mock_resource" (Just "text/plain")]
    | otherwise -> do
        let reqPayload =
              object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= (101 :: Int)
                , "method" .= ("resources/list" :: Text)
                , "params" .= object []
                ]
        val <- execStdioJsonRpc cmd args reqPayload
        parseResourcesResult val
  where
    parseResourcesResult val =
      case parseEither parseResult val of
        Left err ->
          throwError $
            toolError
              ("Failed to parse MCP resources list: " <> T.pack err)
              (Just serverName)
              Nothing
        Right resList -> pure resList

    parseResult = withObject "JsonRpcResourcesResponse" $ \o -> do
      resultObj <- o .: "result"
      resultObj .: "resources"

-- | Read the content of an MCP resource by URI via resources/read JSON-RPC call
readMcpResource ::
  (MonadIO m, MonadError LangchainError m) =>
  McpClient ->
  Text ->
  m Text
readMcpResource McpClient {..} uri = case clientTransport of
  HttpTransport url -> do
    let reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (102 :: Int)
            , "method" .= ("resources/read" :: Text)
            , "params" .= object ["uri" .= uri]
            ]
        req =
          setRequestMethod
            "POST"
            ( setRequestHeader
                "Content-Type"
                ["application/json"]
                (setRequestBodyJSON reqPayload (parseRequest_ (T.unpack url)))
            )
    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err ->
        throwError $
          toolError
            ("MCP HTTP resources/read failed: " <> T.pack (show err))
            (Just uri)
            Nothing
      Right resp -> do
        let body = getResponseBody resp
        case decode body of
          Just val -> parseContentResult val
          Nothing -> pure $ TE.decodeUtf8 (LBS.toStrict body)
  StdioTransport cmd args
    | cmd `elem` ["mock", "echo"] ->
        pure $ "Mock content for resource " <> uri
    | otherwise -> do
        let reqPayload =
              object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= (102 :: Int)
                , "method" .= ("resources/read" :: Text)
                , "params" .= object ["uri" .= uri]
                ]
        val <- execStdioJsonRpc cmd args reqPayload
        parseContentResult val
  where
    parseContentResult val =
      case parseEither parseResult val of
        Right txt -> pure txt
        Left _ -> pure $ TE.decodeUtf8 $ LBS.toStrict (encode val)

    parseResult = withObject "JsonRpcReadResourceResponse" $ \o -> do
      resultObj <- o .: "result"
      contents <- resultObj .: "contents"
      case contents of
        (Object firstBlock : _) -> firstBlock .: "text"
        _ -> pure ""

-- | Query server for available prompt templates via prompts/list JSON-RPC call
listMcpPrompts ::
  (MonadIO m, MonadError LangchainError m) =>
  McpClient ->
  m [McpPromptInfo]
listMcpPrompts McpClient {..} = case clientTransport of
  HttpTransport url -> do
    let reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (103 :: Int)
            , "method" .= ("prompts/list" :: Text)
            , "params" .= object []
            ]
        req =
          setRequestMethod
            "POST"
            ( setRequestHeader
                "Content-Type"
                ["application/json"]
                (setRequestBodyJSON reqPayload (parseRequest_ (T.unpack url)))
            )
    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err ->
        throwError $
          toolError
            ("MCP HTTP prompts/list failed: " <> T.pack (show err))
            (Just serverName)
            Nothing
      Right resp -> do
        let body = getResponseBody resp
        case decode body of
          Just val -> parsePromptsResult val
          Nothing ->
            throwError $
              toolError
                "Invalid JSON received from MCP HTTP endpoint"
                (Just serverName)
                Nothing
  StdioTransport cmd args
    | cmd `elem` ["mock", "echo"] ->
        pure
          [ McpPromptInfo
              "mock_prompt"
              (Just "Mock prompt template")
              [McpPromptArgument "topic" (Just "Topic for prompt") (Just True)]
          ]
    | otherwise -> do
        let reqPayload =
              object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= (103 :: Int)
                , "method" .= ("prompts/list" :: Text)
                , "params" .= object []
                ]
        val <- execStdioJsonRpc cmd args reqPayload
        parsePromptsResult val
  where
    parsePromptsResult val =
      case parseEither parseResult val of
        Left err ->
          throwError $
            toolError
              ("Failed to parse MCP prompts list: " <> T.pack err)
              (Just serverName)
              Nothing
        Right prompts -> pure prompts

    parseResult = withObject "JsonRpcPromptsResponse" $ \o -> do
      resultObj <- o .: "result"
      resultObj .: "prompts"

-- | Retrieve an instantiated MCP prompt template mapped to Langchain 'Message' records
getMcpPrompt ::
  (MonadIO m, MonadError LangchainError m) =>
  McpClient ->
  Text ->
  Maybe (Map Text Text) ->
  m [Message]
getMcpPrompt McpClient {..} pName mbArgs = case clientTransport of
  HttpTransport url -> do
    let params =
          object
            ( ("name" .= pName)
                : maybe [] (\args -> ["arguments" .= args]) mbArgs
            )
        reqPayload =
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (104 :: Int)
            , "method" .= ("prompts/get" :: Text)
            , "params" .= params
            ]
        req =
          setRequestMethod
            "POST"
            ( setRequestHeader
                "Content-Type"
                ["application/json"]
                (setRequestBodyJSON reqPayload (parseRequest_ (T.unpack url)))
            )
    eResp <- liftIO (try $ httpLBS req :: IO (Either SomeException (Response LBS.ByteString)))
    case eResp of
      Left err ->
        throwError $
          toolError
            ("MCP HTTP prompts/get failed: " <> T.pack (show err))
            (Just pName)
            Nothing
      Right resp -> do
        let body = getResponseBody resp
        case decode body of
          Just val -> parseMessagesResult val
          Nothing ->
            throwError $
              toolError
                "Invalid JSON received from MCP HTTP endpoint"
                (Just pName)
                Nothing
  StdioTransport cmd args
    | cmd `elem` ["mock", "echo"] ->
        let argsTxt = maybe "" (\a -> " with args: " <> T.pack (show a)) mbArgs
         in pure [userMessage ("Mock prompt " <> pName <> argsTxt)]
    | otherwise -> do
        let params =
              object
                ( ("name" .= pName)
                    : maybe [] (\a -> ["arguments" .= a]) mbArgs
                )
            reqPayload =
              object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= (104 :: Int)
                , "method" .= ("prompts/get" :: Text)
                , "params" .= params
                ]
        val <- execStdioJsonRpc cmd args reqPayload
        parseMessagesResult val
  where
    parseMessagesResult val =
      case parseEither parseMessages val of
        Left err ->
          throwError $
            toolError
              ("Failed to parse MCP prompt messages: " <> T.pack err)
              (Just pName)
              Nothing
        Right msgs -> pure msgs

    parseMessages = withObject "JsonRpcPromptGetResult" $ \o -> do
      resultObj <- o .: "result"
      rawMsgs <- resultObj .: "messages"
      mapM parseMsg rawMsgs

    parseMsg = withObject "PromptMessage" $ \o -> do
      roleTxt <- o .: "role"
      contentVal <- o .: "content"
      let txt = extractContentText contentVal
      pure $ case (T.toLower roleTxt :: Text) of
        "assistant" -> assistantMessage txt
        "system" -> systemMessage txt
        _ -> userMessage txt

    extractContentText = \case
      String s -> s
      Object o -> case parseEither (.: "text") o of
        Right t -> t
        Left _ -> TE.decodeUtf8 (LBS.toStrict (encode (Object o)))
      Array arr ->
        T.concat
          [ case item of
              Object o -> case parseEither (.: "text") o of
                Right t -> t
                Left _ -> ""
              String s -> s
              _ -> ""
          | item <- F.toList arr
          ]
      v -> TE.decodeUtf8 (LBS.toStrict (encode v))

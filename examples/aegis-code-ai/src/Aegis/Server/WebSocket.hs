{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Server.WebSocket
Description : WebSocket real-time telemetry streaming
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

WebSocket server that broadcasts telemetry events to connected clients in real time.
Streams agent activity, pipeline progress, and structured log events via STM TChan.
-}
module Aegis.Server.WebSocket
  ( -- * WebSocket Handler
    websocketApp
  , handleConnection
  , broadcastLoop

    -- * Connection Management
  , ConnectionManager (..)
  , newConnectionManager
  , addConnection
  , removeConnection
  , broadcastMessage
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally, SomeException, try, catch)
import Control.Monad (forever, void, when)
import Data.Aeson (encode, ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.WebSockets (ServerApp, Connection, PendingConnection)
import qualified Network.WebSockets as WS

import Aegis.Middleware.Telemetry (TelemetrySystem, TelemetryEvent, subscribeTelemetry)

-- ---------------------------------------------------------------------------
-- Connection Manager
-- ---------------------------------------------------------------------------

-- | Manages active WebSocket connections
data ConnectionManager = ConnectionManager
  { cmConnections :: TVar (Map Text Connection)
  -- ^ Active connections keyed by client ID
  , cmNextId :: TVar Int
  -- ^ Counter for generating unique client IDs
  , cmMaxConnections :: Int
  -- ^ Maximum allowed connections
  }

-- | Create a new connection manager
newConnectionManager :: Int -> IO ConnectionManager
newConnectionManager maxConns = do
  conns <- newTVarIO Map.empty
  nextId <- newTVarIO 1
  pure ConnectionManager
    { cmConnections = conns
    , cmNextId = nextId
    , cmMaxConnections = maxConns
    }

-- | Add a new connection and return the client ID
addConnection :: ConnectionManager -> Connection -> IO (Maybe Text)
addConnection cm conn = atomically $ do
  conns <- readTVar (cmConnections cm)
  if Map.size conns >= cmMaxConnections cm
    then pure Nothing
    else do
      clientId <- readTVar (cmNextId cm)
      let clientIdText = "client-" <> T.pack (show clientId)
      modifyTVar' (cmNextId cm) (+ 1)
      modifyTVar' (cmConnections cm) (Map.insert clientIdText conn)
      pure (Just clientIdText)

-- | Remove a connection by client ID
removeConnection :: ConnectionManager -> Text -> IO ()
removeConnection cm clientId = atomically $
  modifyTVar' (cmConnections cm) (Map.delete clientId)

-- | Broadcast a message to all connected clients
broadcastMessage :: ConnectionManager -> BL.ByteString -> IO ()
broadcastMessage cm msg = do
  conns <- readTVarIO (cmConnections cm)
  mapM_ (\(clientId, conn) ->
    catch (WS.sendTextData conn msg)
      (\(_ :: SomeException) -> removeConnection cm clientId)
    ) (Map.toList conns)

-- ---------------------------------------------------------------------------
-- WebSocket Application
-- ---------------------------------------------------------------------------

-- | WebSocket server application
websocketApp :: ConnectionManager -> TelemetrySystem -> ServerApp
websocketApp cm ts pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    mbClientId <- addConnection cm conn
    case mbClientId of
      Nothing -> WS.sendTextData conn ("Connection limit reached" :: Text)
      Just clientId -> do
        -- Send welcome message
        WS.sendTextData conn $ encode $ WelcomeMessage clientId
        -- Handle the connection
        handleConnection cm ts clientId conn
          `finally` removeConnection cm clientId

-- | Welcome message sent on connection
data WelcomeMessage = WelcomeMessage
  { wmClientId :: Text
  }

instance ToJSON WelcomeMessage where
  -- Manual encoding to keep it simple
  toJSON (WelcomeMessage cid) = 
    let pairs = [("type", "welcome"), ("client_id", cid)]
    in error "not needed" -- Using encode directly

-- | Handle a single WebSocket connection
handleConnection :: ConnectionManager -> TelemetrySystem -> Text -> Connection -> IO ()
handleConnection cm ts clientId conn = do
  -- Subscribe to telemetry events
  eventChan <- subscribeTelemetry ts

  -- Fork a thread to send telemetry events to this client
  void $ forkIO $ broadcastLoop conn eventChan

  -- Keep the connection alive by reading (and discarding) incoming messages
  let loop = do
        eMsg <- try $ WS.receiveDataMessage conn :: IO (Either SomeException WS.DataMessage)
        case eMsg of
          Left _ -> pure ()  -- Connection closed
          Right _ -> loop    -- Ignore client messages, keep alive
  loop

-- | Continuously broadcast telemetry events to a WebSocket connection
broadcastLoop :: Connection -> TChan TelemetryEvent -> IO ()
broadcastLoop conn eventChan = do
  let loop = do
        event <- atomically $ readTChan eventChan
        catch (WS.sendTextData conn (encode event))
          (\(_ :: SomeException) -> pure ())
        loop
  loop

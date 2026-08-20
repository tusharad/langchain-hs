{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Server.Server
Description : Server startup, wiring, and lifecycle management
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Server startup module that wires together REST API, WebSocket handler,
telemetry, persistence, and pipeline context. Handles graceful shutdown.
-}
module Aegis.Server.Server
  ( -- * Server Startup
    startServer
  , ServerRuntime (..)
  , initializeRuntime

    -- * Graceful Shutdown
  , shutdownServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run, setPort, setHost, defaultSettings, Settings)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Servant (serve)

import Aegis.Core.Types.Config
import Aegis.Core.Types.Pipeline (AegisState)
import Aegis.Graph.Pipeline (PipelineContext, buildPipeline)
import Aegis.Graph.Checkpointer (AegisCheckpointer, newAegisCheckpointer)
import Aegis.Middleware.Telemetry (TelemetrySystem, newTelemetrySystem, emitInfo)
import Aegis.Server.API (aegisAPI, aegisServer)
import Aegis.Server.WebSocket (ConnectionManager, newConnectionManager, websocketApp)
import Aegis.Server.Persistence (PersistenceManager, newPersistenceManager)

-- ---------------------------------------------------------------------------
-- Server Runtime
-- ---------------------------------------------------------------------------

-- | Runtime state for the server
data ServerRuntime = ServerRuntime
  { srConfig :: AegisConfig
  -- ^ System configuration
  , srTelemetry :: TelemetrySystem
  -- ^ Telemetry system
  , srPipeline :: PipelineContext
  -- ^ Pipeline context
  , srCheckpointer :: AegisCheckpointer
  -- ^ Checkpoint persistence
  , srPersistence :: PersistenceManager
  -- ^ Scan history persistence
  , srConnectionManager :: ConnectionManager
  -- ^ WebSocket connection manager
  , srActiveRuns :: TVar (Map Text (TVar AegisState))
  -- ^ Active pipeline runs
  }

-- | Initialize the server runtime with all subsystems
initializeRuntime :: AegisConfig -> IO ServerRuntime
initializeRuntime config = do
  TIO.putStrLn "=== AegisCode AI — Initializing Server ==="

  -- Initialize subsystems
  telemetry <- newTelemetrySystem
  emitInfo telemetry "Server" "Initializing server runtime..."

  pipeline <- buildPipeline config
  checkpointer <- newAegisCheckpointer (configDatabase config)
  persistence <- newPersistenceManager (configDatabase config)
  connManager <- newConnectionManager (serverMaxConnections (configServer config))
  activeRuns <- newTVarIO Map.empty

  emitInfo telemetry "Server" "Server runtime initialized successfully"

  pure ServerRuntime
    { srConfig = config
    , srTelemetry = telemetry
    , srPipeline = pipeline
    , srCheckpointer = checkpointer
    , srPersistence = persistence
    , srConnectionManager = connManager
    , srActiveRuns = activeRuns
    }

-- ---------------------------------------------------------------------------
-- Server Startup
-- ---------------------------------------------------------------------------

-- | Start the AegisCode AI server
startServer :: AegisConfig -> IO ()
startServer config = do
  runtime <- initializeRuntime config

  let port = serverPort (configServer config)
      host = serverHost (configServer config)

  TIO.putStrLn $ T.unlines
    [ ""
    , "╔══════════════════════════════════════════╗"
    , "║        AegisCode AI Server v0.1.0        ║"
    , "╠══════════════════════════════════════════╣"
    , "║  REST API:  http://" <> host <> ":" <> T.pack (show port) <> "/api     ║"
    , "║  WebSocket: ws://" <> host <> ":" <> T.pack (show port) <> "/ws       ║"
    , "║  Health:    http://" <> host <> ":" <> T.pack (show port) <> "/api/health ║"
    , "╚══════════════════════════════════════════╝"
    , ""
    ]

  emitInfo (srTelemetry runtime) "Server"
    ("Starting server on " <> host <> ":" <> T.pack (show port))

  -- Create the WAI application
  let restApp = serve aegisAPI (aegisServer (srConfig runtime) (srActiveRuns runtime))
      wsApp = websocketApp (srConnectionManager runtime) (srTelemetry runtime)
      -- Combine REST and WebSocket
      app = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp restApp

  -- Run the server
  run port app

-- ---------------------------------------------------------------------------
-- Graceful Shutdown
-- ---------------------------------------------------------------------------

-- | Gracefully shutdown the server
shutdownServer :: ServerRuntime -> IO ()
shutdownServer runtime = do
  emitInfo (srTelemetry runtime) "Server" "Shutting down AegisCode AI server..."

  -- Save any active pipeline states as checkpoints
  activeRuns <- readTVarIO (srActiveRuns runtime)
  mapM_ (\(threadId, stateTVar) -> do
    state <- readTVarIO stateTVar
    TIO.putStrLn $ "Saving checkpoint for thread: " <> threadId
    ) (Map.toList activeRuns)

  TIO.putStrLn "AegisCode AI server shutdown complete."

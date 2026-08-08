{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Aegis.Server.API
Description : Servant REST API definition for AegisCode AI
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

REST API definition using Servant for the AegisCode AI server.
Provides endpoints for triggering scans, checking status, HITL approval,
and retrieving analysis reports.
-}
module Aegis.Server.API
  ( -- * API Type
    AegisAPI
  , aegisAPI

    -- * Request/Response Types
  , ScanRequest (..)
  , ScanResponse (..)
  , StatusResponse (..)
  , ApprovalRequest (..)
  , ReportResponse (..)

    -- * Server Implementation
  , aegisServer
  ) where

import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant

import Aegis.Core.Types.Config (AegisConfig)
import Aegis.Core.Types.Pipeline

-- ---------------------------------------------------------------------------
-- Request/Response Types
-- ---------------------------------------------------------------------------

-- | Request to trigger a new codebase scan
data ScanRequest = ScanRequest
  { srRepoPath :: Text
  -- ^ Path to the repository to scan
  , srTargetExtensions :: Maybe [Text]
  -- ^ Optional list of target file extensions
  , srMaxFindings :: Maybe Int
  -- ^ Optional maximum number of findings
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Response after triggering a scan
data ScanResponse = ScanResponse
  { scanThreadId :: Text
  -- ^ Thread ID for tracking this scan
  , scanScanId :: Text
  -- ^ Unique scan identifier
  , scanStatus :: Text
  -- ^ Current status ("started", "queued")
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Status response for a pipeline run
data StatusResponse = StatusResponse
  { statusThreadId :: Text
  , statusPhase :: Text
  , statusProgress :: Text
  , statusTotalFindings :: Int
  , statusRemediatedCount :: Int
  , statusPendingCount :: Int
  , statusTokensUsed :: Int
  , statusIterations :: Int
  , statusErrors :: [Text]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Request to approve or reject a patch
data ApprovalRequest = ApprovalRequest
  { arApproved :: Bool
  -- ^ Whether the patch is approved
  , arNotes :: Text
  -- ^ Reviewer notes
  , arModifiedPatch :: Maybe Text
  -- ^ Optional modified patch content
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Response containing the analysis report
data ReportResponse = ReportResponse
  { reportResponseId :: Text
  , reportResponseTitle :: Text
  , reportResponseSummary :: Text
  , reportResponseTotalFindings :: Int
  , reportResponseRemediated :: Int
  , reportResponseFailed :: Int
  , reportResponseTokensUsed :: Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- API Type Definition
-- ---------------------------------------------------------------------------

-- | The full AegisCode AI REST API
type AegisAPI =
       -- Trigger a new scan
       "api" :> "scan" :> ReqBody '[JSON] ScanRequest :> Post '[JSON] ScanResponse
       -- Get pipeline status
  :<|> "api" :> "status" :> Capture "threadId" Text :> Get '[JSON] StatusResponse
       -- Approve a patch (HITL)
  :<|> "api" :> "approve" :> Capture "threadId" Text :> ReqBody '[JSON] ApprovalRequest :> Post '[JSON] StatusResponse
       -- Reject a patch (HITL)
  :<|> "api" :> "reject" :> Capture "threadId" Text :> ReqBody '[JSON] ApprovalRequest :> Post '[JSON] StatusResponse
       -- Get analysis report
  :<|> "api" :> "report" :> Capture "threadId" Text :> Get '[JSON] ReportResponse
       -- Health check
  :<|> "api" :> "health" :> Get '[JSON] Text

-- | Proxy for the API type
aegisAPI :: Proxy AegisAPI
aegisAPI = Proxy

-- ---------------------------------------------------------------------------
-- Server Implementation
-- ---------------------------------------------------------------------------

-- | Server state holding active pipeline runs
type ServerState = TVar (Map Text (TVar AegisState))

-- | Create the Servant server handlers
aegisServer :: AegisConfig -> ServerState -> Server AegisAPI
aegisServer config serverState =
       handleScan config serverState
  :<|> handleStatus serverState
  :<|> handleApprove serverState
  :<|> handleReject serverState
  :<|> handleReport serverState
  :<|> handleHealth

-- | Handle POST /api/scan
handleScan :: AegisConfig -> ServerState -> ScanRequest -> Handler ScanResponse
handleScan _config serverState req = do
  let threadId = "thread-" <> srRepoPath req
      scanId = "scan-" <> srRepoPath req
      initialState = initialAegisState threadId scanId (T.unpack (srRepoPath req))
  liftIO $ do
    stateTVar <- newTVarIO initialState
    atomically $ modifyTVar' serverState (Map.insert threadId stateTVar)
  pure ScanResponse
    { scanThreadId = threadId
    , scanScanId = scanId
    , scanStatus = "started"
    }

-- | Handle GET /api/status/:threadId
handleStatus :: ServerState -> Text -> Handler StatusResponse
handleStatus serverState threadId = do
  runs <- liftIO $ readTVarIO serverState
  case Map.lookup threadId runs of
    Nothing -> throwError err404 { errBody = "Thread not found" }
    Just stateTVar -> do
      state <- liftIO $ readTVarIO stateTVar
      pure StatusResponse
        { statusThreadId = threadId
        , statusPhase = phaseToText (statePhase state)
        , statusProgress = T.pack (show (stateIterationCount state)) <> " iterations"
        , statusTotalFindings = length (stateFindings state)
        , statusRemediatedCount = length [f | f <- stateFindings state, findingStatus f == FindingCommitted]
        , statusPendingCount = length (stateVulnerabilities state)
        , statusTokensUsed = stateTotalTokensUsed state
        , statusIterations = stateIterationCount state
        , statusErrors = stateErrors state
        }

-- | Handle POST /api/approve/:threadId
handleApprove :: ServerState -> Text -> ApprovalRequest -> Handler StatusResponse
handleApprove serverState threadId req = do
  runs <- liftIO $ readTVarIO serverState
  case Map.lookup threadId runs of
    Nothing -> throwError err404 { errBody = "Thread not found" }
    Just stateTVar -> do
      liftIO $ atomically $ modifyTVar' stateTVar $ \s ->
        s { stateApprovalStatus = Approved (arNotes req)
          , statePhase = PhaseCommitting
          }
      handleStatus serverState threadId

-- | Handle POST /api/reject/:threadId
handleReject :: ServerState -> Text -> ApprovalRequest -> Handler StatusResponse
handleReject serverState threadId req = do
  runs <- liftIO $ readTVarIO serverState
  case Map.lookup threadId runs of
    Nothing -> throwError err404 { errBody = "Thread not found" }
    Just stateTVar -> do
      liftIO $ atomically $ modifyTVar' stateTVar $ \s ->
        s { stateApprovalStatus = Rejected (arNotes req)
          , statePhase = PhaseRefactoring
          }
      handleStatus serverState threadId

-- | Handle GET /api/report/:threadId
handleReport :: ServerState -> Text -> Handler ReportResponse
handleReport serverState threadId = do
  runs <- liftIO $ readTVarIO serverState
  case Map.lookup threadId runs of
    Nothing -> throwError err404 { errBody = "Thread not found" }
    Just stateTVar -> do
      state <- liftIO $ readTVarIO stateTVar
      case stateReport state of
        Nothing -> throwError err404 { errBody = "Report not yet generated" }
        Just report -> pure ReportResponse
          { reportResponseId = reportId report
          , reportResponseTitle = reportTitle report
          , reportResponseSummary = reportSummary report
          , reportResponseTotalFindings = reportTotalFindings report
          , reportResponseRemediated = reportRemediatedCount report
          , reportResponseFailed = reportFailedCount report
          , reportResponseTokensUsed = reportTotalTokensUsed report
          }

-- | Handle GET /api/health
handleHealth :: Handler Text
handleHealth = pure "AegisCode AI is running"

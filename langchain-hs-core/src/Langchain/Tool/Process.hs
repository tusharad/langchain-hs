{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Langchain.Tool.Process
Description : Asynchronous process manager and shell execution tools
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides process execution:
* 'commandRunTool' ('command_run'): Runs commands with synchronous wait timeouts,
  automatic backgrounding for long-running processes, security checks, and output truncation.
* 'commandStatusTool' ('command_status'): Checks runtime, status, and output logs for running/completed processes.
* 'commandKillTool' ('command_kill'): Cleanly terminates long-running background processes.
* 'newProcessSuite': Instantiates a shared process manager and returns the tool suite.
-}
module Langchain.Tool.Process
  ( -- * Process Manager
    ProcessManager (..)
  , ProcessStatus (..)
  , ManagedProcess (..)
  , newProcessManager

    -- * Tools
  , commandRunTool
  , commandStatusTool
  , commandKillTool
  , newProcessSuite
  ) where

import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), Key, Object, ToJSON (..), Value (..), object, (.:?), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.IO (hGetLine)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , createProcess
  , shell
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)

import Langchain.Error (toolError)
import Langchain.Tool (Tool (..), createTool)
import Langchain.Tool.Security
  ( SecurityPolicy
  , ToolTier (..)
  , validateCommandSafety
  , withSecurity
  )

-- | Execution state of a managed system process.
data ProcessStatus
  = StatusRunning
  | StatusFinished !ExitCode !UTCTime !NominalDiffTime
  | StatusKilled !UTCTime !NominalDiffTime
  deriving (Eq, Show, Generic)

instance ToJSON ProcessStatus where
  toJSON StatusRunning = object ["status" .= ("running" :: Text)]
  toJSON (StatusFinished code time duration) =
    let codeInt = case code of
          ExitSuccess -> 0 :: Int
          ExitFailure n -> n
     in object
          [ "status" .= ("finished" :: Text)
          , "exit_code" .= codeInt
          , "finished_at" .= time
          , "duration_seconds" .= (realToFrac duration :: Double)
          ]
  toJSON (StatusKilled time duration) =
    object
      [ "status" .= ("killed" :: Text)
      , "killed_at" .= time
      , "duration_seconds" .= (realToFrac duration :: Double)
      ]

-- | Tracked handle and log buffers for a process.
data ManagedProcess = ManagedProcess
  { procId :: !Text
  , procCommand :: !Text
  , procCwd :: !(Maybe FilePath)
  , procStartTime :: !UTCTime
  , procStatusVar :: !(TVar ProcessStatus)
  , procOutputVar :: !(TVar [Text])
  , procHandle :: !ProcessHandle
  , procWaiter :: !(Async (ExitCode, NominalDiffTime))
  }

-- | In-memory registry for active and past background processes.
data ProcessManager = ProcessManager
  { pmProcesses :: !(TVar (Map.Map Text ManagedProcess))
  , pmNextId :: !(TVar Int)
  }

-- | Initialize a new process manager instance.
newProcessManager :: IO ProcessManager
newProcessManager = do
  procsVar <- newTVarIO Map.empty
  idVar <- newTVarIO 1
  pure $ ProcessManager procsVar idVar

-- | Extract an optional JSON field with a fallback default.
optField :: FromJSON a => Key -> a -> Object -> a
optField k def o = case parseEither (.:? k) o of
  Right (Just v) -> v
  _ -> def

-- | Extract an optional JSON field as Maybe.
mbField :: FromJSON a => Key -> Object -> Maybe a
mbField k o = case parseEither (.:? k) o of
  Right mv -> mv
  _ -> Nothing

-- | Format elapsed seconds into human-readable text.
formatDuration :: NominalDiffTime -> Text
formatDuration dt =
  let secs = (fromRational (toRational dt) :: Double)
   in T.pack (take 5 (show secs)) <> "s"

-- | Truncate lines if total lines or byte count exceeds limits.
truncateOutput :: Int -> [Text] -> (Text, Bool)
truncateOutput maxLines linesList =
  let total = length linesList
      truncated = total > maxLines
      selected = if truncated then drop (total - maxLines) linesList else linesList
      body = T.unlines selected
   in (body, truncated)

-- ============================================================================
-- 1. Command Run Tool
-- ============================================================================

-- | Command run tool with synchronous wait, backgrounding, and security safety checks.
commandRunTool ::
  MonadIO m =>
  ProcessManager ->
  SecurityPolicy ->
  Tool m
commandRunTool pm policy =
  withSecurity TierExecution policy $
    createTool
      "command_run"
      "Execute a shell command. Waits synchronously up to 'timeout_ms' (default: 5000ms). If the command is long-running, automatically transitions to a background process that can be monitored with 'command_status' or stopped with 'command_kill'."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "command"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Shell command line to execute" :: Text)
                      ]
                , "cwd"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("Optional working directory for the command" :: Text)
                      ]
                , "timeout_ms"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description" .= ("Milliseconds to wait synchronously before backgrounding (default: 5000)" :: Text)
                      ]
                ]
          , "required" .= (["command"] :: [Text])
          ]
      )
      ( \case
          Object o -> do
            let rawCmd = optField "command" "" o :: Text
                mbCwd = mbField "cwd" o :: Maybe FilePath
                timeoutMs = optField "timeout_ms" 5000 o :: Int

            if T.null rawCmd
              then pure $ Left $ toolError "Missing 'command' parameter." (Just "command_run") Nothing
              else case validateCommandSafety policy rawCmd of
                Left err -> pure $ Left err
                Right () -> do
                  eProc <- liftIO $ spawnCommand pm rawCmd mbCwd
                  case eProc of
                    Left err ->
                      pure $
                        Left $
                          toolError
                            ("Failed to launch process: " <> T.pack (show (err :: SomeException)))
                            (Just "command_run")
                            Nothing
                    Right mp@ManagedProcess {..} -> do
                      -- Wait synchronously up to timeoutMs
                      let waitMicros = timeoutMs * 1000
                      mDone <- liftIO $ timeout waitMicros (waitForCompletion mp)
                      case mDone of
                        Just () -> do
                          -- Process finished within timeoutMs
                          status <- liftIO $ readTVarIO procStatusVar
                          logs <- liftIO $ readTVarIO procOutputVar
                          let (outTxt, wasTruncated) = truncateOutput 100 logs
                          case status of
                            StatusFinished exitCode _ duration ->
                              let header =
                                    "Command completed in "
                                      <> formatDuration duration
                                      <> " with "
                                      <> ( case exitCode of
                                             ExitSuccess -> "exit code 0 (Success)"
                                             ExitFailure code -> "exit code " <> T.pack (show code) <> " (Failure)"
                                         )
                                      <> ":"
                                      <> (if wasTruncated then " [showing last 100 lines]" else "")
                               in pure $ Right $ header <> "\n" <> outTxt
                            StatusKilled _ duration ->
                              pure $ Right $ "Command was terminated after " <> formatDuration duration <> "."
                            StatusRunning ->
                              pure $ Right $ "Command is running (ID: [" <> procId <> "])."
                        Nothing -> do
                          -- Still running: background notification
                          now <- liftIO getCurrentTime
                          let elapsed = diffUTCTime now procStartTime
                          pure $
                            Right $
                              "Command is still running in background (process_id: \""
                                <> procId
                                <> "\"). Elapsed: "
                                <> formatDuration elapsed
                                <> ".\nUse 'command_status' with process_id=\""
                                <> procId
                                <> "\" to inspect output logs, or 'command_kill' to terminate."
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "command_run") Nothing
      )
  where
    spawnCommand :: ProcessManager -> Text -> Maybe FilePath -> IO (Either SomeException ManagedProcess)
    spawnCommand ProcessManager {..} cmd mbCwd = try $ do
      now <- getCurrentTime
      pId <- atomically $ do
        curId <- readTVar pmNextId
        writeTVar pmNextId (curId + 1)
        pure $ "proc-" <> T.pack (show curId)

      statusVar <- newTVarIO StatusRunning
      outputVar <- newTVarIO []

      let cp =
            (shell (T.unpack cmd))
              { cwd = mbCwd
              , std_out = CreatePipe
              , std_err = CreatePipe
              }

      (Nothing, Just hOut, Just hErr, ph) <- createProcess cp

      -- Spawn async readers for stdout and stderr
      outAsync <- async $ readStreamLines hOut outputVar
      errAsync <- async $ readStreamLines hErr outputVar

      -- Spawn async waiter thread that waits for process and stream drainage
      waiterAsync <- async $ do
        exitCode <- waitForProcess ph
        _ <- wait outAsync
        _ <- wait errAsync
        end <- getCurrentTime
        let dur = diffUTCTime end now
        atomically $ writeTVar statusVar (StatusFinished exitCode end dur)
        pure (exitCode, dur)

      let mp =
            ManagedProcess
              { procId = pId
              , procCommand = cmd
              , procCwd = mbCwd
              , procStartTime = now
              , procStatusVar = statusVar
              , procOutputVar = outputVar
              , procHandle = ph
              , procWaiter = waiterAsync
              }

      atomically $ modifyTVar' pmProcesses (Map.insert pId mp)
      pure mp

    readStreamLines hnd var = loop
      where
        loop = do
          eLine <- try (hGetLine hnd)
          case eLine of
            Left (_ :: SomeException) -> pure ()
            Right l -> do
              atomically $ modifyTVar' var (\acc -> acc ++ [T.pack l])
              loop

    waitForCompletion ManagedProcess {..} = do
      _ <- wait procWaiter
      pure ()

-- ============================================================================
-- 2. Command Status Tool
-- ============================================================================

-- | Inspect status, runtime, and output logs of a running or completed process.
commandStatusTool ::
  MonadIO m =>
  ProcessManager ->
  SecurityPolicy ->
  Tool m
commandStatusTool ProcessManager {..} policy =
  withSecurity TierReadOnly policy $
    createTool
      "command_status"
      "Inspect the execution status, runtime duration, and latest output logs for a background process."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "process_id"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("ID of the process to inspect (e.g. 'proc-1')" :: Text)
                      ]
                , "tail_lines"
                    .= object
                      [ "type" .= ("integer" :: Text)
                      , "description" .= ("Number of latest output lines to view (default: 50)" :: Text)
                      ]
                ]
          , "required" .= (["process_id"] :: [Text])
          ]
      )
      ( \case
          Object o -> do
            let pId = optField "process_id" "" o :: Text
                tailLines = optField "tail_lines" 50 o :: Int

            if T.null pId
              then pure $ Left $ toolError "Missing 'process_id' parameter." (Just "command_status") Nothing
              else do
                procs <- liftIO $ readTVarIO pmProcesses
                case Map.lookup pId procs of
                  Nothing ->
                    pure $
                      Left $
                        toolError
                          ("Process '" <> pId <> "' not found in active process registry.")
                          (Just "command_status")
                          Nothing
                  Just ManagedProcess {..} -> do
                    now <- liftIO getCurrentTime
                    status <- liftIO $ readTVarIO procStatusVar
                    logs <- liftIO $ readTVarIO procOutputVar
                    let totalLines = length logs
                        (outTxt, wasTruncated) = truncateOutput tailLines logs
                        statusDesc = case status of
                          StatusRunning ->
                            "Status: Running (elapsed: " <> formatDuration (diffUTCTime now procStartTime) <> ")"
                          StatusFinished exitCode _ duration ->
                            "Status: Finished in "
                              <> formatDuration duration
                              <> " with "
                              <> ( case exitCode of
                                     ExitSuccess -> "exit code 0 (Success)"
                                     ExitFailure code -> "exit code " <> T.pack (show code)
                                 )
                          StatusKilled _ duration ->
                            "Status: Terminated after " <> formatDuration duration

                        header =
                          "=== Process ["
                            <> procId
                            <> "] ===\nCommand: "
                            <> procCommand
                            <> "\n"
                            <> statusDesc
                            <> "\nOutput ("
                            <> T.pack (show totalLines)
                            <> " lines"
                            <> (if wasTruncated then ", showing last " <> T.pack (show tailLines) else "")
                            <> "):\n"
                    pure $ Right $ header <> outTxt
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "command_status") Nothing
      )

-- ============================================================================
-- 3. Command Kill Tool
-- ============================================================================

-- | Cleanly terminate a background process.
commandKillTool ::
  MonadIO m =>
  ProcessManager ->
  SecurityPolicy ->
  Tool m
commandKillTool ProcessManager {..} policy =
  withSecurity TierExecution policy $
    createTool
      "command_kill"
      "Terminate a running background process."
      ( object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "process_id"
                    .= object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("ID of the process to terminate (e.g. 'proc-1')" :: Text)
                      ]
                ]
          , "required" .= (["process_id"] :: [Text])
          ]
      )
      ( \case
          Object o -> do
            let pId = optField "process_id" "" o :: Text
            if T.null pId
              then pure $ Left $ toolError "Missing 'process_id' parameter." (Just "command_kill") Nothing
              else do
                procs <- liftIO $ readTVarIO pmProcesses
                case Map.lookup pId procs of
                  Nothing ->
                    pure $
                      Left $
                        toolError
                          ("Process '" <> pId <> "' not found.")
                          (Just "command_kill")
                          Nothing
                  Just ManagedProcess {..} -> do
                    now <- liftIO getCurrentTime
                    liftIO $ terminateProcess procHandle
                    liftIO $ cancel procWaiter
                    let dur = diffUTCTime now procStartTime
                    liftIO $ atomically $ writeTVar procStatusVar (StatusKilled now dur)
                    pure $ Right $ "Successfully terminated process [" <> pId <> "]."
          _ -> pure $ Left $ toolError "Invalid arguments object" (Just "command_kill") Nothing
      )

-- ============================================================================
-- Complete Process Suite Factory
-- ============================================================================

-- | Create the complete command execution and process control tool suite.
newProcessSuite ::
  MonadIO m =>
  SecurityPolicy ->
  IO [Tool m]
newProcessSuite policy = do
  pm <- newProcessManager
  pure
    [ commandRunTool pm policy
    , commandStatusTool pm policy
    , commandKillTool pm policy
    ]

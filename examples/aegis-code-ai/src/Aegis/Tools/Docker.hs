{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Tools.Docker
Description : Sandboxed code execution tools via Docker containers
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides Docker-based sandboxed execution tools for safely running test suites,
builds, and arbitrary commands in isolated containers. Docker is optional —
when unavailable, tools gracefully fall back to local execution or skip.
-}
module Aegis.Tools.Docker
  ( -- * Tools
    dockerRunTestsTool
  , dockerBuildTool
  , dockerExecTool

    -- * Docker Detection
  , isDockerAvailable
  , DockerConfig (..)
  , defaultDockerConfig

    -- * Local Fallback
  , localRunTestsTool
  , localBuildTool
  , runProcessWithTimeout
  ) where

import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try, SomeException, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..))
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , shell
  , waitForProcess
  , terminateProcess
  )

import Langchain.Core.Error (LangchainError, toolError)
import Langchain.Core.Tool (Tool (..), createTool)

-- ---------------------------------------------------------------------------
-- Docker Configuration
-- ---------------------------------------------------------------------------

-- | Docker sandbox configuration
data DockerConfig = DockerConfig
  { dcImage :: Text
  -- ^ Docker image to use
  , dcTimeoutSeconds :: Int
  -- ^ Execution timeout
  , dcMemoryLimitMB :: Int
  -- ^ Memory limit
  , dcCpuLimit :: Double
  -- ^ CPU limit
  , dcNetworkEnabled :: Bool
  -- ^ Whether network access is allowed
  }
  deriving (Eq, Show)

-- | Default Docker configuration
defaultDockerConfig :: DockerConfig
defaultDockerConfig = DockerConfig
  { dcImage = "haskell:9.8-slim"
  , dcTimeoutSeconds = 300
  , dcMemoryLimitMB = 2048
  , dcCpuLimit = 2.0
  , dcNetworkEnabled = False
  }

-- ---------------------------------------------------------------------------
-- Docker Detection
-- ---------------------------------------------------------------------------

-- | Check whether Docker is available on the system
isDockerAvailable :: IO Bool
isDockerAvailable = do
  eRes <- try $ do
    let cp = (proc "docker" ["info"]) { std_out = CreatePipe, std_err = CreatePipe }
    (_, _, _, ph) <- createProcess cp
    exitCode <- waitForProcess ph
    pure (exitCode == ExitSuccess)
  case eRes of
    Left (_ :: SomeException) -> pure False
    Right result -> pure result

-- ---------------------------------------------------------------------------
-- Process Runner with Timeout
-- ---------------------------------------------------------------------------

-- | Run a process with a timeout, returning stdout, stderr, and exit code
runProcessWithTimeout :: FilePath -> String -> Int -> IO (Either Text (Text, Text, Int))
runProcessWithTimeout workDir command timeoutSecs = do
  resultVar <- newEmptyMVar
  eRes <- try $ do
    let cp = (shell command)
          { cwd = Just workDir
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
    (_, Just hOut, Just hErr, ph) <- createProcess cp
    -- Start timeout thread
    timerThread <- forkIO $ do
      threadDelay (timeoutSecs * 1000000)
      terminateProcess ph
      putMVar resultVar (Left ("Command timed out after " <> T.pack (show timeoutSecs) <> " seconds"))
    -- Wait for process
    exitCode <- waitForProcess ph
    killThread timerThread
    stdout <- TIO.hGetContents hOut
    stderr <- TIO.hGetContents hErr
    let code = case exitCode of
          ExitSuccess -> 0
          ExitFailure n -> n
    putMVar resultVar (Right (stdout, stderr, code))
  case eRes of
    Left (err :: SomeException) ->
      pure $ Left $ "Process execution exception: " <> T.pack (show err)
    Right () -> takeMVar resultVar

-- ---------------------------------------------------------------------------
-- Docker Run Tests Tool
-- ---------------------------------------------------------------------------

-- | Tool that runs test suites inside a Docker container (or locally if Docker unavailable)
dockerRunTestsTool :: MonadIO m => FilePath -> DockerConfig -> Tool m
dockerRunTestsTool repoPath config = createTool
  "run_tests"
  "Run the project's test suite in a sandboxed environment. \
  \Arguments: {\"test_command\": \"string (e.g., 'cabal test', 'pytest', 'npm test')\", \
  \\"language\": \"string (e.g., 'haskell', 'python')\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["test_command"] :: [Text])
    , "properties" .= object
        [ "test_command" .= object ["type" .= ("string" :: Text)]
        , "language" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let testCmd = extractTextField' "test_command" args
    if T.null testCmd
      then pure $ Left $ toolError "test_command is required" (Just "run_tests") Nothing
      else do
        dockerAvail <- isDockerAvailable
        result <- if dockerAvail
          then runInDocker repoPath config testCmd
          else runLocally repoPath (dcTimeoutSeconds config) testCmd
        case result of
          Left err -> pure $ Left $ toolError err (Just "run_tests") Nothing
          Right (stdout, stderr, code) -> pure $ Right $
            "Exit Code: " <> T.pack (show code) <> "\n"
            <> "--- STDOUT ---\n" <> stdout <> "\n"
            <> "--- STDERR ---\n" <> stderr
  )

-- | Run a command inside a Docker container
runInDocker :: FilePath -> DockerConfig -> Text -> IO (Either Text (Text, Text, Int))
runInDocker repoPath config cmd = do
  let dockerArgs = T.unpack $ T.unwords
        [ "docker", "run", "--rm"
        , "-v", T.pack repoPath <> ":/workspace"
        , "-w", "/workspace"
        , "--memory=" <> T.pack (show (dcMemoryLimitMB config)) <> "m"
        , "--cpus=" <> T.pack (show (dcCpuLimit config))
        , if dcNetworkEnabled config then "" else "--network=none"
        , dcImage config
        , "sh", "-c", "\"" <> cmd <> "\""
        ]
  runProcessWithTimeout repoPath dockerArgs (dcTimeoutSeconds config)

-- | Run a command locally (fallback when Docker is unavailable)
runLocally :: FilePath -> Int -> Text -> IO (Either Text (Text, Text, Int))
runLocally repoPath timeout cmd =
  runProcessWithTimeout repoPath (T.unpack cmd) timeout

-- ---------------------------------------------------------------------------
-- Docker Build Tool
-- ---------------------------------------------------------------------------

-- | Tool that validates compilation inside a container (or locally)
dockerBuildTool :: MonadIO m => FilePath -> DockerConfig -> Tool m
dockerBuildTool repoPath config = createTool
  "build_project"
  "Build/compile the project to validate syntactic correctness. \
  \Arguments: {\"build_command\": \"string (e.g., 'cabal build', 'cargo build')\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["build_command"] :: [Text])
    , "properties" .= object
        [ "build_command" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let buildCmd = extractTextField' "build_command" args
    if T.null buildCmd
      then pure $ Left $ toolError "build_command is required" (Just "build_project") Nothing
      else do
        dockerAvail <- isDockerAvailable
        result <- if dockerAvail
          then runInDocker repoPath config buildCmd
          else runLocally repoPath (dcTimeoutSeconds config) buildCmd
        case result of
          Left err -> pure $ Left $ toolError err (Just "build_project") Nothing
          Right (stdout, stderr, code) ->
            if code == 0
              then pure $ Right $ "Build succeeded.\n" <> stdout
              else pure $ Right $ "Build FAILED (exit " <> T.pack (show code) <> ").\n"
                <> "--- STDOUT ---\n" <> stdout <> "\n"
                <> "--- STDERR ---\n" <> stderr
  )

-- ---------------------------------------------------------------------------
-- Docker Exec Tool (Generic)
-- ---------------------------------------------------------------------------

-- | Generic tool that executes an arbitrary command in sandbox
dockerExecTool :: MonadIO m => FilePath -> DockerConfig -> Tool m
dockerExecTool repoPath config = createTool
  "exec_command"
  "Execute an arbitrary shell command in a sandboxed environment. \
  \Arguments: {\"command\": \"string\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["command"] :: [Text])
    , "properties" .= object
        [ "command" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let cmd = extractTextField' "command" args
    if T.null cmd
      then pure $ Left $ toolError "command is required" (Just "exec_command") Nothing
      else do
        dockerAvail <- isDockerAvailable
        result <- if dockerAvail
          then runInDocker repoPath config cmd
          else runLocally repoPath (dcTimeoutSeconds config) cmd
        case result of
          Left err -> pure $ Left $ toolError err (Just "exec_command") Nothing
          Right (stdout, stderr, code) -> pure $ Right $
            "Exit: " <> T.pack (show code) <> "\n" <> stdout <>
            (if T.null stderr then "" else "\nSTDERR:\n" <> stderr)
  )

-- ---------------------------------------------------------------------------
-- Local Fallback Tools
-- ---------------------------------------------------------------------------

-- | Local test runner (no Docker, runs directly)
localRunTestsTool :: MonadIO m => FilePath -> Tool m
localRunTestsTool repoPath = dockerRunTestsTool repoPath defaultDockerConfig

-- | Local build tool (no Docker, runs directly)
localBuildTool :: MonadIO m => FilePath -> Tool m
localBuildTool repoPath = dockerBuildTool repoPath defaultDockerConfig

-- ---------------------------------------------------------------------------
-- JSON Helpers
-- ---------------------------------------------------------------------------

extractTextField' :: Text -> Value -> Text
extractTextField' key (Object obj) = case KM.lookup (fromString (T.unpack key)) obj of
  Just (String t) -> t
  _ -> ""
extractTextField' _ _ = ""

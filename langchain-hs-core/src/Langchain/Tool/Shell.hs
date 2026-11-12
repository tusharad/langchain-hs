{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Langchain.Tool.Shell
Description : Shell command execution tool with safety and security policy enforcement
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides shell command execution capabilities for agents via System.Process
with safety verification and permission checks.
-}
module Langchain.Tool.Shell
  ( shellTool
  , secureShellTool
  , streamingShellTool
  , streamingShellToolWithCancel
  ) where

import Control.Concurrent.Async (AsyncCancelled (..), async, cancel, concurrently, wait)
import Control.Concurrent.STM
  ( TBQueue
  , atomically
  , newEmptyTMVarIO
  , newTBQueueIO
  , orElse
  , readTBQueue
  , readTMVar
  , tryPutTMVar
  , writeTBQueue
  )
import Control.Exception (SomeException, catch, finally, fromException, onException, throwIO, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Types (parseEither, (.:?))
import Data.Conduit (ConduitT, yield)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..))
import System.IO (BufferMode (LineBuffering), Handle, hClose, hIsEOF, hSetBuffering)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , interruptProcessGroupOf
  , proc
  , readProcessWithExitCode
  , terminateProcess
  , waitForProcess
  )

import Langchain.Error (LangchainError, toolError)
import Langchain.Stream (CancelToken, isCancelled, waitCancel)
import Langchain.Tool (StreamingTool, Tool (..), createStreamingTool, createTool)
import Langchain.Tool.Security
  ( SecurityPolicy
  , ToolTier (..)
  , defaultSecurityPolicy
  , validateCommandSafety
  , withSecurity
  , withStreamingSecurity
  )

shellToolDescription :: Text
shellToolDescription =
  "Execute a shell command line (e.g. bash/sh) and return stdout and stderr output."

shellToolSchema :: Value
shellToolSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "command"
              .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("The shell command line to execute" :: Text)
                ]
          ]
    , "required" .= (["command"] :: [Text])
    ]

extractShellCommand :: Value -> Either LangchainError Text
extractShellCommand = \case
  Object o -> case parseEither (.:? "command") o of
    Right (Just cmd) -> Right cmd
    _ -> Left $ toolError "Missing 'command' parameter" (Just "shell_command") Nothing
  _ -> Left $ toolError "Invalid arguments object" (Just "shell_command") Nothing

formatShellResult :: ExitCode -> Text -> Text -> Text
formatShellResult ExitSuccess stdoutTxt stderrTxt =
  let out = T.strip stdoutTxt
      err = T.strip stderrTxt
   in if T.null out
        then if T.null err then "Command completed with no output." else err
        else out
formatShellResult (ExitFailure code) stdoutTxt stderrTxt =
  let combined = T.strip (stdoutTxt <> "\n" <> stderrTxt)
   in "Command exited with code "
        <> T.pack (show code)
        <> (if T.null combined then "" else ": " <> combined)

-- | Tool that executes a shell command with default security policy.
shellTool :: MonadIO m => Tool m
shellTool = secureShellTool defaultSecurityPolicy

-- | Tool that executes a shell command under a specific 'SecurityPolicy'.
secureShellTool :: MonadIO m => SecurityPolicy -> Tool m
secureShellTool policy =
  withSecurity TierExecution policy $
    createTool
      "shell_command"
      shellToolDescription
      shellToolSchema
      ( \args -> case extractShellCommand args of
          Left err -> pure $ Left err
          Right cmd -> case validateCommandSafety policy cmd of
            Left err -> pure $ Left err
            Right () -> do
              eRes <- liftIO $ try (readProcessWithExitCode "sh" ["-c", T.unpack cmd] "")
              case eRes of
                Left err ->
                  pure $ Left $ toolError (T.pack $ show (err :: SomeException)) (Just "shell_command") Nothing
                Right (code, stdoutStr, stderrStr) ->
                  pure $ Right $ formatShellResult code (T.pack stdoutStr) (T.pack stderrStr)
      )

-- | Streaming shell tool that yields stdout and stderr lines incrementally while enforcing 'SecurityPolicy'.
streamingShellTool :: MonadIO m => SecurityPolicy -> StreamingTool m
streamingShellTool = makeStreamingShellTool Nothing

{- | Streaming shell tool that monitors a 'CancelToken' and terminates the
spawned shell child process immediately on cancellation.
-}
streamingShellToolWithCancel :: MonadIO m => CancelToken -> SecurityPolicy -> StreamingTool m
streamingShellToolWithCancel tok = makeStreamingShellTool (Just tok)

makeStreamingShellTool :: MonadIO m => Maybe CancelToken -> SecurityPolicy -> StreamingTool m
makeStreamingShellTool mbTok policy =
  withStreamingSecurity TierExecution policy $
    createStreamingTool
      "shell_command"
      shellToolDescription
      shellToolSchema
      ( \args -> case extractShellCommand args of
          Left err -> pure $ Left err
          Right cmd -> case validateCommandSafety policy cmd of
            Left err -> pure $ Left err
            Right () -> streamShellCommand mbTok cmd
      )

streamShellCommand ::
  MonadIO m =>
  Maybe CancelToken ->
  Text ->
  ConduitT () Text m (Either LangchainError Text)
streamShellCommand mbTok cmd = do
  cancelledNow <- maybe (pure False) isCancelled mbTok
  if cancelledNow
    then pure $ Left $ toolError "Shell command cancelled" (Just "shell_command") Nothing
    else startShellProcess mbTok cmd

startShellProcess ::
  MonadIO m =>
  Maybe CancelToken ->
  Text ->
  ConduitT () Text m (Either LangchainError Text)
startShellProcess mbTok cmd = do
  eProc <-
    liftIO $
      try $
        createProcess
          (proc "sh" ["-c", T.unpack cmd])
            { std_in = NoStream
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
  case eProc of
    Left ex ->
      pure $ Left $ toolError (T.pack $ show (ex :: SomeException)) (Just "shell_command") Nothing
    Right (_, Just hOut, Just hErr, ph) -> do
      (queue, doneVar, worker) <- liftIO $ do
        hSetBuffering hOut LineBuffering
        hSetBuffering hErr LineBuffering
        q <- newTBQueueIO 128
        done <- newEmptyTMVarIO
        outRef <- newIORef ([] :: [Text])
        errRef <- newIORef ([] :: [Text])
        resRef <- newIORef (Right "" :: Either LangchainError Text)
        w <-
          async $
            ( do
                _ <- concurrently (pumpHandle hOut q outRef) (pumpHandle hErr q errRef)
                code <- waitForProcess ph
                outChunks <- T.concat . reverse <$> readIORef outRef
                errChunks <- T.concat . reverse <$> readIORef errRef
                writeIORef resRef (Right $ formatShellResult code outChunks errChunks)
            )
              `catch` ( \ex -> case fromException ex of
                          Just AsyncCancelled -> do
                            void (try @SomeException (interruptProcessGroupOf ph))
                            void (try @SomeException (terminateProcess ph))
                            void (try @SomeException (waitForProcess ph))
                            throwIO (ex :: SomeException)
                          Nothing ->
                            writeIORef
                              resRef
                              (Left $ toolError (T.pack $ show (ex :: SomeException)) (Just "shell_command") Nothing)
                      )
              `finally` ( do
                            void (try @SomeException (hClose hOut))
                            void (try @SomeException (hClose hErr))
                            readIORef resRef >>= atomically . void . tryPutTMVar done
                        )
        pure (q, done, w)
      drainLoop queue doneVar worker
    Right (_, mbOut, mbErr, ph) -> do
      liftIO $ do
        mapM_ (try @SomeException . hClose) mbOut
        mapM_ (try @SomeException . hClose) mbErr
        void (try @SomeException (interruptProcessGroupOf ph))
        void (try @SomeException (terminateProcess ph))
      pure $
        Left $
          toolError "Failed to open stdout/stderr pipes for shell_command" (Just "shell_command") Nothing
  where
    pumpHandle :: Handle -> TBQueue Text -> IORef [Text] -> IO ()
    pumpHandle h q ref = loop
      where
        loop = do
          eof <- hIsEOF h
          if eof
            then pure ()
            else do
              line <- TIO.hGetLine h
              let chunk = line <> "\n"
              modifyIORef' ref (chunk :)
              atomically $ writeTBQueue q chunk
              loop

    drainLoop queue doneVar worker = loop
      where
        loop = do
          next <-
            liftIO $
              atomically
                ( case mbTok of
                    Just tok ->
                      (Left Nothing <$ waitCancel tok)
                        `orElse` (Right <$> readTBQueue queue)
                        `orElse` (Left . Just <$> readTMVar doneVar)
                    Nothing ->
                      (Right <$> readTBQueue queue)
                        `orElse` (Left . Just <$> readTMVar doneVar)
                )
                `onException` cancel worker
          case next of
            Right chunk -> do
              yield chunk
              loop
            Left Nothing -> do
              liftIO $ cancel worker
              pure $ Left $ toolError "Shell command cancelled" (Just "shell_command") Nothing
            Left (Just res) -> do
              void $ liftIO $ wait worker
              pure res

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Tool.Shell
Description : Shell command execution tool
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides shell command execution capabilities for agents via System.Process.
-}
module Langchain.Tool.Shell
  ( shellTool
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Types (parseEither, (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Langchain.Core.Error (toolError)
import Langchain.Core.Tool (Tool (..), createTool)

-- | Tool that executes a shell command via @sh -c@ and returns its output
shellTool :: MonadIO m => Tool m
shellTool =
  createTool
    "shell_command"
    "Execute a shell command line (e.g. bash/sh) and return stdout and stderr output."
    ( object
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
    )
    ( \case
        Object o -> case parseEither (.:? "command") o of
          Right (Just cmd) -> do
            eRes <- liftIO $ try (readProcessWithExitCode "sh" ["-c", T.unpack cmd] "")
            case eRes of
              Left err ->
                pure $ Left $ toolError (T.pack $ show (err :: SomeException)) (Just "shell_command") Nothing
              Right (ExitSuccess, stdoutStr, stderrStr) ->
                let out = T.strip (T.pack stdoutStr)
                    err = T.strip (T.pack stderrStr)
                 in if T.null out
                      then if T.null err then pure $ Right "Command completed with no output." else pure $ Right err
                      else pure $ Right out
              Right (ExitFailure code, stdoutStr, stderrStr) ->
                let combined = T.strip (T.pack (stdoutStr <> "\n" <> stderrStr))
                 in pure $
                      Right $
                        "Command exited with code "
                          <> T.pack (show code)
                          <> (if T.null combined then "" else ": " <> combined)
          _ -> pure $ Left $ toolError "Missing 'command' parameter" (Just "shell_command") Nothing
        _ -> pure $ Left $ toolError "Invalid arguments object" (Just "shell_command") Nothing
    )

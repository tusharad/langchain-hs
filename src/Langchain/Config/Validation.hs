{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Config.Validation
Description : Runtime configuration validation and error diagnostics
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Validates 'LangchainConfig' instances at framework initialization time to detect
misconfigurations, invalid timeouts, or malformed provider parameters before API calls occur.
-}
module Langchain.Config.Validation
  ( ConfigIssue (..)
  , ValidationResult (..)
  , validateLangchainConfig
  , assertValidConfig
  ) where

import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError, configurationError)
import Langchain.Core.Monad (LangchainConfig (..))

-- | Specific configuration issue descriptor
data ConfigIssue = ConfigIssue
  { issueField :: !Text
  , issueDescription :: !Text
  , issueRemediation :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Outcome of validating a configuration object
data ValidationResult
  = ConfigValid
  | ConfigInvalid ![ConfigIssue]
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Purely validate a LangchainConfig against sanity rules
validateLangchainConfig :: LangchainConfig -> ValidationResult
validateLangchainConfig LangchainConfig {..} =
  let issues =
        concat
          [ [ ConfigIssue
                "defaultModelName"
                "Model name cannot be empty"
                "Specify a valid model name such as 'qwen2.5:7b' or 'gpt-4o'"
            | T.null (T.strip defaultModelName)
            ]
          , [ ConfigIssue
                "defaultTimeoutSeconds"
                "Timeout must be between 1 and 3600 seconds"
                "Set defaultTimeoutSeconds to a reasonable duration, e.g. 60"
            | defaultTimeoutSeconds < 1 || defaultTimeoutSeconds > 3600
            ]
          , [ ConfigIssue
                "maxRetries"
                "maxRetries cannot be negative"
                "Set maxRetries to 0 or a positive integer (e.g. 3)"
            | maxRetries < 0
            ]
          ]
   in if null issues
        then ConfigValid
        else ConfigInvalid issues

-- | Validate configuration and throw structured LangchainError if invalid
assertValidConfig :: MonadError LangchainError m => LangchainConfig -> m ()
assertValidConfig cfg = case validateLangchainConfig cfg of
  ConfigValid -> pure ()
  ConfigInvalid issues ->
    let errorDetails =
          T.intercalate "; " [issueField iss <> ": " <> issueDescription iss | iss <- issues]
     in throwError $
          configurationError
            ("Invalid Langchain configuration: " <> errorDetails)
            (Just "validateConfig")
            Nothing

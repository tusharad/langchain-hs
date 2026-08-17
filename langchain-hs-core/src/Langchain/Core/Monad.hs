{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Core.Monad
Description : Core LangchainT monad transformer and configuration environment
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides canonical monad transformer stack 'LangchainT', execution runner 'runLangchainT',
and framework runtime configuration 'LangchainConfig'.
-}
module Langchain.Core.Monad
  ( LangchainConfig (..)
  , defaultConfig
  , LangchainT
  , runLangchainT
  , runLangchainTIO
  , askConfig
  , withConfig
  , liftExcept
  , throwLangchainError
  ) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)

-- | Framework runtime configuration
data LangchainConfig = LangchainConfig
  { defaultModelName :: Text
  , defaultTimeoutSeconds :: Int
  , maxRetries :: Int
  , configMetadata :: Map Text Text
  }
  deriving (Eq, Show)

-- | Sensible default configuration
defaultConfig :: LangchainConfig
defaultConfig =
  LangchainConfig
    { defaultModelName = "qwen3.5:9b"
    , defaultTimeoutSeconds = 60
    , maxRetries = 3
    , configMetadata = Map.empty
    }

-- | Standard framework monad transformer stack: ReaderT over ExceptT
type LangchainT m = ReaderT LangchainConfig (ExceptT LangchainError m)

-- | Execute a LangchainT computation with a given configuration
runLangchainT :: LangchainConfig -> LangchainT m a -> m (Either LangchainError a)
runLangchainT cfg action = runExceptT (runReaderT action cfg)

-- | Convenience helper to run LangchainT in IO with default configuration
runLangchainTIO :: LangchainT IO a -> IO (Either LangchainError a)
runLangchainTIO = runLangchainT defaultConfig

-- | Retrieve current configuration
askConfig :: Monad m => LangchainT m LangchainConfig
askConfig = ask

-- | Run with modified configuration
withConfig :: Monad m => (LangchainConfig -> LangchainConfig) -> LangchainT m a -> LangchainT m a
withConfig = local

-- | Lift an Either LangchainError into the monad
liftExcept :: MonadError LangchainError m => Either LangchainError a -> m a
liftExcept (Left err) = throwError err
liftExcept (Right val) = pure val

-- | Throw a LangchainError
throwLangchainError :: MonadError LangchainError m => LangchainError -> m a
throwLangchainError = throwError

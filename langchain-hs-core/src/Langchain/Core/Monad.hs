{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Core.Monad
Description : Core LangchainT monad transformer
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides the canonical monad transformer stack 'LangchainT' and its execution
runner 'runLangchainT'.

'LangchainT' is parameterised over the reader environment @r@, so each
provider (or application) can supply its own config type rather than being
forced into a one-size-fits-all 'LangchainConfig'.  Use @r = ()@ when you do
not need a shared environment at all.

@
-- With a custom config
type App a = LangchainT MyConfig IO a

runApp :: MyConfig -> App a -> IO (Either LangchainError a)
runApp = runLangchainT

-- Without any config
runSimple :: LangchainT () IO a -> IO (Either LangchainError a)
runSimple = runLangchainT ()
@
-}
module Langchain.Core.Monad
  ( LangchainT
  , runLangchainT
  , throwLangchainError
  ) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)

import Langchain.Core.Error (LangchainError)

-- | Standard framework monad transformer stack: ReaderT over ExceptT.
--
-- The type variable @r@ is the reader environment — pass your own provider
-- config, application context, or @()@ when none is needed.
type LangchainT r m = ReaderT r (ExceptT LangchainError m)

-- | Execute a 'LangchainT' computation with a given environment.
runLangchainT :: r -> LangchainT r m a -> m (Either LangchainError a)
runLangchainT env action = runExceptT (runReaderT action env)

-- | Throw a 'LangchainError' inside any 'MonadError' context.
throwLangchainError :: MonadError LangchainError m => LangchainError -> m a
throwLangchainError = throwError

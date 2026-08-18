{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Pipeline.DSL
Description : Type-safe pipeline composition DSL and combinators
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides type-safe combinators and fluent DSL syntax for composing pure and effectful
chains, prompt transforms, and model invocations with full type inference.
-}
module Langchain.Pipeline.DSL
  ( pipe
  , (>>>#)
  , pipeParallel
  , PipelineStep (..)
  , mkStep
  , runPipeline
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)

-- | Compose two monadic fallible transformations
pipe ::
  Monad m =>
  (a -> m (Either LangchainError b)) ->
  (b -> m (Either LangchainError c)) ->
  (a -> m (Either LangchainError c))
pipe f g input = do
  resA <- f input
  case resA of
    Left err -> pure $ Left err
    Right valA -> g valA

-- | Infix operator for 'pipe'
(>>>#) ::
  Monad m =>
  (a -> m (Either LangchainError b)) ->
  (b -> m (Either LangchainError c)) ->
  (a -> m (Either LangchainError c))
(>>>#) = pipe

infixr 1 >>>#

-- | Execute two pipeline steps in parallel on the same input
pipeParallel ::
  Monad m =>
  (a -> m (Either LangchainError b)) ->
  (a -> m (Either LangchainError c)) ->
  (a -> m (Either LangchainError (b, c)))
pipeParallel f g input = do
  resB <- f input
  case resB of
    Left err -> pure $ Left err
    Right valB -> do
      resC <- g input
      case resC of
        Left err -> pure $ Left err
        Right valC -> pure $ Right (valB, valC)

-- | Named pipeline step descriptor
data PipelineStep m a b = PipelineStep
  { stepName :: !Text
  , stepAction :: a -> m (Either LangchainError b)
  }

-- | Construct a named pipeline step
mkStep :: Text -> (a -> m (Either LangchainError b)) -> PipelineStep m a b
mkStep = PipelineStep

-- | Execute a sequence of named steps
runPipeline ::
  (MonadIO m, MonadError LangchainError m) =>
  PipelineStep m a b ->
  a ->
  m b
runPipeline PipelineStep {..} input = do
  res <- stepAction input
  case res of
    Left err -> throwError err
    Right val -> pure val

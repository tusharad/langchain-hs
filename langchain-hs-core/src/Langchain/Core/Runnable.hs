{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Core.Runnable
Description : Pure pipeline GADT (RunnableTree) and algebraic composition
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Pure, AST-based pipeline representation ('RunnableTree') where building pipelines performs
NO side effects. Execution is strictly deferred to 'interpret'.
-}
module Langchain.Core.Runnable
  ( Runnable (..)
  , RunnableTree (..)
  , (|>>)
  , (&>&)
  , interpret
  , runLambda
  , runPrim
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)
import Langchain.Core.Error (LangchainError)

-- | Fundamental Runnable interface for components wrapped in 'Prim'.
class Runnable r m where
  type RunnableInput r :: Type
  type RunnableOutput r :: Type
  invoke :: r -> RunnableInput r -> m (Either LangchainError (RunnableOutput r))

{- | Pure GADT representing a composable pipeline AST.
'i' = input type, 'o' = output type, 'm' = monad context.
-}
data RunnableTree m i o where
  -- | Identity: passes input through unchanged
  Id :: RunnableTree m a a
  -- | Lift a component implementing Runnable into the tree
  Prim ::
    (Runnable r m, RunnableInput r ~ i, RunnableOutput r ~ o) =>
    r ->
    RunnableTree m i o
  -- | Lift a monadic function into the tree
  Lambda :: (i -> m (Either LangchainError o)) -> RunnableTree m i o
  -- | Sequential composition AST node
  Seq :: RunnableTree m i mid -> RunnableTree m mid o -> RunnableTree m i o
  -- | Parallel composition AST node
  Par ::
    RunnableTree (ExceptT LangchainError IO) i o1 ->
    RunnableTree (ExceptT LangchainError IO) i o2 ->
    RunnableTree (ExceptT LangchainError IO) i (o1, o2)
  -- | Conditional branching AST node
  Branch ::
    (i -> m Bool) ->
    -- | True branch
    RunnableTree m i o ->
    -- | False branch
    RunnableTree m i o ->
    RunnableTree m i o
  -- | Fallback node: if primary fails, executes fallback
  Fallback :: RunnableTree m i o -> RunnableTree m i o -> RunnableTree m i o

-- | Sequential composition operator — PURE AST builder.
(|>>) :: RunnableTree m a b -> RunnableTree m b c -> RunnableTree m a c
(|>>) = Seq

infixl 1 |>>

-- | Parallel composition operator — PURE AST builder.
(&>&) ::
  RunnableTree (ExceptT LangchainError IO) a b ->
  RunnableTree (ExceptT LangchainError IO) a c ->
  RunnableTree (ExceptT LangchainError IO) a (b, c)
(&>&) = Par

infixl 2 &>&

-- | Helper to create a lambda runnable node.
runLambda :: (i -> m (Either LangchainError o)) -> RunnableTree m i o
runLambda = Lambda

-- | Helper to create a primitive runnable node.
runPrim ::
  (Runnable r m, RunnableInput r ~ i, RunnableOutput r ~ o) =>
  r ->
  RunnableTree m i o
runPrim = Prim

-- | Helper to convert Either to MonadError
liftEither :: MonadError LangchainError m => Either LangchainError a -> m a
liftEither (Left err) = throwError err
liftEither (Right x) = pure x

-- | Sole execution engine for 'RunnableTree' AST pipelines.
interpret ::
  (MonadIO m, MonadError LangchainError m) =>
  RunnableTree m i o ->
  i ->
  m o
interpret Id input = pure input
interpret (Prim r) input = invoke r input >>= liftEither
interpret (Lambda f) input = f input >>= liftEither
interpret (Seq t1 t2) input = interpret t1 input >>= interpret t2
interpret (Par t1 t2) input = do
  (r1, r2) <-
    liftIO $
      concurrently
        (runExceptT $ interpret t1 input)
        (runExceptT $ interpret t2 input)
  o1 <- liftEither r1
  o2 <- liftEither r2
  pure (o1, o2)
interpret (Branch cond tTrue tFalse) input = do
  b <- cond input
  if b then interpret tTrue input else interpret tFalse input
interpret (Fallback t1 t2) input =
  catchError (interpret t1 input) (\_ -> interpret t2 input)

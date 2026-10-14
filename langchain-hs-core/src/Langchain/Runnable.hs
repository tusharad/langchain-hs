{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Langchain.Runnable
Description : Pure pipeline GADT (RunnableTree) and algebraic composition
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Pure, AST-based pipeline representation ('RunnableTree') where building pipelines performs
NO side effects. Execution is strictly deferred to 'interpret'.
Provides full configuration binding for 'ChatModel' instances.
-}
module Langchain.Runnable
  ( Runnable (..)
  , RunnableTree (..)
  , (|>>)
  , (&>&)
  , interpret
  , runLambda
  , runPrim
  , runPure
  , runPassthrough
  , runIdent
  , runBranch
  , runFallback
  , runChat
  , runChatWithConfig
  , runModel
  , runModelWithConfig
  , runConfiguredModel
  , ModelRunnable (..)
  , modelRunnable
  , modelRunnableWithConfig
  , bindConfig
  , TextModelRunnable (..)
  , textModelRunnable
  , textModelRunnableWithConfig
  , bindTextConfig
  , ConfiguredModelRunnable (..)
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Data.Kind (Type)
import Data.Text (Text)

import Langchain.Error (LangchainError)
import Langchain.Model (ChatModel, ModelConfig, extractMessageText, userMessage)
import qualified Langchain.Model as M (invoke)
import Langchain.Model.Types (Message)
import Langchain.Tool (Tool (..))

-- | Fundamental Runnable interface for components wrapped in 'Prim'.
class Runnable r m where
  type RunnableInput r :: Type
  type RunnableOutput r :: Type
  invoke :: r -> RunnableInput r -> m (Either LangchainError (RunnableOutput r))

-- | Any Tool can be executed as a primitive Runnable taking JSON 'Value' to 'Text'.
instance Runnable (Tool m) m where
  type RunnableInput (Tool m) = Value
  type RunnableOutput (Tool m) = Text
  invoke = toolExecute

-- ---------------------------------------------------------------------------
-- Model Runnables with Configuration Support
-- ---------------------------------------------------------------------------

-- | Wrapper to treat any 'ChatModel' as a Runnable over '[Message]' -> 'Message' with optional bound configuration.
data ModelRunnable c = ModelRunnable
  { modelRunnableModel :: !c
  , modelRunnableConfig :: !(Maybe (ModelConfig c))
  }

deriving instance (Show c, Show (ModelConfig c)) => Show (ModelRunnable c)
deriving instance (Eq c, Eq (ModelConfig c)) => Eq (ModelRunnable c)

-- | Construct a ModelRunnable without pre-set configuration.
modelRunnable :: c -> ModelRunnable c
modelRunnable c = ModelRunnable c Nothing

-- | Construct a ModelRunnable with pre-bound model configuration.
modelRunnableWithConfig :: c -> ModelConfig c -> ModelRunnable c
modelRunnableWithConfig c cfg = ModelRunnable c (Just cfg)

-- | Bind or replace the configuration on a ModelRunnable (equivalent to LangChain's model.bind).
bindConfig :: ModelConfig c -> ModelRunnable c -> ModelRunnable c
bindConfig cfg (ModelRunnable c _) = ModelRunnable c (Just cfg)

instance (ChatModel c, MonadIO m) => Runnable (ModelRunnable c) m where
  type RunnableInput (ModelRunnable c) = [Message]
  type RunnableOutput (ModelRunnable c) = Message
  invoke (ModelRunnable c mCfg) msgs = runExceptT (M.invoke c msgs mCfg)

-- | Wrapper to treat any 'ChatModel' as a simple 'Text' -> 'Text' Runnable with optional bound configuration.
data TextModelRunnable c = TextModelRunnable
  { textModelRunnableModel :: !c
  , textModelRunnableConfig :: !(Maybe (ModelConfig c))
  }

deriving instance (Show c, Show (ModelConfig c)) => Show (TextModelRunnable c)
deriving instance (Eq c, Eq (ModelConfig c)) => Eq (TextModelRunnable c)

-- | Construct a TextModelRunnable without pre-set configuration.
textModelRunnable :: c -> TextModelRunnable c
textModelRunnable c = TextModelRunnable c Nothing

-- | Construct a TextModelRunnable with pre-bound model configuration.
textModelRunnableWithConfig :: c -> ModelConfig c -> TextModelRunnable c
textModelRunnableWithConfig c cfg = TextModelRunnable c (Just cfg)

-- | Bind or replace the configuration on a TextModelRunnable.
bindTextConfig :: ModelConfig c -> TextModelRunnable c -> TextModelRunnable c
bindTextConfig cfg (TextModelRunnable c _) = TextModelRunnable c (Just cfg)

instance (ChatModel c, MonadIO m) => Runnable (TextModelRunnable c) m where
  type RunnableInput (TextModelRunnable c) = Text
  type RunnableOutput (TextModelRunnable c) = Text
  invoke (TextModelRunnable c mCfg) prompt = do
    res <- runExceptT $ M.invoke c [userMessage prompt] mCfg
    pure (extractMessageText <$> res)

-- | Dynamic model runnable allowing model configuration to be passed per-request.
newtype ConfiguredModelRunnable c = ConfiguredModelRunnable c
  deriving (Eq, Show)

instance (ChatModel c, MonadIO m) => Runnable (ConfiguredModelRunnable c) m where
  type RunnableInput (ConfiguredModelRunnable c) = ([Message], Maybe (ModelConfig c))
  type RunnableOutput (ConfiguredModelRunnable c) = Message
  invoke (ConfiguredModelRunnable c) (msgs, mCfg) = runExceptT (M.invoke c msgs mCfg)

-- ---------------------------------------------------------------------------
-- RunnableTree Pipeline AST
-- ---------------------------------------------------------------------------

{- | Pure GADT representing a composable pipeline AST.
@i@ = input type, @o@ = output type, @m@ = monad context.
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

-- | Lift a pure function into a 'RunnableTree' node.
runPure :: Monad m => (i -> o) -> RunnableTree m i o
runPure f = runLambda (pure . Right . f)

-- | Identity node in a 'RunnableTree' (passes input through unchanged, like LangChain's @RunnablePassthrough@).
runPassthrough :: RunnableTree m a a
runPassthrough = Id

-- | Alias for 'runPassthrough'.
runIdent :: RunnableTree m a a
runIdent = Id

-- | Construct a conditional branch AST node.
runBranch :: (i -> m Bool) -> RunnableTree m i o -> RunnableTree m i o -> RunnableTree m i o
runBranch = Branch

-- | Construct a self-healing fallback AST node (tries first, catches errors and runs second).
runFallback :: RunnableTree m i o -> RunnableTree m i o -> RunnableTree m i o
runFallback = Fallback

-- | Lift any 'ChatModel' into a simple 'Text' -> 'Text' pipeline step (without configuration).
runChat :: (ChatModel c, MonadIO m) => c -> RunnableTree m Text Text
runChat c = runPrim (TextModelRunnable c Nothing)

-- | Lift any 'ChatModel' into a simple 'Text' -> 'Text' pipeline step with bound model configuration.
runChatWithConfig :: (ChatModel c, MonadIO m) => c -> ModelConfig c -> RunnableTree m Text Text
runChatWithConfig c cfg = runPrim (TextModelRunnable c (Just cfg))

-- | Lift any 'ChatModel' into a structured '[Message]' -> 'Message' pipeline step (without configuration).
runModel :: (ChatModel c, MonadIO m) => c -> RunnableTree m [Message] Message
runModel c = runPrim (ModelRunnable c Nothing)

-- | Lift any 'ChatModel' into a structured '[Message]' -> 'Message' pipeline step with bound model configuration.
runModelWithConfig ::
  (ChatModel c, MonadIO m) => c -> ModelConfig c -> RunnableTree m [Message] Message
runModelWithConfig c cfg = runPrim (ModelRunnable c (Just cfg))

-- | Lift any 'ChatModel' into a pipeline step taking dynamic messages and configuration per request.
runConfiguredModel ::
  (ChatModel c, MonadIO m) => c -> RunnableTree m ([Message], Maybe (ModelConfig c)) Message
runConfiguredModel c = runPrim (ConfiguredModelRunnable c)

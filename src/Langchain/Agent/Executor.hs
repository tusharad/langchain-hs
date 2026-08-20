{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Agent.Executor
Description : Agent execution loop and orchestration
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides simple execution wrapper for ReActAgent.
-}
module Langchain.Agent.Executor
  ( runAgent
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Langchain.Agent.ReAct
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel, Message, userMessage)

-- | Run ReAct agent on a text input prompt
runAgent ::
  (ChatModel model, MonadIO m, MonadError LangchainError m) =>
  ReActAgent model m ->
  Text ->
  m Message
runAgent agent input = runReActAgent agent [userMessage input]

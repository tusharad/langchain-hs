{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Chain.Conversational
Description : Conversational chain wrapping ChatModel with conversation memory
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Maintains full multi-turn conversation history automatically across user inputs.
-}
module Langchain.Chain.Conversational
  ( ConversationalChain (..)
  , newConversationalChain
  , runConversationalChain
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , systemMessage
  )
import Langchain.Memory.Core (BaseMemory (..))

-- | Conversational chain linking a ChatModel with a BaseMemory store
data ConversationalChain model memory = ConversationalChain
  { convModel :: model
  , convMemory :: memory
  , convSystemPrompt :: Maybe Text
  }

-- | Construct a new ConversationalChain
newConversationalChain :: model -> memory -> Maybe Text -> ConversationalChain model memory
newConversationalChain = ConversationalChain

-- | Execute one conversational turn: records user input, generates response, stores AI response
runConversationalChain ::
  (ChatModel model, BaseMemory memory, MonadIO m, MonadError LangchainError m) =>
  ConversationalChain model memory ->
  Text ->
  m Text
runConversationalChain ConversationalChain {..} userTxt = do
  addUserMessage convMemory userTxt
  history <- messages convMemory
  let conversation = case convSystemPrompt of
        Just sys -> systemMessage sys : history
        Nothing -> history
  aiResp <- invoke convModel conversation Nothing
  let respTxt = extractMessageText aiResp
  addAiMessage convMemory respTxt
  pure respTxt

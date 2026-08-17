{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Cognitive.Synthesizer
Description : Dynamic Prompt Rewriting & Iterative Synthesis Engine
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Synthesizes customized system prompts tailored to decomposed tasks and executes
multi-step synthesis producing a structured final answer with inline citations.
-}
module Cortex.Cognitive.Synthesizer
  ( CognitiveFinalAnswer (..)
  , rewriteSystemPrompt
  , synthesizeCognitiveResponse
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)

import Cortex.Cognitive.Decomposer (SplittedInput (..), UserTask (..))
import Cortex.Cognitive.Evaluator (CognitiveDecision (..))
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel, Role (..), extractMessageText, invoke, textMessage)
import Langchain.DocumentLoader.Core (Document (..))

-- | Final structured response produced by Cognitive RAG loop
data CognitiveFinalAnswer = CognitiveFinalAnswer
  { ansSummary :: !Text
  , ansDetails :: !Text
  , ansCitations :: ![Text]
  , ansTasksCompleted :: !Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON CognitiveFinalAnswer
instance FromJSON CognitiveFinalAnswer

-- | Dynamically rewrite the system prompt based on active tasks and required tools
rewriteSystemPrompt
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> Text               -- ^ Base system prompt
  -> SplittedInput      -- ^ Decomposed task specification
  -> CognitiveDecision  -- ^ Evaluator decision & tools to activate
  -> m Text
rewriteSystemPrompt model basePrompt SplittedInput {..} CognitiveDecision {..} = do
  let taskBullets = T.intercalate "\n" ["- " <> taskQuery t | t <- splitTasks]
  let toolList = if null toolsToActivate then "None" else T.intercalate ", " toolsToActivate

  let promptText =
        "You are an expert meta-prompt engineer. Specialize the system prompt for the given tasks.\n\n"
          <> "Base System Prompt:\n" <> basePrompt <> "\n\n"
          <> "Decomposed Tasks to Focus on:\n" <> taskBullets <> "\n\n"
          <> "Active Tools:\n" <> toolList <> "\n\n"
          <> "Rewrite the system prompt to enforce strict focus on answering these tasks with evidence. Output ONLY the updated prompt text."

  let msg = textMessage User promptText
  aiMsg <- invoke model [msg] Nothing
  pure $ extractMessageText aiMsg

-- | Synthesize a final comprehensive answer from decomposed tasks and retrieved context
synthesizeCognitiveResponse
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> Text               -- ^ Active system prompt
  -> SplittedInput      -- ^ Decomposed input
  -> [Document]         -- ^ Evidence passages
  -> m CognitiveFinalAnswer
synthesizeCognitiveResponse model systemPrompt SplittedInput {..} docs = do
  let contextBlocks =
        [ "[" <> T.pack (show idx) <> "] " <> TL.toStrict (pageContent d)
        | (idx, d) <- zip [1 :: Int ..] docs
        ]
  let contextSection =
        if null contextBlocks
          then "No external knowledge passages available."
          else T.intercalate "\n\n" contextBlocks

  let taskBullets = T.intercalate "\n" ["- " <> taskQuery t | t <- splitTasks]

  let promptText =
        "Instructions: " <> splitInstructions <> "\n\n"
          <> "Tasks:\n" <> taskBullets <> "\n\n"
          <> "Context:\n" <> contextSection <> "\n\n"
          <> "Please provide a thorough, structured final answer. Reference context brackets (e.g. [1], [2]) for citations."

  let msgs =
        [ textMessage System systemPrompt
        , textMessage User promptText
        ]

  aiMsg <- invoke model msgs Nothing
  let fullAnswer = extractMessageText aiMsg
  let sources = extractSourceList docs

  pure CognitiveFinalAnswer
    { ansSummary = T.take 200 fullAnswer <> "..."
    , ansDetails = fullAnswer
    , ansCitations = sources
    , ansTasksCompleted = True
    }
  where
    extractSourceList ds =
      [ src
      | d <- ds
      , Just (String src) <- [Map.lookup "source" (metadata d)]
      ]

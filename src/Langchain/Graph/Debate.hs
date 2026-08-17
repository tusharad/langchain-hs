{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.Debate
Description : Multi-agent debate orchestration pattern
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Coordinates multiple debater models that exchange counter-arguments over several rounds,
with a moderator model judging convergence and synthesizing the final conclusion.
-}
module Langchain.Graph.Debate
  ( Debater (..)
  , DebateRound (..)
  , DebateConfig (..)
  , defaultDebateConfig
  , runDebate
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model
  ( ChatModel (..)
  , Message (..)
  , extractMessageText
  , systemMessage
  , userMessage
  )

-- | Debater agent definition
data Debater model = Debater
  { debaterName :: !Text
  , debaterPerspective :: !Text
  , debaterModel :: model
  }

-- | Record of arguments made in a single debate round
data DebateRound = DebateRound
  { roundNumber :: !Int
  , roundArguments :: ![(Text, Text)] -- (DebaterName, Argument)
  , moderatorFeedback :: !(Maybe Text)
  }
  deriving (Show, Eq)

-- | Configuration options for the debate
data DebateConfig = DebateConfig
  { maxDebateRounds :: Int
  , debateTopic :: Text
  }
  deriving (Show, Eq)

-- | Default debate configuration
defaultDebateConfig :: Text -> DebateConfig
defaultDebateConfig topic =
  DebateConfig
    { maxDebateRounds = 3
    , debateTopic = topic
    }

-- | Run a multi-agent debate session to convergence or max rounds
runDebate
  :: (ChatModel model, ChatModel moderatorModel, MonadIO m, MonadError LangchainError m)
  => DebateConfig
  -> [Debater model]
  -> moderatorModel
  -> m (Text, [DebateRound])
runDebate DebateConfig {..} debaters moderator = do
  if null debaters
    then throwError $ agentError "Debate requires at least one debater" (Just "runDebate") Nothing
    else loop 1 []
  where
    loop roundNum prevRounds
      | roundNum > maxDebateRounds = finalizeDebate prevRounds
      | otherwise = do
          let historyContext = formatHistory prevRounds
          roundArgs <- flip mapM debaters $ \d -> do
            let prompt =
                  "Topic: "
                    <> debateTopic
                    <> "\nYour Perspective: "
                    <> debaterPerspective d
                    <> "\n\nPrevious Debate History:\n"
                    <> historyContext
                    <> "\nProvide your argument or rebuttal for Round "
                    <> T.pack (show roundNum)
                    <> ":"
            resp <- invoke (debaterModel d) [userMessage prompt] Nothing
            pure (debaterName d, extractMessageText resp)

          let checkConvergencePrompt =
                "Topic: "
                  <> debateTopic
                  <> "\n\nArguments from Round "
                  <> T.pack (show roundNum)
                  <> ":\n"
                  <> T.unlines [name <> ": " <> arg | (name, arg) <- roundArgs]
                  <> "\nHave the debaters reached a consensus or conclusion? If YES, reply 'CONVERGED: <summary>'. Otherwise reply 'CONTINUE'."

          modResp <- invoke moderator [userMessage checkConvergencePrompt] Nothing
          let modTxt = T.strip (extractMessageText modResp)
          let currentRound = DebateRound roundNum roundArgs (Just modTxt)
          if "CONVERGED:" `T.isPrefixOf` modTxt
            then pure (T.strip (T.drop 10 modTxt), prevRounds ++ [currentRound])
            else loop (roundNum + 1) (prevRounds ++ [currentRound])

    finalizeDebate history = do
      let finalPrompt =
            "Topic: "
              <> debateTopic
              <> "\n\nComplete Debate Transcript:\n"
              <> formatHistory history
              <> "\n\nSynthesize the strongest points from all perspectives and provide a final verdict:"
      finalMsg <- invoke moderator [userMessage finalPrompt] Nothing
      pure (extractMessageText finalMsg, history)

    formatHistory rounds =
      if null rounds
        then "No prior rounds."
        else
          T.unlines
            [ "--- Round "
                <> T.pack (show roundNumber)
                <> " ---\n"
                <> T.unlines [name <> ": " <> arg | (name, arg) <- roundArguments]
            | DebateRound {..} <- rounds
            ]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Graph.Voting
Description : Voting and ensemble classification multi-agent pattern
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Queries multiple independent models or prompts in parallel, aggregates their discrete votes or labels,
and resolves the winning classification via majority voting and configurable tie-breaking.
-}
module Langchain.Graph.Voting
  ( VoteRecord (..)
  , TieBreaker (..)
  , VotingClassifier (..)
  , newVotingClassifier
  , runVotingClassification
  ) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , userMessage
  )

-- | Record of an individual voter's output
data VoteRecord = VoteRecord
  { voterName :: !Text
  , voteChoice :: !Text
  }
  deriving (Show, Eq)

-- | Strategy to resolve vote ties
data TieBreaker
  = FirstVoteWins
  | ModeratorVote
  deriving (Show, Eq)

-- | Container for voting ensemble
data VotingClassifier model = VotingClassifier
  { voterModels :: ![(Text, model)] -- (VoterName, Model)
  , votePrompt :: !Text
  , tieBreaker :: !TieBreaker
  }

-- | Construct a new VotingClassifier
newVotingClassifier :: [(Text, model)] -> Text -> VotingClassifier model
newVotingClassifier models prompt =
  VotingClassifier
    { voterModels = models
    , votePrompt = prompt
    , tieBreaker = FirstVoteWins
    }

-- | Execute the ensemble vote on a given input text
runVotingClassification ::
  (ChatModel model, MonadIO m, MonadError LangchainError m) =>
  VotingClassifier model ->
  Text ->
  m (Text, [VoteRecord])
runVotingClassification VotingClassifier {..} input = do
  if null voterModels
    then
      throwError $
        agentError "Voting classifier requires at least one voter" (Just "runVotingClassification") Nothing
    else do
      voteResults <- forM voterModels $ \(name, model) -> do
        let p =
              votePrompt
                <> "\n\nInput to classify:\n"
                <> input
                <> "\n\nOutput ONLY the chosen classification label:"
        resp <- invoke model [userMessage p] Nothing
        let choice = T.strip (extractMessageText resp)
        pure $ VoteRecord name choice

      let tally = foldr (\v m -> Map.insertWith (+) (voteChoice v) (1 :: Int) m) Map.empty voteResults
          topScore = maximum (Map.elems tally)
          winners =
            [ label
            | (label, count) <- Map.toList tally
            , count == topScore
            ]
      winningChoice <- case listToMaybe winners of
        Just res -> pure res
        Nothing -> throwError $ agentError "winner list is empty" Nothing Nothing

      pure (winningChoice, voteResults)

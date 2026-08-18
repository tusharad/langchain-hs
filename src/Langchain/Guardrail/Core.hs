{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Guardrail.Core
Description : Agent input/output validation guardrails and safety filters
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Composable guardrails for validating prompt safety, topic restriction, and response format constraints.
-}
module Langchain.Guardrail.Core
  ( GuardrailResult (..)
  , Guardrail (..)
  , contentSafetyGuardrail
  , topicGuardrail
  , outputLengthGuardrail
  , composeGuardrails
  , withGuardrails
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model
  ( ChatModel (..)
  , extractMessageText
  , userMessage
  )

-- | Outcome of evaluating a guardrail check
data GuardrailResult
  = GuardrailPass
  | GuardrailFail !Text -- Reason for failure
  deriving (Show, Eq)

-- | Composable guardrail container
data Guardrail m = Guardrail
  { guardrailName :: !Text
  , validateInput :: Text -> m GuardrailResult
  , validateOutput :: Text -> m GuardrailResult
  }

-- | Simple keyword-based content safety guardrail
contentSafetyGuardrail :: MonadIO m => [Text] -> Guardrail m
contentSafetyGuardrail forbiddenWords =
  Guardrail
    { guardrailName = "ContentSafety"
    , validateInput = \input ->
        let lower = T.toLower input
            matched = filter (\w -> w `T.isInfixOf` lower) (map T.toLower forbiddenWords)
         in pure $
              if null matched
                then GuardrailPass
                else GuardrailFail ("Input contains forbidden content: " <> T.intercalate ", " matched)
    , validateOutput = \output ->
        let lower = T.toLower output
            matched = filter (\w -> w `T.isInfixOf` lower) (map T.toLower forbiddenWords)
         in pure $
              if null matched
                then GuardrailPass
                else GuardrailFail ("Output contains forbidden content: " <> T.intercalate ", " matched)
    }

-- | Output length guardrail
outputLengthGuardrail :: MonadIO m => Int -> Guardrail m
outputLengthGuardrail maxLen =
  Guardrail
    { guardrailName = "OutputLength"
    , validateInput = \_ -> pure GuardrailPass
    , validateOutput = \out ->
        if T.length out <= maxLen
          then pure GuardrailPass
          else
            pure $
              GuardrailFail
                ("Output length (" <> T.pack (show (T.length out)) <> ") exceeds limit of " <> T.pack (show maxLen))
    }

-- | LLM-based topic relevance guardrail
topicGuardrail ::
  (ChatModel model, MonadIO m, MonadError LangchainError m) => model -> Text -> Guardrail m
topicGuardrail model allowedTopic =
  Guardrail
    { guardrailName = "TopicRestriction"
    , validateInput = \input -> do
        let prompt =
              "Allowed Topic: "
                <> allowedTopic
                <> "\n\nUser Input: "
                <> input
                <> "\nIs the user input relevant to the allowed topic? Reply ONLY with 'YES' or 'NO: <reason>'."
        resp <- invoke model [userMessage prompt] Nothing
        let ans = T.strip (extractMessageText resp)
        pure $
          if "YES" `T.isPrefixOf` ans
            then GuardrailPass
            else GuardrailFail ("Topic violation: " <> ans)
    , validateOutput = \_ -> pure GuardrailPass
    }

-- | Compose multiple guardrails in sequence
composeGuardrails :: (MonadIO m) => [Guardrail m] -> Guardrail m
composeGuardrails [] =
  Guardrail "NoOp" (\_ -> pure GuardrailPass) (\_ -> pure GuardrailPass)
composeGuardrails rails =
  Guardrail
    { guardrailName = T.intercalate "+" (map guardrailName rails)
    , validateInput = \input -> checkAll (map validateInput rails) input
    , validateOutput = \output -> checkAll (map validateOutput rails) output
    }
  where
    checkAll [] _ = pure GuardrailPass
    checkAll (v : vs) txt = do
      res <- v txt
      case res of
        GuardrailPass -> checkAll vs txt
        failRes -> pure failRes

-- | Execute an action wrapped by input and output guardrails
withGuardrails ::
  (MonadIO m, MonadError LangchainError m) =>
  Guardrail m ->
  (Text -> m Text) ->
  Text ->
  m Text
withGuardrails rail action input = do
  inRes <- validateInput rail input
  case inRes of
    GuardrailFail reason ->
      throwError $ agentError ("Input guardrail failed: " <> reason) (Just (guardrailName rail)) Nothing
    GuardrailPass -> do
      output <- action input
      outRes <- validateOutput rail output
      case outRes of
        GuardrailFail reason ->
          throwError $ agentError ("Output guardrail failed: " <> reason) (Just (guardrailName rail)) Nothing
        GuardrailPass -> pure output

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Langchain.Eval
Description : Lightweight evaluation harness for chains, agents, and models
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Quantitative evaluation and regression testing framework for LLM applications.
Provides typed datasets ('EvalCase'), deterministic scorers (exact match, substring,
tool call trace accuracy), LLM-as-a-judge scoring, and JSON evaluation report generation.
-}
module Langchain.Eval
  ( -- * Core Types
    EvalCase (..)
  , EvalScore (..)
  , Scorer (..)
  , createScorer
  , CaseResult (..)
  , CaseScoreResult (..)
  , EvalReport (..)

    -- * Deterministic Scorers
  , exactMatchScorer
  , containsScorer
  , toolCallExactScorer

    -- * Model-based Scorers
  , llmJudgeScorer

    -- * Evaluation Runner
  , runEvalSuite
  ) where

import Control.Monad.Except (MonadError, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , decode
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Langchain.Agent.ReAct (AgentStep (..), AgentTrace (..))
import Langchain.Error (LangchainError, errorMessage)
import Langchain.Model (ChatModel (..))
import Langchain.Model.Types (extractMessageText, toolCallName, userMessage)

-- | A single evaluation test case with input, optional reference target, and metadata.
data EvalCase i o = EvalCase
  { evalInput :: !i
  -- ^ Input supplied to the subject under test
  , evalExpected :: !(Maybe o)
  -- ^ Optional ground-truth or expected reference output
  , evalMetadata :: !(Maybe (Map Text Text))
  -- ^ Optional contextual tags or metadata
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Assessment produced by a 'Scorer' on an individual test case.
data EvalScore = EvalScore
  { scoreValue :: !Double
  -- ^ Normalized score between 0.0 and 1.0
  , scorePassed :: !Bool
  -- ^ Whether the case satisfied the threshold or assertion
  , scoreReason :: !(Maybe Text)
  -- ^ Diagnostic or qualitative reasoning for the score
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Named evaluation metric evaluating actual output against a test case.
data Scorer i o m = Scorer
  { scorerName :: !Text
  -- ^ Unique identifier for this evaluation metric
  , scoreCase :: EvalCase i o -> o -> m EvalScore
  -- ^ Evaluation function producing 'EvalScore'
  }

-- | Construct a custom 'Scorer'.
createScorer :: Text -> (EvalCase i o -> o -> m EvalScore) -> Scorer i o m
createScorer = Scorer

-- | Score result produced by a specific named scorer.
data CaseScoreResult = CaseScoreResult
  { caseScoreName :: !Text
  , caseScore :: !EvalScore
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Outcome of running all scorers on a single test case.
data CaseResult = CaseResult
  { caseInput :: !Value
  , caseExpected :: !(Maybe Value)
  , caseActual :: !(Either Text Value)
  , caseScores :: ![CaseScoreResult]
  , caseLatencyMicros :: !Int
  , casePassed :: !Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Aggregated summary report of an evaluation suite run.
data EvalReport = EvalReport
  { reportTotalCases :: !Int
  -- ^ Total number of evaluated cases
  , reportPassedCases :: !Int
  -- ^ Number of cases that passed all scorers
  , reportPassRate :: !Double
  -- ^ Ratio of passed cases (0.0 to 1.0)
  , reportMeanScore :: !Double
  -- ^ Average score value across all evaluations (0.0 to 1.0)
  , reportMeanLatencyMicros :: !Int
  -- ^ Average execution latency per case in microseconds
  , reportCases :: ![CaseResult]
  -- ^ Detailed per-case scoring breakdown
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Deterministic scorer verifying exact equality between actual and expected output.
exactMatchScorer :: (Eq o, Show o, Monad m) => Scorer i o m
exactMatchScorer = createScorer "exact_match" $ \c actual ->
  case evalExpected c of
    Nothing -> pure $ EvalScore 1.0 True (Just "No expected output specified")
    Just expected ->
      if actual == expected
        then pure $ EvalScore 1.0 True (Just "Exact match")
        else
          pure $
            EvalScore
              0.0
              False
              (Just $ "Expected: " <> T.pack (show expected) <> ", Actual: " <> T.pack (show actual))

-- | Deterministic scorer checking if expected text is contained within actual text.
containsScorer :: Monad m => Scorer Text Text m
containsScorer = createScorer "contains" $ \c actual ->
  case evalExpected c of
    Nothing -> pure $ EvalScore 1.0 True (Just "No expected substring specified")
    Just expectedSub ->
      if expectedSub `T.isInfixOf` actual
        then pure $ EvalScore 1.0 True (Just "Substring found")
        else
          pure $
            EvalScore
              0.0
              False
              (Just $ "Substring '" <> expectedSub <> "' not found in actual output")

-- | Deterministic scorer verifying that an 'AgentTrace' invoked an exact sequence of tools.
toolCallExactScorer :: Monad m => [Text] -> Scorer i AgentTrace m
toolCallExactScorer expectedTools = createScorer "tool_call_exact" $ \_ trace -> do
  let actualTools =
        [ toolCallName tc
        | (AgentAction _ tcs, _) <- traceSteps trace
        , tc <- tcs
        ]
  if actualTools == expectedTools
    then pure $ EvalScore 1.0 True (Just "Tool call sequence matched exactly")
    else
      pure $
        EvalScore
          0.0
          False
          ( Just $
              "Expected tool sequence: "
                <> T.pack (show expectedTools)
                <> ", but got: "
                <> T.pack (show actualTools)
          )

-- | LLM-as-a-judge scorer evaluating output against a custom qualitative rubric.
llmJudgeScorer ::
  forall model m.
  (ChatModel model, MonadIO m, MonadError LangchainError m) =>
  model ->
  Text ->
  Scorer Text Text m
llmJudgeScorer model rubric = createScorer "llm_judge" $ \c actual -> do
  let inputTxt = evalInput c
      expectedTxt = fromMaybe "" (evalExpected c)
      prompt =
        "You are an expert AI evaluator. Assess the following actual output against the rubric.\n\n"
          <> "Evaluation Rubric: "
          <> rubric
          <> "\n\n"
          <> "Test Input: "
          <> inputTxt
          <> "\n"
          <> "Reference Target: "
          <> expectedTxt
          <> "\n"
          <> "Actual Output: "
          <> actual
          <> "\n\n"
          <> "Reply with a JSON object: {\"passed\": true/false, \"score\": 0.0-1.0, \"reason\": \"<explanation>\"}"
  eResp <- (Right <$> invoke model [userMessage prompt] Nothing) `catchError` (pure . Left)
  case eResp of
    Left err -> pure $ EvalScore 0.0 False (Just $ "LLM judge invocation failed: " <> errorMessage err)
    Right resp -> do
      let outTxt = extractMessageText resp
      case parseJudgeJson outTxt of
        Just (passed, sc, rsn) -> pure $ EvalScore sc passed (Just rsn)
        Nothing ->
          let passed = not ("fail" `T.isInfixOf` T.toLower outTxt)
              sc = if passed then 1.0 else 0.0
           in pure $ EvalScore sc passed (Just outTxt)

parseJudgeJson :: Text -> Maybe (Bool, Double, Text)
parseJudgeJson txt = do
  val <- decode (LBS.fromStrict (TE.encodeUtf8 txt))
  case parseEither parser val of
    Right res -> Just res
    Left _ -> Nothing
  where
    parser = withObject "JudgeResponse" $ \o -> do
      passed <- o .: "passed"
      sc <-
        o .:? "score" >>= \case
          Just s -> pure s
          Nothing -> pure (if passed then 1.0 else 0.0)
      rsn <-
        o .:? "reason" >>= \case
          Just r -> pure r
          Nothing -> pure ""
      pure (passed, sc, rsn)

-- | Execute an evaluation suite over a set of test cases and scorers.
runEvalSuite ::
  forall i o m.
  (MonadIO m, ToJSON i, ToJSON o) =>
  (i -> m (Either LangchainError o)) ->
  [Scorer i o m] ->
  [EvalCase i o] ->
  m EvalReport
runEvalSuite runner scorers cases = do
  caseResults <- mapM evaluateCase cases
  let total = length caseResults
      passedCount = length (filter casePassed caseResults)
      passRate = if total == 0 then 1.0 else fromIntegral passedCount / fromIntegral total
      allScores = [scoreValue (caseScore sr) | cr <- caseResults, sr <- caseScores cr]
      meanScore =
        if null allScores
          then if total == 0 then 1.0 else 0.0
          else sum allScores / fromIntegral (length allScores)
      allLatencies = map caseLatencyMicros caseResults
      meanLatency =
        if null allLatencies
          then 0
          else sum allLatencies `div` total
  pure
    EvalReport
      { reportTotalCases = total
      , reportPassedCases = passedCount
      , reportPassRate = passRate
      , reportMeanScore = meanScore
      , reportMeanLatencyMicros = meanLatency
      , reportCases = caseResults
      }
  where
    evaluateCase c = do
      startTime <- liftIO getCurrentTime
      eActual <- runner (evalInput c)
      endTime <- liftIO getCurrentTime
      let latencyMicros =
            round (realToFrac (diffUTCTime endTime startTime) * 1000000 :: Double) :: Int
      case eActual of
        Left err -> do
          let failedScore = EvalScore 0.0 False (Just $ errorMessage err)
              scores = [CaseScoreResult (scorerName s) failedScore | s <- scorers]
          pure
            CaseResult
              { caseInput = toJSON (evalInput c)
              , caseExpected = fmap toJSON (evalExpected c)
              , caseActual = Left (errorMessage err)
              , caseScores = scores
              , caseLatencyMicros = latencyMicros
              , casePassed = False
              }
        Right actual -> do
          scores <- mapM (\s -> CaseScoreResult (scorerName s) <$> scoreCase s c actual) scorers
          let caseOk = all (scorePassed . caseScore) scores
          pure
            CaseResult
              { caseInput = toJSON (evalInput c)
              , caseExpected = fmap toJSON (evalExpected c)
              , caseActual = Right (toJSON actual)
              , caseScores = scores
              , caseLatencyMicros = latencyMicros
              , casePassed = caseOk
              }

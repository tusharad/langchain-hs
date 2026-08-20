{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Middleware.TokenBudget
Description : Token budgeting and cost tracking middleware
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Tracks cumulative token usage per pipeline run via STM, enforces configurable
token ceilings to terminate runaway agent loops, and provides cost estimation.
-}
module Aegis.Middleware.TokenBudget
  ( -- * Token Budget Manager
    TokenBudgetManager (..)
  , newTokenBudgetManager

    -- * Operations
  , recordTokenUsage
  , checkBudget
  , getBudgetStatus
  , resetBudget

    -- * Budget Status
  , BudgetStatus (..)
  , BudgetWarning (..)
  , TokenUsageRecord (..)
  ) where

import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)

import Aegis.Core.Types.Config (TokenBudgetConfig (..))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A record of a single token usage event
data TokenUsageRecord = TokenUsageRecord
  { turAgent :: Text
  -- ^ Which agent consumed the tokens
  , turPromptTokens :: Int
  -- ^ Tokens used in the prompt
  , turCompletionTokens :: Int
  -- ^ Tokens used in the completion
  , turTotalTokens :: Int
  -- ^ Total tokens (prompt + completion)
  , turModel :: Text
  -- ^ Model used
  , turTimestamp :: Maybe UTCTime
  -- ^ When the usage occurred
  }
  deriving (Eq, Show)

-- | Budget warning levels
data BudgetWarning
  = BudgetOK
  -- ^ Under threshold
  | BudgetWarning Double
  -- ^ Over warning threshold (percentage used)
  | BudgetExceeded
  -- ^ Budget exhausted
  deriving (Eq, Show)

-- | Current budget status snapshot
data BudgetStatus = BudgetStatus
  { bsTotalUsed :: Int
  -- ^ Total tokens consumed
  , bsTotalBudget :: Int
  -- ^ Total token budget
  , bsRemainingTokens :: Int
  -- ^ Remaining tokens
  , bsPercentUsed :: Double
  -- ^ Percentage of budget consumed
  , bsWarning :: BudgetWarning
  -- ^ Current warning level
  , bsEstimatedCost :: Double
  -- ^ Estimated cost in configured currency
  , bsRecordCount :: Int
  -- ^ Number of usage records
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Token Budget Manager
-- ---------------------------------------------------------------------------

-- | Thread-safe token budget manager using STM
data TokenBudgetManager = TokenBudgetManager
  { tbConfig :: TokenBudgetConfig
  -- ^ Budget configuration
  , tbTotalUsed :: TVar Int
  -- ^ Total tokens consumed
  , tbRecords :: TVar [TokenUsageRecord]
  -- ^ History of usage records
  , tbExceeded :: TVar Bool
  -- ^ Whether budget has been exceeded
  }

-- | Create a new token budget manager
newTokenBudgetManager :: TokenBudgetConfig -> IO TokenBudgetManager
newTokenBudgetManager config = do
  total <- newTVarIO 0
  records <- newTVarIO []
  exceeded <- newTVarIO False
  pure TokenBudgetManager
    { tbConfig = config
    , tbTotalUsed = total
    , tbRecords = records
    , tbExceeded = exceeded
    }

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Record token usage and check if budget is exceeded
recordTokenUsage :: TokenBudgetManager -> Text -> Int -> Int -> Text -> IO BudgetWarning
recordTokenUsage mgr agent promptTokens completionTokens model = do
  now <- getCurrentTime
  let totalTokens = promptTokens + completionTokens
      record = TokenUsageRecord
        { turAgent = agent
        , turPromptTokens = promptTokens
        , turCompletionTokens = completionTokens
        , turTotalTokens = totalTokens
        , turModel = model
        , turTimestamp = Just now
        }
  atomically $ do
    modifyTVar' (tbTotalUsed mgr) (+ totalTokens)
    modifyTVar' (tbRecords mgr) (record :)
    used <- readTVar (tbTotalUsed mgr)
    let budget = maxTotalTokens (tbConfig mgr)
        pct = fromIntegral used / fromIntegral budget
        warnThreshold = warnThresholdPercent (tbConfig mgr)
    if used >= budget
      then do
        writeTVar (tbExceeded mgr) True
        pure BudgetExceeded
      else if pct >= warnThreshold
        then pure $ BudgetWarning pct
        else pure BudgetOK

-- | Check whether the budget allows more token usage
checkBudget :: TokenBudgetManager -> IO BudgetWarning
checkBudget mgr = atomically $ do
  used <- readTVar (tbTotalUsed mgr)
  let budget = maxTotalTokens (tbConfig mgr)
      pct = fromIntegral used / fromIntegral budget
      warnThreshold = warnThresholdPercent (tbConfig mgr)
  if used >= budget
    then pure BudgetExceeded
    else if pct >= warnThreshold
      then pure $ BudgetWarning pct
      else pure BudgetOK

-- | Get current budget status snapshot
getBudgetStatus :: TokenBudgetManager -> IO BudgetStatus
getBudgetStatus mgr = atomically $ do
  used <- readTVar (tbTotalUsed mgr)
  records <- readTVar (tbRecords mgr)
  let budget = maxTotalTokens (tbConfig mgr)
      remaining = max 0 (budget - used)
      pct = fromIntegral used / fromIntegral (max 1 budget)
      warnThreshold = warnThresholdPercent (tbConfig mgr)
      costRate = estimatedCostPerMillionTokens (tbConfig mgr)
      cost = fromIntegral used * costRate / 1000000.0
      warning
        | used >= budget = BudgetExceeded
        | pct >= warnThreshold = BudgetWarning pct
        | otherwise = BudgetOK
  pure BudgetStatus
    { bsTotalUsed = used
    , bsTotalBudget = budget
    , bsRemainingTokens = remaining
    , bsPercentUsed = pct
    , bsWarning = warning
    , bsEstimatedCost = cost
    , bsRecordCount = length records
    }

-- | Reset the budget (e.g., for a new pipeline run)
resetBudget :: TokenBudgetManager -> IO ()
resetBudget mgr = atomically $ do
  writeTVar (tbTotalUsed mgr) 0
  writeTVar (tbRecords mgr) []
  writeTVar (tbExceeded mgr) False

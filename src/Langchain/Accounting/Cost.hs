{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Accounting.Cost
Description : Token counting estimation and multi-provider financial cost accounting
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides token estimation heuristics and provider-specific pricing calculations to
estimate and budget token usage costs in USD.
-}
module Langchain.Accounting.Cost
  ( ModelPricing (..)
  , CostEstimate (..)
  , estimateTokenCount
  , getStandardPricing
  , calculateCost
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Pricing rates per million tokens in USD
data ModelPricing = ModelPricing
  { inputPricePerMillionUSD :: !Double
  , outputPricePerMillionUSD :: !Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Calculated cost breakdown
data CostEstimate = CostEstimate
  { estimatedInputTokens :: !Int
  , estimatedOutputTokens :: !Int
  , inputCostUSD :: !Double
  , outputCostUSD :: !Double
  , totalCostUSD :: !Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Heuristic estimation of token count from raw text (~4 characters per token average)
estimateTokenCount :: Text -> Int
estimateTokenCount txt =
  let charLen = T.length txt
      wordLen = length (T.words txt)
      -- Combine character-based and word-based heuristics
      charEst = (charLen + 3) `div` 4
      wordEst = (wordLen * 4) `div` 3
   in max 1 (max charEst wordEst)

{- | Lookup standard pricing rates for widely used commercial and local models
TODO: Remove standard pricing
-}
getStandardPricing :: Text -> ModelPricing
getStandardPricing modelName
  | "gpt-4o-mini" `T.isInfixOf` lower = ModelPricing 0.15 0.60
  | "gpt-4o" `T.isInfixOf` lower = ModelPricing 2.50 10.00
  | "gemini-1.5-flash" `T.isInfixOf` lower = ModelPricing 0.075 0.30
  | "gemini-1.5-pro" `T.isInfixOf` lower = ModelPricing 1.25 5.00
  | otherwise = ModelPricing 0.0 0.0 -- Local models like Ollama are free
  where
    lower = T.toLower modelName

-- | Calculate total financial cost estimate for prompt and completion
calculateCost :: ModelPricing -> Int -> Int -> CostEstimate
calculateCost ModelPricing {..} inTokens outTokens =
  let inCost = (fromIntegral inTokens / 1000000.0) * inputPricePerMillionUSD
      outCost = (fromIntegral outTokens / 1000000.0) * outputPricePerMillionUSD
      totCost = inCost + outCost
   in CostEstimate inTokens outTokens inCost outCost totCost

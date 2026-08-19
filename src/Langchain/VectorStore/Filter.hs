{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Langchain.VectorStore.Filter
Description : Structured Metadata Filtering Predicates for Vector Stores
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides expressive metadata filter predicates ($eq, $in, $and, $or, $gt, $lt)
for narrowing down vector search candidates in multi-tenant and multi-document domains.
-}
module Langchain.VectorStore.Filter
  ( FilterPredicate (..)
  , evalFilter
  , filterDocuments
  , eqFilter
  , inFilter
  , andFilter
  , orFilter
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import GHC.Generics (Generic)

import Langchain.DocumentLoader.Core (Document (..))

-- | Structured metadata filter predicate AST
data FilterPredicate
  = Eq !Text !Value
  | Neq !Text !Value
  | In !Text ![Value]
  | Nin !Text ![Value]
  | Gt !Text !Double
  | Lt !Text !Double
  | And ![FilterPredicate]
  | Or ![FilterPredicate]
  | Not !FilterPredicate
  | TrueFilter
  deriving (Show, Eq, Generic)

instance ToJSON FilterPredicate
instance FromJSON FilterPredicate

-- | Helper constructor for equality filter
eqFilter :: Text -> Value -> FilterPredicate
eqFilter = Eq

-- | Helper constructor for inclusion filter
inFilter :: Text -> [Value] -> FilterPredicate
inFilter = In

-- | Helper constructor for conjunction filter
andFilter :: [FilterPredicate] -> FilterPredicate
andFilter = And

-- | Helper constructor for disjunction filter
orFilter :: [FilterPredicate] -> FilterPredicate
orFilter = Or

-- | Evaluate a filter predicate against document metadata
evalFilter :: FilterPredicate -> Map Text Value -> Bool
evalFilter TrueFilter _ = True
evalFilter (Eq k expected) meta = Map.lookup k meta == Just expected
evalFilter (Neq k unexpected) meta = Map.lookup k meta /= Just unexpected
evalFilter (In k allowed) meta =
  case Map.lookup k meta of
    Just val -> val `elem` allowed
    Nothing -> False
evalFilter (Nin k disallowed) meta =
  case Map.lookup k meta of
    Just val -> val `notElem` disallowed
    Nothing -> True
evalFilter (Gt k threshold) meta =
  case Map.lookup k meta of
    Just (Number n) -> toRealFloat n > threshold
    _ -> False
evalFilter (Lt k threshold) meta =
  case Map.lookup k meta of
    Just (Number n) -> toRealFloat n < threshold
    _ -> False
evalFilter (And predicates) meta = all (`evalFilter` meta) predicates
evalFilter (Or predicates) meta = null predicates || any (`evalFilter` meta) predicates
evalFilter (Not p) meta = not (evalFilter p meta)

-- | Filter a list of documents by metadata predicate
filterDocuments :: FilterPredicate -> [Document] -> [Document]
filterDocuments TrueFilter docs = docs
filterDocuments predicate docs = filter (evalFilter predicate . metadata) docs

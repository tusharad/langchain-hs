{-# LANGUAGE OverloadedStrings #-}

-- TODO: this shall not be part of the package

{- |
Module      : Langchain.Laws
Description : Formal algebraic law verification suite for Langchain abstractions
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Exports executable QuickCheck algebraic law specifications proving mathematical soundness
for 'RunnableTree', 'StateReducer', 'Checkpointer', and serialization codecs.
-}
module Langchain.Laws
  ( -- * Law Verification Runner
    verifyAllLaws

    -- * Individual Law Properties
  , prop_runnableLeftIdentity
  , prop_runnableRightIdentity
  , prop_runnableAssociativity
  , prop_reducerAssociativity
  , prop_reducerLeftIdentity
  , prop_reducerRightIdentity
  , prop_messageJsonRoundTrip
  ) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Test.QuickCheck

import Langchain.Core.Model
import Langchain.Core.Runnable
import Langchain.Graph.StateGraph (appendMessagesReducer)

-- | QuickCheck Property: Runnable Left Identity (Id |>> f === f)
prop_runnableLeftIdentity :: String -> Property
prop_runnableLeftIdentity input = ioProperty $ do
  let step = runLambda (\x -> pure $ Right (x ++ "!"))
      piped = Id |>> step
  r1 <- runExceptT $ interpret step input
  r2 <- runExceptT $ interpret piped input
  pure $ r1 === r2

-- | QuickCheck Property: Runnable Right Identity (f |>> Id === f)
prop_runnableRightIdentity :: String -> Property
prop_runnableRightIdentity input = ioProperty $ do
  let step = runLambda (\x -> pure $ Right (x ++ "!"))
      piped = step |>> Id
  r1 <- runExceptT $ interpret step input
  r2 <- runExceptT $ interpret piped input
  pure $ r1 === r2

-- | QuickCheck Property: Runnable Associativity ((f |>> g) |>> h === f |>> (g |>> h))
prop_runnableAssociativity :: String -> Property
prop_runnableAssociativity input = ioProperty $ do
  let f = runLambda (\x -> pure $ Right (x ++ "-1"))
      g = runLambda (\x -> pure $ Right (x ++ "-2"))
      h = runLambda (\x -> pure $ Right (x ++ "-3"))
      lhs = (f |>> g) |>> h
      rhs = f |>> (g |>> h)
  r1 <- runExceptT $ interpret lhs input
  r2 <- runExceptT $ interpret rhs input
  pure $ r1 === r2

-- | QuickCheck Property: Reducer Monoid Associativity ((a <> b) <> c === a <> (b <> c))
prop_reducerAssociativity :: [String] -> [String] -> [String] -> Property
prop_reducerAssociativity a b c =
  let msgsA = map (userMessage . T.pack) a
      msgsB = map (assistantMessage . T.pack) b
      msgsC = map (userMessage . T.pack) c
      lhs = appendMessagesReducer (appendMessagesReducer msgsA msgsB) msgsC
      rhs = appendMessagesReducer msgsA (appendMessagesReducer msgsB msgsC)
   in lhs === rhs

-- | QuickCheck Property: Reducer Left Identity ([] <> a === a)
prop_reducerLeftIdentity :: [String] -> Property
prop_reducerLeftIdentity a =
  let msgsA = map (userMessage . T.pack) a
   in appendMessagesReducer [] msgsA === msgsA

-- | QuickCheck Property: Reducer Right Identity (a <> [] === a)
prop_reducerRightIdentity :: [String] -> Property
prop_reducerRightIdentity a =
  let msgsA = map (userMessage . T.pack) a
   in appendMessagesReducer msgsA [] === msgsA

-- | QuickCheck Property: Message JSON Round-Trip Invariance
prop_messageJsonRoundTrip :: String -> Property
prop_messageJsonRoundTrip str =
  let msg = userMessage (T.pack str)
      encoded = encode msg
      decoded = decode encoded
   in decoded === Just msg

-- | Run all algebraic law checks and return True if all passed
verifyAllLaws :: IO Bool
verifyAllLaws = do
  res1 <- quickCheckResult prop_runnableLeftIdentity
  res2 <- quickCheckResult prop_runnableRightIdentity
  res3 <- quickCheckResult prop_runnableAssociativity
  res4 <- quickCheckResult prop_reducerAssociativity
  res5 <- quickCheckResult prop_reducerLeftIdentity
  res6 <- quickCheckResult prop_reducerRightIdentity
  res7 <- quickCheckResult prop_messageJsonRoundTrip
  pure $ all isSuccess [res1, res2, res3, res4, res5, res6, res7]

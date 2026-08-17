{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Property.RunnableSpec (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Langchain.Core.Error
import Langchain.Core.Runnable

type PureMonad = ExceptT LangchainError IO

tests :: TestTree
tests =
  testGroup
    "Langchain.Property.RunnableSpec (QuickCheck)"
    [ testProperty "Left Identity: Id |>> t(x) == t(x)" $
        \n -> ioProperty $ do
          let step :: RunnableTree PureMonad Int Int
              step = runLambda (\i -> pure $ Right (i * 2))
              pipeline = Id |>> step
          res1 <- runExceptT $ interpret pipeline n
          res2 <- runExceptT $ interpret step n
          pure (res1 === res2)
    , testProperty "Right Identity: t(x) |>> Id == t(x)" $
        \n -> ioProperty $ do
          let step :: RunnableTree PureMonad Int Int
              step = runLambda (\i -> pure $ Right (i + 10))
              pipeline = step |>> Id
          res1 <- runExceptT $ interpret pipeline n
          res2 <- runExceptT $ interpret step n
          pure (res1 === res2)
    , testProperty "Associativity: ((f |>> g) |>> h) == (f |>> (g |>> h))" $
        \n -> ioProperty $ do
          let f :: RunnableTree PureMonad Int Int
              f = runLambda (\i -> pure $ Right (i + 1))
              g :: RunnableTree PureMonad Int Int
              g = runLambda (\i -> pure $ Right (i * 3))
              h :: RunnableTree PureMonad Int Int
              h = runLambda (\i -> pure $ Right (i - 5))

              p1 = (f |>> g) |>> h
              p2 = f |>> (g |>> h)
          res1 <- runExceptT $ interpret p1 n
          res2 <- runExceptT $ interpret p2 n
          pure (res1 === res2)
    , testProperty "Branch selects correct branch based on predicate" $
        \n -> ioProperty $ do
          let isPositive :: Int -> PureMonad Bool
              isPositive i = pure (i > 0)
              thenBranch :: RunnableTree PureMonad Int Text
              thenBranch = runLambda (\_ -> pure $ Right "POSITIVE")
              elseBranch :: RunnableTree PureMonad Int Text
              elseBranch = runLambda (\_ -> pure $ Right "NON-POSITIVE")
              branchTree = Branch isPositive thenBranch elseBranch
          res <- runExceptT $ interpret branchTree n
          let expected = if n > 0 then Right "POSITIVE" else Right "NON-POSITIVE"
          pure (res === expected)
    , testProperty "Fallback executes fallback branch on primary error" $
        \n -> ioProperty $ do
          let failingTree :: RunnableTree PureMonad Int Int
              failingTree = runLambda (\_ -> pure $ Left $ internalError "Failed" Nothing Nothing)
              fallbackTree :: RunnableTree PureMonad Int Int
              fallbackTree = runLambda (\i -> pure $ Right (i + 100))
              pipeline = Fallback failingTree fallbackTree
          res <- runExceptT $ interpret pipeline n
          pure (res === Right (n + 100))
    , testProperty "Parallel composition (&>&) produces pair output" $
        \n -> ioProperty $ do
          let doubleStep :: RunnableTree PureMonad Int Int
              doubleStep = runLambda (\i -> pure $ Right (i * 2))
              tripleStep :: RunnableTree PureMonad Int Int
              tripleStep = runLambda (\i -> pure $ Right (i * 3))
              parallelTree = doubleStep &>& tripleStep
          res <- runExceptT $ interpret parallelTree n
          pure (res === Right (n * 2, n * 3))
    ]

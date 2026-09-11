{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.Core.Runnable (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.Text as T

import Langchain.Core.Error
import Langchain.Core.Runnable

type TestMonad = ExceptT LangchainError IO

-- Pure test lambda helpers
addOneLambda :: RunnableTree TestMonad Int Int
addOneLambda = runLambda $ \x -> pure (Right (x + 1))

doubleLambda :: RunnableTree TestMonad Int Int
doubleLambda = runLambda $ \x -> pure (Right (x * 2))

failLambda :: RunnableTree TestMonad Int Int
failLambda = runLambda $ \_ -> pure (Left $ runnableError "Pipeline failed" Nothing Nothing)

tests :: TestTree
tests =
  testGroup
    "Langchain.Core.Runnable"
    [ testGroup
        "Identity Laws"
        [ testCase "Left Identity: Id |>> t == t" $ do
            let t = addOneLambda
            r1 <- runExceptT $ interpret (Id |>> t) 5
            r2 <- runExceptT $ interpret t 5
            r1 @?= Right 6
            r1 @?= r2
        , testCase "Right Identity: t |>> Id == t" $ do
            let t = addOneLambda
            r1 <- runExceptT $ interpret (t |>> Id) 5
            r2 <- runExceptT $ interpret t 5
            r1 @?= Right 6
            r1 @?= r2
        ]
    , testGroup
        "Sequential Composition Laws"
        [ testCase "Seq executes in left-to-right order" $ do
            let pipeline = addOneLambda |>> doubleLambda -- (5 + 1) * 2 = 12
            res <- runExceptT $ interpret pipeline 5
            res @?= Right 12
        , testCase "Seq propagates errors early" $ do
            let pipeline = failLambda |>> doubleLambda
            res <- runExceptT $ interpret pipeline 5
            case res of
              Left (RunnableError msg _) -> assertBool "Should contain error message" ("failed" `T.isInfixOf` msg)
              _ -> assertFailure "Expected RunnableError"
        ]
    , testGroup
        "Parallel Composition Laws (&>&)"
        [ testCase "Par executes branches concurrently" $ do
            let pipeline = addOneLambda &>& doubleLambda
            res <- runExceptT $ interpret pipeline 10
            res @?= Right (11, 20)
        ]
    , testGroup
        "Branching and Fallback"
        [ testCase "Branch selects True branch" $ do
            let pipeline = Branch (\x -> pure (x > 0)) addOneLambda doubleLambda
            res <- runExceptT $ interpret pipeline 5
            res @?= Right 6
        , testCase "Branch selects False branch" $ do
            let pipeline = Branch (\x -> pure (x > 0)) addOneLambda doubleLambda
            res <- runExceptT $ interpret pipeline (-5)
            res @?= Right (-10)
        , testCase "Fallback executes secondary when primary fails" $ do
            let pipeline = Fallback failLambda doubleLambda
            res <- runExceptT $ interpret pipeline 7
            res @?= Right 14
        ]
    , testGroup
        "Property Tests (QuickCheck)"
        [ QC.testProperty "Identity Law: Id |>> t(x) == t(x)" $ \(x :: Int) ->
            QC.ioProperty $ do
              r1 <- runExceptT $ interpret (Id |>> addOneLambda) x
              r2 <- runExceptT $ interpret addOneLambda x
              pure (r1 == r2)
        , QC.testProperty "Associativity: (f |>> g) |>> h == f |>> (g |>> h)" $ \(x :: Int) ->
            QC.ioProperty $ do
              let f = addOneLambda
                  g = doubleLambda
                  h = addOneLambda
                  p1 = (f |>> g) |>> h
                  p2 = f |>> (g |>> h)
              r1 <- runExceptT $ interpret p1 x
              r2 <- runExceptT $ interpret p2 x
              pure (r1 == r2)
        ]
    ]

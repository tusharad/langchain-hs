{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Pipeline.PipelineDSLSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Pipeline.DSL

tests :: TestTree
tests =
  testGroup
    "Langchain.Pipeline.PipelineDSLSpec"
    [ testCase "pipe operator (>>>#) composes transformations sequentially" $ do
        let step1 x = pure $ Right (x <> " -> Step1")
            step2 x = pure $ Right (x <> " -> Step2")
            pipeline = step1 >>># step2
        res <- pipeline ("Start" :: Text)
        res @?= Right "Start -> Step1 -> Step2"
    , testCase "pipeParallel executes both branches" $ do
        let branchA x = pure $ Right (T.toUpper x)
            branchB x = pure $ Right (T.length x)
            parallelStep = pipeParallel branchA branchB
        res <- parallelStep "haskell"
        res @?= Right ("HASKELL", 7)
    , testCase "runPipeline evaluates named step" $ do
        let step = mkStep "upper" (pure . Right . T.toUpper)
        res <- runExceptT $ runPipeline step "hello"
        res @?= Right "HELLO"
    ]

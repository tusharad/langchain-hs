{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Runnable.Chains (tests) where

import Langchain.Runnable.Chain
import Langchain.Runnable.Core
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

addOne :: MockRunnable Int Int
addOne = MockRunnable (\x -> return $ Right (x + 1))

multiplyByTwo :: MockRunnable Int Int
multiplyByTwo = MockRunnable (\x -> return $ Right (x * 2))

evenCheck :: MockRunnable Int Bool
evenCheck = MockRunnable $ return . Right . even

failingMock :: MockRunnable a b
failingMock = MockRunnable (\_ -> return $ Left "Mock error")

newtype MockRunnable a b = MockRunnable {runMock :: a -> IO (Either String b)}

instance Runnable (MockRunnable a b) where
  type RunnableInput (MockRunnable a b) = a
  type RunnableOutput (MockRunnable a b) = b
  invoke = runMock

tests :: TestTree
tests =
  testGroup
    "Runnable Chain Tests"
    [ testGroup
        "RunnableBranch Tests"
        [ testCase "Selects first matching branch" $ do
            let branch1 =
                  RunnableBranch
                    [ ((== 1), addOne)
                    , ((== 2), multiplyByTwo)
                    ]
                    failingMock
            result <- runBranch branch1 1
            assertEqual "Should choose addOne branch" (Right 2) result
        , testCase "Uses default when no conditions match" $ do
            let defaultBranch = RunnableBranch [] addOne
            result <- runBranch defaultBranch 5
            assertEqual "Should use default" (Right 6) result
        ]
    , testGroup
        "RunnableMap Tests"
        [ testCase "Applies input/output transformations" $ do
            let inputMap = (* 2)
                outputMap = (+ 1)
                mapped = RunnableMap inputMap outputMap addOne
            result <- runMap mapped 3 -- 3*2=6 → addOne →7 → +1 →8
            assertEqual "Transformations applied" (Right 8) result
        ]
    , testGroup
        "RunnableSequence Tests"
        [ testCase "Executes sequence in order" $ do
            let sequence0 = buildSequence addOne multiplyByTwo
            result <- runSequence sequence0 2 -- 2+1=3 → *2=6
            assertEqual "Sequence executed" (Right 6) result

            {-
            , testCase "Handles multi-step sequences" $ do
                let sequence_ = (addOne |>> multiplyByTwo) |>> evenCheck
                result <- sequence_ 3 -- 3+1=4 → *2=8 → even → True
                assertEqual "Three-step sequence" (Right True) result
                -}
        ]
    , testGroup
        "Chain Operator Tests"
        [ testCase "Chains two runnables" $ do
            let pipeline = addOne |>> multiplyByTwo
            result <- pipeline 3
            assertEqual "3+1=4 → *2=8" (Right 8) result
        , testCase "Propagates errors in chain" $ do
            let pipeline = failingMock |>> multiplyByTwo
            result <- pipeline ()
            assertEqual "Error in first step" (Left "Mock error") result
        ]
    , testGroup
        "Branch Tests"
        [ testCase "Runs parallel branches" $ do
            result <- branch evenCheck addOne 4
            assertEqual "Both branches run" (Right (True, 5)) result
        , testCase "Handles branch errors" $ do
            result <- branch failingMock addOne 5
            assertEqual
              "Left error in first branch"
              (Left "Mock error" :: Either String (Bool, Int))
              result
        ]
    ]

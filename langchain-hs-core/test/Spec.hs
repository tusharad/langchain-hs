module Main (main) where

import Test.Tasty

import qualified Test.Langchain.Core.Model as ModelTest
import qualified Test.Langchain.Core.Runnable as RunnableTest
import qualified Test.Langchain.Core.Stream as StreamTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "langchain-hs-core"
    [ RunnableTest.tests
    , ModelTest.tests
    , StreamTest.tests
    ]

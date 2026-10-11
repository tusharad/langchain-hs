module Main (main) where

import Test.Tasty

import qualified Test.Langchain.Model as ModelTest
import qualified Test.Langchain.Monad as MonadTest
import qualified Test.Langchain.Runnable as RunnableTest
import qualified Test.Langchain.Stream as StreamTest
import qualified Test.Langchain.Tool as ToolTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "langchain-hs-core"
    [ RunnableTest.tests
    , ModelTest.tests
    , StreamTest.tests
    , ToolTest.tests
    , MonadTest.tests
    ]

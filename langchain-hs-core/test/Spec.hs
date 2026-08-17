module Main (main) where

import Test.Tasty

import qualified Test.Langchain.Core.Model as ModelTest
import qualified Test.Langchain.Core.Monad as MonadTest
import qualified Test.Langchain.Core.Runnable as RunnableTest
import qualified Test.Langchain.Core.Stream as StreamTest
import qualified Test.Langchain.Core.Tool as ToolTest

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

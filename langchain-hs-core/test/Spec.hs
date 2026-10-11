module Main (main) where

import Test.Tasty

import qualified Test.Langchain.Callback as CallbackTest
import qualified Test.Langchain.Model as ModelTest
import qualified Test.Langchain.Monad as MonadTest
import qualified Test.Langchain.Observability as ObservabilityTest
import qualified Test.Langchain.PromptTemplate as PromptTemplateTest
import qualified Test.Langchain.Resilience as ResilienceTest
import qualified Test.Langchain.Retriever as RetrieverTest
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
    , PromptTemplateTest.tests
    , ObservabilityTest.tests
    , CallbackTest.tests
    , RetrieverTest.tests
    , ResilienceTest.tests
    ]

module Main (main) where

import Test.Tasty

import qualified Test.Langchain.Graph.CheckpointerSpec as CheckpointerSpec
import qualified Test.Langchain.Graph.HITLSpec as HITLSpec
import qualified Test.Langchain.Graph.MultiAgentSpec as MultiAgentSpec
import qualified Test.Langchain.Graph.StateGraphSpec as StateGraphSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "langchain-hs-graph"
    [ StateGraphSpec.tests
    , CheckpointerSpec.tests
    , HITLSpec.tests
    , MultiAgentSpec.tests
    ]

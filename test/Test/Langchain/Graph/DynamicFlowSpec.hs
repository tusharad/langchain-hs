{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.DynamicFlowSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Graph.DynamicFlow

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.DynamicFlow"
    [ testCase "topologicalSortFlow detects linear dependency" $ do
        let nodeA = FlowNode "nodeA" "source" Map.empty
            nodeB = FlowNode "nodeB" "transform" Map.empty
            nodeC = FlowNode "nodeC" "sink" Map.empty
            edge1 = FlowEdge "nodeA" "nodeB" Nothing
            edge2 = FlowEdge "nodeB" "nodeC" Nothing
            flow = newDynamicFlow "flow-1" [nodeA, nodeB, nodeC] [edge1, edge2]
        case topologicalSortFlow flow of
          Left err -> assertFailure ("Sort failed: " ++ T.unpack err)
          Right ord -> ord @?= ["nodeA", "nodeB", "nodeC"]
    , testCase "topologicalSortFlow detects cycle in flow" $ do
        let nodeA = FlowNode "nodeA" "source" Map.empty
            nodeB = FlowNode "nodeB" "sink" Map.empty
            edge1 = FlowEdge "nodeA" "nodeB" Nothing
            edge2 = FlowEdge "nodeB" "nodeA" Nothing
            flow = newDynamicFlow "flow-cycle" [nodeA, nodeB] [edge1, edge2]
        case topologicalSortFlow flow of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected cycle detection to fail"
    , testCase "executeDynamicFlow runs registry executors and accumulates outputs" $ do
        let nodeA = FlowNode "nodeA" "constant" (Map.singleton "val" (String "Hello"))
            nodeB = FlowNode "nodeB" "append" (Map.singleton "suffix" (String " World!"))
            edge = FlowEdge "nodeA" "nodeB" (Just "text")
            flow = newDynamicFlow "flow-exec" [nodeA, nodeB] [edge]

            constantEx _ params = pure $ Map.singleton "text" (Map.findWithDefault (String "") "val" params)
            appendEx _ params = do
              let base = case Map.lookup "text" params of Just (String s) -> s; _ -> ""
                  suf = case Map.lookup "suffix" params of Just (String s) -> s; _ -> ""
              pure $ Map.singleton "result" (String (base <> suf))

            registry = Map.fromList [("constant", constantEx), ("append", appendEx)]

        eRes <- runExceptT $ executeDynamicFlow registry flow Map.empty
        case eRes of
          Left err -> assertFailure ("Execution failed: " ++ show err)
          Right res -> do
            let bOut = Map.lookup "nodeB" (flowOutputs res)
            bOut @?= Just (Map.singleton "result" (String "Hello World!"))
    ]

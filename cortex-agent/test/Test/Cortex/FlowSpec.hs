{-# LANGUAGE OverloadedStrings #-}

module Test.Cortex.FlowSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Cortex.Flow.Components
import Langchain.Core.Model (MockModel (..), newMockModel)
import Langchain.Graph.DynamicFlow

tests :: TestTree
tests = testGroup "Cortex.Flow"
  [ testCase "Dynamic flow executes prompt and LLM nodes sequentially" $ do
      let model = newMockModel "AI response on functional programming."
          registry = buildCortexComponentRegistry model Nothing

          promptNode = FlowNode "p1" "prompt" (Map.singleton "template" (String "Explain: {input}"))
          llmNode = FlowNode "l1" "llm" Map.empty
          edge = FlowEdge "p1" "l1" (Just "prompt_text")
          flow = newDynamicFlow "flow-test" [promptNode, llmNode] [edge]

          initialInputs = Map.singleton "input" (String "Haskell")

      eRes <- runExceptT $ executeDynamicFlow registry flow initialInputs
      case eRes of
        Left err -> assertFailure ("Flow execution failed: " ++ show err)
        Right res -> do
          let lOut = Map.lookup "l1" (flowOutputs res)
          case lOut of
            Nothing -> assertFailure "LLM node output missing"
            Just m -> do
              let txt = Map.lookup "response" m
              txt @?= Just (String "AI response on functional programming.")
  ]

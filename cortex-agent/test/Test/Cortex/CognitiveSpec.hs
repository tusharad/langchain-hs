{-# LANGUAGE OverloadedStrings #-}

module Test.Cortex.CognitiveSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Cortex.Cognitive.Decomposer
import Cortex.Cognitive.Evaluator
import Cortex.Cognitive.Synthesizer
import Langchain.Core.Model (MockModel (..), newMockModel)
import Langchain.DocumentLoader.Core (Document (..))

tests :: TestTree
tests = testGroup "Cortex.Cognitive"
  [ testCase "parseDecomposerOutput parses instructions and subtasks" $ do
      let raw = "Instructions: Research concurrent functional programming.\n"
                <> "Reasoning: Topic has multiple facets regarding STM and actors.\n"
                <> "Tasks:\n"
                <> "1. What are Haskell STM invariants?\n"
                <> "2. How does Erlang actor model differ?\n"
          res = parseDecomposerOutput "fallback" raw
      splitInstructions res @?= "Research concurrent functional programming."
      splitReasoning res @?= "Topic has multiple facets regarding STM and actors."
      length (splitTasks res) @?= 2
      taskQuery (head (splitTasks res)) @?= "What are Haskell STM invariants?"

  , testCase "evaluateTasks determines completability and active tools" $ do
      let mockResp = "Completable: yes\nTool: none\nReasoning: Found in retrieved documents."
          model = newMockModel mockResp
          tasks = [UserTask "task-1" "Explain STM" False]
          docs = [Document "Haskell STM provides atomic transactions." Map.empty]
      eRes <- runExceptT $ evaluateTasks model tasks docs ["web_search"]
      case eRes of
        Left err -> assertFailure ("Evaluation error: " ++ show err)
        Right dec -> do
          allCompletable dec @?= True
          length (evaluatedTasks dec) @?= 1

  , testCase "synthesizeCognitiveResponse generates structured answer with citations" $ do
      let mockResp = "Haskell STM provides atomic transactions without race conditions [1]."
          model = newMockModel mockResp
          splitted = SplittedInput "Answer accurately." "Direct evidence." [UserTask "t1" "What is STM?" True]
          docs = [Document "STM guarantees ACID in memory." (Map.singleton "source" "https://haskell.org/stm")]
      eRes <- runExceptT $ synthesizeCognitiveResponse model "System Prompt" splitted docs
      case eRes of
        Left err -> assertFailure ("Synthesis error: " ++ show err)
        Right ans -> do
          ansTasksCompleted ans @?= True
          ansCitations ans @?= ["https://haskell.org/stm"]
          assertBool "Details present" (T.isInfixOf "Haskell STM" (ansDetails ans))
  ]

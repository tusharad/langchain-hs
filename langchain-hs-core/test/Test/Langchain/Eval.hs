{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Eval (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct (AgentStep (..), AgentTrace (..))
import Langchain.Error (LangchainError)
import Langchain.Eval
import Langchain.Model.Types (Role (..), ToolCall (..), assistantMessage, textMessage)
import Test.Langchain.TestModel (TestChatModel (..))

tests :: TestTree
tests =
  testGroup
    "Langchain.Eval"
    [ testCase "runEvalSuite computes pass rate, mean score, and latency across mixed cases" $ do
        let cases :: [EvalCase Text Text]
            cases =
              [ EvalCase "query-1" (Just "answer-1") Nothing
              , EvalCase "query-2" (Just "answer-2") Nothing
              ]
            mockRunner :: Text -> IO (Either LangchainError Text)
            mockRunner "query-1" = pure $ Right "answer-1"
            mockRunner "query-2" = pure $ Right "wrong-answer"
            mockRunner _ = pure $ Right ""
        report <- runEvalSuite mockRunner [exactMatchScorer] cases
        reportTotalCases report @?= 2
        reportPassedCases report @?= 1
        reportPassRate report @?= 0.5
        reportMeanScore report @?= 0.5
        assertBool "Non-negative latency" (reportMeanLatencyMicros report >= 0)
        length (reportCases report) @?= 2
    , testCase "containsScorer verifies substring presence" $ do
        let cPass :: EvalCase Text Text
            cPass = EvalCase "in" (Just "needle") Nothing
            cFail :: EvalCase Text Text
            cFail = EvalCase "in" (Just "missing") Nothing
        scPass <- scoreCase containsScorer cPass ("haystack needle haystack" :: Text)
        scorePassed scPass @?= True
        scoreValue scPass @?= 1.0
        scFail <- scoreCase containsScorer cFail ("haystack other haystack" :: Text)
        scorePassed scFail @?= False
        scoreValue scFail @?= 0.0
    , testCase "toolCallExactScorer verifies sequence of invoked tool names in AgentTrace" $ do
        let tc1 = ToolCall "c1" "function" "search" (Aeson.object [])
            tc2 = ToolCall "c2" "function" "calculator" (Aeson.object [])
            action1 = AgentAction (assistantMessage "") [tc1]
            action2 = AgentAction (assistantMessage "") [tc2]
            finish = AgentFinish (assistantMessage "Done")
            obs = textMessage Tool "obs"
            trace =
              AgentTrace
                { traceSteps = [(action1, [obs]), (action2, [obs]), (finish, [])]
                , traceFinalAnswer = assistantMessage "Done"
                , traceIterations = 3
                }
            matchingScorer = toolCallExactScorer ["search", "calculator"]
            mismatchScorer = toolCallExactScorer ["calculator", "search"]
            evalCase = EvalCase ("test" :: Text) Nothing Nothing
        resMatch <- scoreCase matchingScorer evalCase trace
        scorePassed resMatch @?= True
        scoreValue resMatch @?= 1.0
        resMismatch <- scoreCase mismatchScorer evalCase trace
        scorePassed resMismatch @?= False
        scoreValue resMismatch @?= 0.0
    , testCase "llmJudgeScorer parses JSON judge evaluation and score" $ do
        let judgeModel =
              TestChatModel
                "{\"passed\": true, \"score\": 0.95, \"reason\": \"Accurate and concise.\"}"
                "judge-mock"
            scorer = llmJudgeScorer judgeModel "Evaluate answer accuracy"
            evalCase :: EvalCase Text Text
            evalCase = EvalCase "What is 2+2?" (Just "4") Nothing
        eScore <- runExceptT $ scoreCase scorer evalCase "4"
        case eScore of
          Left err -> assertFailure ("Judge scorer failed: " ++ show err)
          Right score -> do
            scorePassed score @?= True
            scoreValue score @?= 0.95
            scoreReason score @?= Just "Accurate and concise."
    , testCase "EvalReport round-trips cleanly via Aeson encode and decode" $ do
        let score = EvalScore 1.0 True (Just "Matched perfectly")
            caseRes =
              CaseResult
                { caseInput = Aeson.String "input-1"
                , caseExpected = Just (Aeson.String "expected-1")
                , caseActual = Right (Aeson.String "actual-1")
                , caseScores = [CaseScoreResult "exact_match" score]
                , caseLatencyMicros = 1200
                , casePassed = True
                }
            report =
              EvalReport
                { reportTotalCases = 1
                , reportPassedCases = 1
                , reportPassRate = 1.0
                , reportMeanScore = 1.0
                , reportMeanLatencyMicros = 1200
                , reportCases = [caseRes]
                }
            encoded = Aeson.encode report
            decoded = Aeson.decode encoded
        decoded @?= Just report
    ]

{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Graph.MultiAgentPatternsSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (newMockModel)
import Langchain.Graph.Blackboard
import Langchain.Graph.Debate
import Langchain.Graph.Voting

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.MultiAgentPatternsSpec"
    [ testCase "Debate session runs rounds and achieves consensus" $ do
        let debaterA = Debater "Proponent" "Advocates for pure functions" (newMockModel "Purity eliminates side effects.")
            debaterB = Debater "Opponent" "Advocates for simplicity" (newMockModel "Purity requires explicit monad stacks.")
            moderator = newMockModel "CONVERGED: Pure functions with clear monad boundaries provide optimal safety."
            cfg = defaultDebateConfig "Should code be purely functional?"
        res <- runExceptT $ runDebate cfg [debaterA, debaterB] moderator
        case res of
          Left err -> assertFailure ("Debate failed: " ++ show err)
          Right (verdict, rounds) -> do
            assertBool "Debate converged" ("Pure functions" `T.isInfixOf` verdict)
            assertBool "Recorded rounds" (not $ null rounds)
    , testCase "VotingClassifier determines winner by majority vote" $ do
        let voter1 = ("ModelA", newMockModel "POSITIVE")
            voter2 = ("ModelB", newMockModel "POSITIVE")
            voter3 = ("ModelC", newMockModel "NEGATIVE")
            classifier = newVotingClassifier [voter1, voter2, voter3] "Classify sentiment"
        res <- runExceptT $ runVotingClassification classifier "Langchain-HS is awesome!"
        case res of
          Left err -> assertFailure ("Voting failed: " ++ show err)
          Right (winner, voteRecords) -> do
            winner @?= "POSITIVE"
            length voteRecords @?= 3
    , testCase "Blackboard coordinates multiple knowledge sources" $ do
        bb <- newBlackboard [("status", "pending"), ("input", "42")]
        let ks1 =
              KnowledgeSource
                "Parser"
                (\m -> Map.lookup "status" m == Just "pending")
                (\_ -> pure $ Map.fromList [("status", "parsed"), ("value", "42")])
            ks2 =
              KnowledgeSource
                "Computer"
                (\m -> Map.lookup "status" m == Just "parsed")
                (\_ -> pure $ Map.fromList [("status", "completed"), ("result", "84")])
            cfg =
              BlackboardConfig
                { maxIterations = 5
                , isComplete = \m -> Map.lookup "status" m == Just "completed"
                }
        res <- runExceptT $ runBlackboard bb [ks1, ks2] cfg
        case res of
          Left err -> assertFailure ("Blackboard failed: " ++ show err)
          Right finalState -> do
            Map.lookup "status" finalState @?= Just "completed"
            Map.lookup "result" finalState @?= Just "84"
    ]

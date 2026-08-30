{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Integration.StateGraphE2ESpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model
import Langchain.Graph.StateGraph
import Test.Langchain.TestHelpers (defaultTestModel, newTestOllama, withOllamaModel)

data GraphPipelineTestState = GraphPipelineTestState
  { originalPrompt :: Text
  , draftResponse :: Text
  , reviewNotes :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

graphStateReducer :: StateReducer GraphPipelineTestState
graphStateReducer old new =
  GraphPipelineTestState
    { originalPrompt = if T.null (originalPrompt new) then originalPrompt old else originalPrompt new
    , draftResponse = if T.null (draftResponse new) then draftResponse old else draftResponse new
    , reviewNotes = if T.null (reviewNotes new) then reviewNotes old else reviewNotes new
    }

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.StateGraphE2ESpec"
    [ testCase "StateGraph multi-node pipeline with live Ollama model" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          provider <- newTestOllama modelName
          let draftNode s = do
                let prompt = [userMessage $ "Answer concisely in one sentence: " <> originalPrompt s]
                res <- invoke provider prompt Nothing
                let txt = extractMessageText res
                pure $ Right (s {draftResponse = txt})

              reviewNode s = do
                let prompt = [userMessage $ "Review and confirm this answer: " <> draftResponse s]
                res <- invoke provider prompt Nothing
                let txt = extractMessageText res
                pure $ Right (s {reviewNotes = txt})

              g =
                addEdge "draft" "review" $
                  addEdge "review" endNodeId $
                    addNode "draft" draftNode $
                      addNode "review" reviewNode $
                        emptyStateGraph graphStateReducer

          case compileGraph g of
            Left err -> assertFailure ("Graph compilation failed: " ++ show err)
            Right cg -> do
              let initState = GraphPipelineTestState "What is 2 + 2?" "" ""
              res <- runExceptT $ runGraph cg "draft" initState
              case res of
                Left err -> assertFailure ("StateGraph run failed: " ++ show err)
                Right finalState -> do
                  assertBool "Draft response generated" (not (T.null $ draftResponse finalState))
                  assertBool "Review notes generated" (not (T.null $ reviewNotes finalState))
    ]

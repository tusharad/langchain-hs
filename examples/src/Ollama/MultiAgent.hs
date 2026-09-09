{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  Multi-Agent Supervisor and Sub-graph example using langchain-hs-graph.

  Architecture:

                    ┌──► researcherNode ──────────────────┐
                    │                                      ▼
    supervisor ─────┤                                   __end__
                    │                                      ▲
                    └──► coderNode (embedded sub-graph) ──┘
                         [coderGen ──► coderReview]

  1. The supervisor LLM evaluates the user prompt and routes to either:
     - "researcher": conceptual, architectural, or theoretical questions
     - "coder": implementation, code generation, or syntax questions
  2. The "coderNode" is an embedded sub-graph (compiled StateGraph) with its
     own internal pipeline: generate code -> review/refine code.
  3. The "researcherNode" answers directly via Ollama.
  4. Both flow to __end__.
-}
module Ollama.MultiAgent (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (extractMessageText, systemMessage, userMessage)
import Langchain.Graph.MultiAgent
  ( embedSubGraphNodeWithStart
  , supervisorNode
  )
import Langchain.Graph.StateGraph
  ( Node (..)
  , addConditionalEdge
  , addEdge
  , addNode
  , compileGraph
  , emptyStateGraph
  , endNodeId
  , replaceFieldReducer
  , runGraph
  )
import Langchain.Prelude (invoke)
import Langchain.Provider.Ollama (defaultConfig, newOllama)

-- ---------------------------------------------------------------------------
-- Top-level parent graph state
-- ---------------------------------------------------------------------------

data ParentState = ParentState
  { taskPrompt :: T.Text
  , selectedRoute :: T.Text
  , finalAnswer :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Sub-graph state (for the specialized coding sub-agent pipeline)
-- ---------------------------------------------------------------------------

data CoderSubState = CoderSubState
  { codeTask :: T.Text
  , rawCode :: T.Text
  , reviewedCode :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type App = ExceptT LangchainError IO

runApp :: IO ()
runApp = do
  let modelName = "gemma3"
  o <- newOllama modelName defaultConfig

  T.putStrLn "=== Initializing Multi-Agent System with Ollama ==="

  -- -------------------------------------------------------------------------
  -- 1. Construct the specialized Coder Sub-Graph
  -- -------------------------------------------------------------------------
  let coderGenNode s = do
        liftIO $ T.putStrLn "  [Coder Sub-Graph] Generating initial code implementation..."
        let prompt =
              T.unlines
                [ "Write a minimal, idiomatic Haskell implementation for the following requirement."
                , "Only output the code, no markdown commentary."
                , "Requirement: " <> codeTask s
                ]
            msgs = [systemMessage "You are an expert Haskell software engineer.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {rawCode = out}

      coderReviewNode s = do
        liftIO $ T.putStrLn "  [Coder Sub-Graph] Reviewing and formatting code..."
        let prompt =
              T.unlines
                [ "Review and improve this Haskell code. Ensure types are explicit and clean."
                , "Code to review:"
                , rawCode s
                ]
            msgs = [systemMessage "You are a senior Haskell code reviewer.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {reviewedCode = out}

      liftSubNode f s = fmap Right (f s)

      subGraphDef =
        addEdge "coderReview" endNodeId
          . addEdge "coderGen" "coderReview"
          . addNode "coderReview" (liftSubNode coderReviewNode)
          . addNode "coderGen" (liftSubNode coderGenNode)
          $ emptyStateGraph replaceFieldReducer

  compiledSubGraph <- case compileGraph subGraphDef of
    Left err -> error $ "Failed to compile coder sub-graph: " ++ show err
    Right sg -> pure sg

  -- -------------------------------------------------------------------------
  -- 2. Embed Coder Sub-Graph as a single node in Parent Graph
  -- -------------------------------------------------------------------------
  let toSubState p =
        CoderSubState
          { codeTask = taskPrompt p
          , rawCode = ""
          , reviewedCode = ""
          }
      fromSubState p sub =
        p {finalAnswer = reviewedCode sub}

      embeddedCoderNode =
        embedSubGraphNodeWithStart
          "coderNode"
          "coderGen"
          compiledSubGraph
          toSubState
          fromSubState

  -- -------------------------------------------------------------------------
  -- 3. Construct Researcher node
  -- -------------------------------------------------------------------------
  let researcherNode s = do
        liftIO $ T.putStrLn "  [Researcher Agent] Researching conceptual explanation..."
        let prompt =
              T.unlines
                [ "Explain the following technical concept clearly and concisely in 2-3 paragraphs."
                , "Question: " <> taskPrompt s
                ]
            msgs = [systemMessage "You are a computer science research specialist.", userMessage prompt]
        resp <- invoke o msgs Nothing
        let out = extractMessageText resp
        pure s {finalAnswer = out}

      liftParentNode f s = fmap Right (f s)

  -- -------------------------------------------------------------------------
  -- 4. Supervisor Routing Node
  -- -------------------------------------------------------------------------
  let routes =
        [ ("researcher", "researcherNode")
        , ("coder", "coderNode")
        ]
      supNode =
        supervisorNode
          o
          "supervisor"
          routes
          taskPrompt
          (\target s -> s {selectedRoute = target})

  -- -------------------------------------------------------------------------
  -- 5. Compile Parent Multi-Agent Graph
  -- -------------------------------------------------------------------------
  let parentGraphDef =
        addEdge "researcherNode" endNodeId
          . addEdge "coderNode" endNodeId
          . addConditionalEdge "supervisor" (pure . Right . selectedRoute)
          . addNode "researcherNode" (liftParentNode researcherNode)
          . addNode "coderNode" (nodeAction embeddedCoderNode)
          . addNode "supervisor" (nodeAction supNode)
          $ emptyStateGraph replaceFieldReducer

  compiledParentGraph <- case compileGraph parentGraphDef of
    Left err -> error $ "Failed to compile parent graph: " ++ show err
    Right g -> pure g

  -- -------------------------------------------------------------------------
  -- 6. Test with a coding task
  -- -------------------------------------------------------------------------
  let codingTask =
        ParentState
          { taskPrompt = "Write a function `fibonacci :: Int -> Integer` with memoization."
          , selectedRoute = ""
          , finalAnswer = ""
          }

  T.putStrLn "\n--- Dispatching Task 1: Coding Request ---"
  T.putStrLn $ "User: " <> taskPrompt codingTask
  res1 <- runExceptT $ runGraph compiledParentGraph "supervisor" codingTask
  case res1 of
    Left err -> T.putStrLn $ "Error in run 1: " <> T.pack (show err)
    Right finalSt -> do
      T.putStrLn $ "Route Selected: " <> selectedRoute finalSt
      T.putStrLn "=== Result ==="
      T.putStrLn (finalAnswer finalSt)

  -- -------------------------------------------------------------------------
  -- 7. Test with a conceptual research task
  -- -------------------------------------------------------------------------
  let researchTask =
        ParentState
          { taskPrompt = "What is the difference between Lazy and Strict evaluation in functional languages?"
          , selectedRoute = ""
          , finalAnswer = ""
          }

  T.putStrLn "\n--- Dispatching Task 2: Conceptual Research Request ---"
  T.putStrLn $ "User: " <> taskPrompt researchTask
  res2 <- runExceptT $ runGraph compiledParentGraph "supervisor" researchTask
  case res2 of
    Left err -> T.putStrLn $ "Error in run 2: " <> T.pack (show err)
    Right finalSt -> do
      T.putStrLn $ "Route Selected: " <> selectedRoute finalSt
      T.putStrLn "=== Result ==="
      T.putStrLn (finalAnswer finalSt)

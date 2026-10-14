{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  Production StateGraph example powered by Google Gemini.

  Topology:

                     ┌────────────────────────┐
                     │                        ▼
    init ──► planner ──► writer ──► reviewer ──► evaluator ──┬──► __end__
                                                  ▲          │
                                                  └── refiner ┘

  Demonstrates:
  * 'addTransformNode' for deterministic state transformations and evaluations.
  * 'addChatNode' for seamless text-in / text-out LLM execution via ChatModel.
  * 'addModelNode' for rich structured Message-level prompt & response LLM execution.
  * 'addConditionalEdge' for adaptive cyclical routing (critique-refine loop).
  * 'setRecursionLimit' to safeguard against unbounded cycles.
  * 'setStepLogger' for full observability and audit tracing.
-}
module Gemini.StateGraph (runApp) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import Gemini.Common (defaultGeminiModelName, getGeminiModel)
import Langchain.Graph.StateGraph
  ( addChatNode
  , addConditionalEdge
  , addEdge
  , addModelNode
  , addTransformNode
  , compileGraph
  , emptyStateGraph
  , endNodeId
  , replaceFieldReducer
  , runGraph
  , setRecursionLimit
  , setStepLogger
  )
import Langchain.Model
  ( extractMessageText
  , systemMessage
  , userMessage
  )

-- ---------------------------------------------------------------------------
-- Graph State
-- ---------------------------------------------------------------------------

data ResearchState = ResearchState
  { topic :: !T.Text
  , plan :: !T.Text
  , draft :: !T.Text
  , reviewFeedback :: !T.Text
  , isApproved :: !Bool
  , iterationCount :: !Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

initialState :: T.Text -> ResearchState
initialState t =
  ResearchState
    { topic = t
    , plan = ""
    , draft = ""
    , reviewFeedback = ""
    , isApproved = False
    , iterationCount = 0
    }

-- ---------------------------------------------------------------------------
-- File & Console Logging
-- ---------------------------------------------------------------------------

logFilePath :: FilePath
logFilePath = "logs-gemini-stategrph.txt"

appendLog :: T.Text -> IO ()
appendLog msg = do
  T.putStrLn msg
  T.appendFile logFilePath (msg <> "\n")

-- ---------------------------------------------------------------------------
-- Application Entry
-- ---------------------------------------------------------------------------

runApp :: IO ()
runApp = do
  -- Reset or initialize log file
  T.writeFile logFilePath "=== Gemini StateGraph Execution Log ===\n\n"

  appendLog ">>> Initializing Gemini Model..."
  gemini <- getGeminiModel defaultGeminiModelName

  let userTopic = "STM (Software Transactional Memory) vs MVCC in Concurrent Haskell Systems"

  appendLog $ ">>> Target Research Topic: " <> userTopic

  -- -------------------------------------------------------------------------
  -- Define StateGraph Nodes
  -- -------------------------------------------------------------------------

  let -- Node 1: Pure transformation to prime initial state
      initNode s =
        s
          { topic = T.strip (topic s)
          , iterationCount = 0
          , isApproved = False
          }

      -- Node 2: ChatModel node using addChatNode (text in -> text out)
      plannerPrompt s =
        T.unlines
          [ "You are an expert systems architect."
          , "Create a focused 3-point outline covering the theoretical and practical aspects of:"
          , topic s
          , "Format output as 3 concise bullet points with key headings."
          ]
      plannerMerge s outline =
        s {plan = outline}

      -- Node 3: Model node using addModelNode (structured [Message] in -> Message out)
      writerMessages s =
        [ systemMessage
            "You are a principal Haskell engineer writing concise documentation. Limit your response to 2 focused paragraphs with explicit type signatures."
        , userMessage $
            T.unlines
              [ "Write a concise 2-paragraph technical comparison based on this outline:"
              , plan s
              , ""
              , "Topic: " <> topic s
              ]
        ]
      writerMerge s respMsg =
        s {draft = extractMessageText respMsg}

      -- Node 4: Reviewer node using addChatNode
      reviewerPrompt s =
        T.unlines
          [ "You are a technical reviewer."
          , "Evaluate the following technical draft."
          , "If it accurately compares the concurrency concepts, respond with 'STATUS: APPROVED' followed by 1 summary sentence."
          , "Otherwise, respond with 'STATUS: REVISE:' followed by 1 brief suggestion."
          , ""
          , "--- Draft ---"
          , draft s
          ]
      reviewerMerge s review =
        s {reviewFeedback = review}

      -- Node 5: Pure evaluator node inspecting feedback
      evaluatorNode s =
        let feedbackUpper = T.toUpper (reviewFeedback s)
            approved = "APPROVED" `T.isInfixOf` feedbackUpper
         in s
              { isApproved = approved
              , iterationCount = iterationCount s + 1
              }

      -- Node 6: Refinement node if critique was given
      refinerPrompt s =
        T.unlines
          [ "You are revising a technical document based on reviewer critique."
          , "Critique: " <> reviewFeedback s
          , "Draft: " <> draft s
          , "Provide a concise 2-paragraph revised version incorporating the suggestions."
          ]
      refinerMerge s revised =
        s {draft = revised}

      -- Conditional routing logic after evaluation
      routingDecision s =
        if isApproved s || iterationCount s >= 2
          then pure $ Right endNodeId
          else pure $ Right "refiner"

  -- -------------------------------------------------------------------------
  -- Assemble Graph
  -- -------------------------------------------------------------------------
  appendLog ">>> Assembling Declarative StateGraph..."

  let rawGraph =
        addEdge "refiner" "reviewer"
          . addConditionalEdge "evaluator" routingDecision
          . addEdge "reviewer" "evaluator"
          . addEdge "writer" "reviewer"
          . addEdge "planner" "writer"
          . addEdge "init" "planner"
          . addChatNode "refiner" gemini refinerPrompt refinerMerge
          . addTransformNode "evaluator" evaluatorNode
          . addChatNode "reviewer" gemini reviewerPrompt reviewerMerge
          . addModelNode "writer" gemini writerMessages writerMerge
          . addChatNode "planner" gemini plannerPrompt plannerMerge
          . addTransformNode "init" initNode
          $ emptyStateGraph replaceFieldReducer

  -- Configure graph execution parameters
  let configuredGraph =
        setStepLogger appendLog
          . setRecursionLimit 15
          $ rawGraph

  -- -------------------------------------------------------------------------
  -- Compile & Validate Graph
  -- -------------------------------------------------------------------------
  appendLog ">>> Compiling and Validating StateGraph..."
  case compileGraph configuredGraph of
    Left err -> appendLog $ "StateGraph compilation failed: " <> T.pack (show err)
    Right compiledGraph -> do
      appendLog ">>> StateGraph compiled successfully! Running pipeline..."
      res <- runExceptT $ runGraph compiledGraph "init" (initialState userTopic)
      case res of
        Left err -> appendLog $ "StateGraph execution error: " <> T.pack (show err)
        Right finalSt -> do
          appendLog "\n========================================================"
          appendLog "               STATEGRAPH RUN COMPLETED"
          appendLog "========================================================"
          appendLog $ "Total Iterations: " <> T.pack (show (iterationCount finalSt))
          appendLog $ "Approved: " <> T.pack (show (isApproved finalSt))
          appendLog "\n--- Final Architectural Outline ---"
          appendLog (plan finalSt)
          appendLog "\n--- Reviewer Assessment ---"
          appendLog (reviewFeedback finalSt)
          appendLog "\n--- Final Generated Technical Content ---"
          appendLog (draft finalSt)
          appendLog "\n>>> All execution logs have been written to logs-gemini-stategrph.txt"

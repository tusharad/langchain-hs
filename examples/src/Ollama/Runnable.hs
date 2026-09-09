{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  Pure Runnable AST Pipeline Example (LCEL in idiomatic Haskell).

  Demonstrates how every LangChain component implements 'Runnable' and can be
  algebraically composed into a tree/graph shape using pure functional operators:

    - '(|>>)': Sequential pipe (Arrow composition)
    - '(&>&)': Concurrent parallel fan-out (runs branches concurrently via async)
    - 'runPassthrough': Identity passthrough (like LangChain's RunnablePassthrough)
    - 'runPure': Pure function lifting (zero side-effects)
    - 'runBranch': Predicate-based conditional routing AST node
    - 'runFallback': Failure recovery fallback AST node
    - 'runPrim': Lifts any component with a 'Runnable' instance into the tree

  Pipeline Architecture:

                               ┌──► runPrim bm25Index |>> runPure formatDocs ────────┐
                               │                                                      │
    Question: Text ────────────┼──► runPure parseMathExpr |>> runPrim calculatorTool  ┼──► Par (&>&)
                               │                                                      │
                               └──► runPassthrough (preserves question) ──────────────┘
                                                                                      │
                                                                                      ▼
                                                                       runPure packPromptVars
                                                                                      │
                                                                                      ▼
                                                                       runPrim promptTemplate
                                                                                      │
                                                                                      ▼
                                                                       runChat primaryModel
                                                                         `runFallback`
                                                                       runChat backupModel
                                                                                      │
                                                                                      ▼
                                                                       runBranch hasExplanation
                                                                         formatFinalOutput
                                                                         refinePipeline
-}
module Ollama.Runnable (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import Langchain.Core.Runnable (runIdent)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt
  ( PromptTemplate (..)
  , TemplateFormat (..)
  , fromTemplateWithFormat
  )
import Langchain.Provider.Ollama (defaultConfig, newOllama)
import Langchain.Retriever.BM25 (newBM25Index)
import Langchain.Tool.Calculator (calculatorTool)

-- ---------------------------------------------------------------------------
-- Domain Knowledge Base (Mock Documents for Retrieval)
-- ---------------------------------------------------------------------------

knowledgeDocs :: [Document]
knowledgeDocs =
  [ Document
      "Haskell concurrency is built on lightweight threads (threads managed by the GHC runtime). \
      \They cost only a few hundred bytes each, allowing millions of concurrent threads. \
      \Channels (TChan) and MVar provide synchronized communication with zero data races."
      Map.empty
  , Document
      "Backpressure in stream processing ensures producers do not overwhelm consumers. \
      \In Haskell, bounded channels (TBQueue in STM) or Conduit/Pipes provide automatic backpressure. \
      \When a TBQueue is full, the producer thread automatically blocks inside STM."
      Map.empty
  , Document
      "Queue throughput sizing: To calculate optimal throughput Q = R * T, where R is arrival rate \
      \and T is average processing latency. For example: 5000 requests/sec * 0.040 sec = 200 items in flight."
      Map.empty
  ]

runApp :: IO ()
runApp = do
  T.putStrLn "=== Pure RunnableTree Pipeline (LCEL in Haskell) ==="

  -- 1. Initialize LLM Models
  primaryModel <- newOllama "gemma3" defaultConfig
  backupModel <- newOllama "gemma3" defaultConfig

  -- 2. Component 1: BM25 Knowledge Retriever (implements Runnable)
  let bm25 = newBM25Index knowledgeDocs

  -- 3. Component 2: Calculator Tool (implements Runnable Value -> Text)
  let calcTool = calculatorTool :: Tool (ExceptT LangchainError IO)

  -- 4. Component 3: PromptTemplate (implements Runnable Map Text Text -> Text)
  let ragTemplate =
        fromTemplateWithFormat
          ( T.unlines
              [ "You are an expert distributed systems architect."
              , "Use the retrieved documentation and computed metric to solve the engineering query."
              , ""
              , "--- System Documentation ---"
              , "{context}"
              , ""
              , "--- Calculated Capacity Metric ---"
              , "Required In-Flight Buffer: {capacity} items"
              , ""
              , "--- Engineering Requirement ---"
              , "{question}"
              , ""
              , "Provide a concise design recommendation with a code sketch:"
              ]
          )
          FString
          Map.empty

  -- -------------------------------------------------------------------------
  -- Functional Adapters & Pure Transformations
  -- -------------------------------------------------------------------------

  -- Formats retrieved documents into a single text block
  let formatDocs :: [Document] -> Text
      formatDocs docs =
        T.intercalate "\n---\n" (map (TL.toStrict . pageContent) docs)

  -- Extracts an arithmetic expression from question to compute buffer capacity
  let extractMathExpr :: Text -> Value
      extractMathExpr _ = object ["expression" .= ("5000 * 0.040" :: Text)]

  -- Combines outputs from parallel branches into prompt template variables
  let packVars :: ((Text, Text), Text) -> Map.Map Text Text
      packVars ((ctx, cap), q) =
        Map.fromList
          [ ("context", ctx)
          , ("capacity", T.strip cap)
          , ("question", q)
          ]

  -- Formats the final answer
  let formatFinal :: Text -> Text
      formatFinal ans =
        T.unlines
          [ "\n======================================================="
          , "  Synthesized Design Recommendation via RunnableTree"
          , "======================================================="
          , T.strip ans
          ]

  -- -------------------------------------------------------------------------
  -- Assemble the Composed Tree AST (PURE - No execution happens here!)
  -- -------------------------------------------------------------------------
  T.putStrLn "Constructing pure RunnableTree AST..."

  -- Sub-tree A: Parallel Knowledge Retrieval + Tool Computation
  -- Input: Text -> Output: (Text, Text)
  let retrievalAndToolBranch =
        (runPrim bm25 |>> runPure formatDocs)
          &>& (runPure extractMathExpr |>> runPrim calcTool)

  -- Sub-tree B: Fan-out combining Sub-tree A with Passthrough User Question
  -- Input: Text -> Output: ((Context, Capacity), Question)
  let fanOutBranch =
        retrievalAndToolBranch &>& runIdent

  -- Sub-tree C: Self-healing LLM Invocation with Fallback
  -- If primaryModel fails (timeout, network, OOM), backupModel seamlessly catches it
  let robustModelStep =
        runChat primaryModel `runFallback` runChat backupModel

  -- Sub-tree D: Conditional Refinement Node
  -- If response is too brief (< 80 chars), route through an elaboration node
  let isBriefAnswer ans = pure (T.length (T.strip ans) < 80)
      elaborationStep =
        runPure ("Elaborate in detail on: " <>)
          |>> runChat backupModel

  let conditionalOutputStep =
        runBranch isBriefAnswer elaborationStep (runPure id)

  -- Complete Pipeline Composition
  let fullPipeline =
        fanOutBranch
          |>> runPure packVars
          |>> runPrim ragTemplate
          |>> robustModelStep
          |>> conditionalOutputStep
          |>> runPure formatFinal

  -- -------------------------------------------------------------------------
  -- Execute the Pipeline via 'interpret'
  -- -------------------------------------------------------------------------
  let userQuery =
        "How should I size and implement a bounded channel queue for 5000 req/s with 40ms latency?"

  T.putStrLn $ "\n[Input Query]: " <> userQuery
  T.putStrLn "Executing pure pipeline through 'interpret'..."

  res <- runExceptT $ interpret fullPipeline userQuery
  case res of
    Left err -> T.putStrLn $ "Pipeline Error: " <> errorMessage err
    Right output -> T.putStrLn output

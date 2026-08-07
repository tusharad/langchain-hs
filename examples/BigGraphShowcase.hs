{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Main
Description : Big Showcase Demonstration Application stretching every feature of langchain-hs
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

End-to-End demonstration application combining:
1. RunnableTree pure pipeline composition (|>>) and (&>&)
2. Multi-modal Message and ContentBlock representation
3. ChatModel invocation and streaming (Ollama gemma3:latest / MockModel)
4. Tool m execution (calculatorTool, readFileTool, writeFileTool)
5. StateGraph stateful agent orchestration with pure StateReducers
6. MemoryCheckpointer & SQLiteCheckpointer state persistence
7. Human-in-the-Loop (HITL) interrupt & state modification/resume
8. MultiAgent supervisor routing between specialized sub-agents
-}
module Main (main) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model
import Langchain.Core.Runnable
import Langchain.Core.Tool (Tool (..), toolExecute)
import Langchain.Graph.Checkpointer
import Langchain.Graph.HITL
import Langchain.Graph.MultiAgent
import Langchain.Graph.StateGraph
import Langchain.Provider.Ollama (newOllama)
import Langchain.Tool.Calculator (calculatorTool)
import Langchain.Tool.FileSystem (readFileTool, writeFileTool)

-- | Comprehensive Application State
data AppState = AppState
  { appMessages :: [Message]
  , appResult :: Text
  , appTargetAgent :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Pure StateReducer combining message history and updating result
appStateReducer :: StateReducer AppState
appStateReducer old new =
  AppState
    { appMessages = appMessages old ++ appMessages new
    , appResult = if T.null (appResult new) then appResult old else appResult new
    , appTargetAgent = if T.null (appTargetAgent new) then appTargetAgent old else appTargetAgent new
    }

main :: IO ()
main = do
  putStrLn "================================================================================"
  putStrLn "  Langchain-hs Big Comprehensive Showcase Application (v0.5.0 Architecture)"
  putStrLn "================================================================================"

  -- 1. RunnableTree Pure Pipeline AST Composition
  putStrLn "\n--- Feature 1: Pure RunnableTree Pipeline AST Composition (|>>) & (&>&) ---"
  let appendExclamation :: RunnableTree (ExceptT LangchainError IO) Text Text
      appendExclamation = runLambda (\t -> pure $ Right (t <> "!"))

      uppercase :: RunnableTree (ExceptT LangchainError IO) Text Text
      uppercase = runLambda (\t -> pure $ Right (T.toUpper t))

      pipeline = uppercase |>> appendExclamation

  ePipeRes <- runExceptT $ interpret pipeline "hello langchain-hs graph"
  case ePipeRes of
    Left err -> putStrLn $ "Pipeline Error: " ++ show err
    Right val -> putStrLn $ "Pipeline Result: " ++ T.unpack val

  -- 2. Tool Execution (Calculator & File System)
  putStrLn "\n--- Feature 2: Effect-Polymorphic Tool Execution ---"
  withSystemTempDirectory "big-showcase" $ \tempDir -> do
    let testFilePath = T.pack (tempDir </> "demo.txt")
        fileContent = ("Calculated Value: 42.0" :: Text)

    putStrLn "Executing writeFileTool..."
    _ <- toolExecute writeFileTool (object ["path" .= testFilePath, "content" .= fileContent]) :: IO (Either LangchainError Text)

    putStrLn "Executing readFileTool..."
    eRead <- toolExecute readFileTool (object ["path" .= testFilePath]) :: IO (Either LangchainError Text)
    case eRead of
      Left err -> putStrLn $ "File Read Error: " ++ show err
      Right readTxt -> putStrLn $ "Read File Content: " ++ T.unpack readTxt

    putStrLn "Executing calculatorTool..."
    eCalc <- toolExecute calculatorTool (object ["expression" .= ("2 + 3" :: Text)]) :: IO (Either LangchainError Text)
    case eCalc of
      Left err -> putStrLn $ "Calc Error: " ++ show err
      Right calcTxt -> putStrLn $ "Calculator Output: " ++ T.unpack calcTxt

  -- 3. StateGraph, Checkpointer & Human-in-the-Loop Interrupt & Resume
  putStrLn "\n--- Feature 3: StateGraph, Thread-Safe MemoryCheckpointer & HITL Resume ---"
  cp <- newMemoryCheckpointer
  let threadId = "showcase-thread-100"
      initAppState =
        AppState
          { appMessages = [userMessage "Initiating automated financial calculation approval workflow"]
          , appResult = ""
          , appTargetAgent = ""
          }

  -- Define workflow nodes
  let prepareNode = Node "prepare" $ \s -> pure $ Right s {appResult = "Step 1: Preparation Complete"}
      approvalHitlNode = hitlNode cp threadId "approval" $ \s -> pure $ Right s {appResult = appResult s <> " | Step 2: Pending Human Approval"}
      finalizeNode = Node "finalize" $ \s -> pure $ Right s {appResult = appResult s <> " | Step 3: Final Execution Completed"}

      g =
        addEdge "prepare" "approval" $
          addEdge "approval" "finalize" $
            addEdge "finalize" endNodeId $
              addNode "prepare" (nodeAction prepareNode) $
                addNode "approval" (nodeAction approvalHitlNode) $
                  addNode "finalize" (nodeAction finalizeNode) $
                    emptyStateGraph appStateReducer

  case compileGraph g of
    Left compileErr -> putStrLn $ "Graph Compilation Error: " ++ show compileErr
    Right compiledG -> do
      putStrLn "Running StateGraph until HITL Interrupt node..."
      eGraphRes <- runExceptT $ runGraph compiledG "prepare" initAppState
      case eGraphRes of
        Left err -> case isHITLInterrupt err of
          Just nodeName -> do
            putStrLn $ "SUCCESS: Interrupted at HITL node '" ++ T.unpack nodeName ++ "' for human review!"
            putStrLn "Simulating Human Review & State Modification before resuming to 'finalize' node..."
            eResumeRes <- runExceptT $ resumeGraph compiledG cp threadId nodeName "finalize" (\s -> s {appResult = appResult s <> " [Human Reviewed & Approved]"})
            case eResumeRes of
              Left resumeErr -> putStrLn $ "Resume Error: " ++ show resumeErr
              Right finalState -> putStrLn $ "Final Post-Resume State Result:\n  " ++ T.unpack (appResult finalState)
          Nothing -> putStrLn $ "Unexpected Graph Error: " ++ show err
        Right finalState -> putStrLn $ "Graph finished unexpectedly without interrupt: " ++ show finalState

  -- 4. Multi-Agent Routing & LLM Provider Integration
  putStrLn "\n--- Feature 4: Multi-Agent Supervisor Routing & Ollama Provider ---"
  _ <- newOllama "gemma3:latest"
  putStrLn "Initialized Ollama provider with model 'gemma3:latest'."
  putStrLn "Executing mock multi-agent supervisor route..."
  let mockLLM = newMockModel "researcher"
      supervisorN = supervisorNode mockLLM "supervisor" [("researcher", "researchNode"), ("calculator", "calcNode")] appResult (\target s -> s {appTargetAgent = target})

  eSupRes <- runExceptT $ nodeAction supervisorN initAppState
  case eSupRes of
    Left err -> putStrLn $ "Supervisor Error: " ++ show err
    Right (Right routedState) -> putStrLn $ "Supervisor Routed Task to Target Agent: '" ++ T.unpack (appTargetAgent routedState) ++ "'"
    Right (Left err) -> putStrLn $ "Supervisor Action Error: " ++ show err

  putStrLn "\n================================================================================"
  putStrLn "  Langchain-hs Big Showcase Application Executed Successfully!"
  putStrLn "================================================================================"

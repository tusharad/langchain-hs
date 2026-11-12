{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Graph.StateGraphSpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (fuseUpstream, runConduit, yield)
import qualified Data.Conduit.List as CL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Error (LangchainError, errorMessage)
import Langchain.Graph.StateGraph
import Langchain.Model (ChatModel (..), assistantMessage, extractMessageText, userMessage)
import Langchain.Runnable (RunnableTree (..), runPure, (|>>))
import Langchain.Stream (StreamEvent (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

newtype MockStreamModel = MockStreamModel [Text]

instance ChatModel MockStreamModel where
  type ModelConfig MockStreamModel = ()

  invoke (MockStreamModel chunks) _ _ =
    pure $ assistantMessage (T.concat chunks)

  stream (MockStreamModel chunks) msgs _ = do
    yield $ LLMStart "mock-run" "mock-stream" msgs
    mapM_ (\c -> yield $ LLMChunk "mock-run" c Nothing) chunks
    yield $ LLMEnd "mock-run" (assistantMessage (T.concat chunks)) Nothing

tests :: TestTree
tests =
  testGroup
    "Langchain.Graph.StateGraphSpec"
    [ testCase "compileGraph succeeds on valid node graph" $ do
        let action :: Text -> ExceptT LangchainError IO (Either LangchainError Text)
            action s = pure $ Right (s <> " world")
            g = addNode "node1" action $ emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right _ -> pure ()
    , testCase "runGraph executes static transitions correctly" $ do
        let action :: Text -> ExceptT LangchainError IO (Either LangchainError Text)
            action s = pure $ Right (s <> " world")
            g =
              addEdge "node1" endNodeId $
                addNode "node1" action $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "node1" ("hello" :: Text)
            res @?= Right "hello world"
    , testCase "addTransformNode updates state purely" $ do
        let g =
              addEdge "step1" endNodeId $
                addTransformNode "step1" (<> " [transformed]") $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "step1" ("input" :: Text)
            res @?= Right "input [transformed]"
    , testCase "addRunnableTreeNode executes pipeline AST within graph node" $ do
        let pipeline :: RunnableTree (ExceptT LangchainError IO) Text Text
            pipeline = runPure (<> " -> p1") |>> runPure (<> " -> p2")
            g =
              addEdge "runnableNode" endNodeId $
                addRunnableTreeNode "runnableNode" pipeline id (\_ out -> out) $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "runnableNode" ("start" :: Text)
            res @?= Right "start -> p1 -> p2"
    , testCase "recursionLimit terminates cyclic graph with error" $ do
        let g =
              setRecursionLimit 5 $
                addEdge "loop" "loop" $
                  addTransformNode "loop" (\n -> (n + 1 :: Int)) $
                    emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compilation failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "loop" (0 :: Int)
            case res of
              Left err ->
                assertBool
                  "Contains recursion limit message"
                  ("recursion limit exceeded" `T.isInfixOf` errorMessage err)
              Right val -> assertFailure $ "Expected recursion error, but got: " ++ show val
    , testCase "addSubGraphNode embeds and executes sub-graph cleanly" $ do
        let subG =
              addEdge "sub1" endNodeId $
                addTransformNode "sub1" (<> "_sub") $
                  emptyStateGraph replaceFieldReducer
        case compileGraph subG of
          Left err -> assertFailure $ "Sub-graph compile failed: " ++ show err
          Right compiledSub -> do
            let parentG =
                  addEdge "parent1" endNodeId $
                    addSubGraphNode "parent1" "sub1" compiledSub id (\_ subOut -> "parent:" <> subOut) $
                      emptyStateGraph replaceFieldReducer
            case compileGraph parentG of
              Left err -> assertFailure $ "Parent compile failed: " ++ show err
              Right compiledParent -> do
                res <- runExceptT $ runGraph compiledParent "parent1" ("init" :: Text)
                res @?= Right "parent:init_sub"
    , testCase "setStepLogger logs transition messages" $ do
        logsVar <- newTVarIO []
        let logger msg = atomically $ modifyTVar' logsVar (msg :)
            g =
              setStepLogger logger $
                addEdge "a" "b" $
                  addEdge "b" endNodeId $
                    addTransformNode "a" (<> "_A") $
                      addTransformNode "b" (<> "_B") $
                        emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compile failed: " ++ show err
          Right compiled -> do
            res <- runExceptT $ runGraph compiled "a" ("start" :: Text)
            res @?= Right "start_A_B"
            logs <- readTVarIO logsVar
            assertBool "Recorded log messages" (length logs >= 4)
            assertBool "Logged entering node 'a'" (any ("Entering node 'a'" `T.isInfixOf`) logs)
    , testCase "streamGraph emits NodeStart and NodeEnd in execution order and returns final state" $ do
        let g =
              addEdge "n1" "n2" $
                addEdge "n2" endNodeId $
                  addTransformNode "n1" (<> "->n1") $
                    addTransformNode "n2" (<> "->n2") $
                      emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compile failed: " ++ show err
          Right compiled -> do
            eventsRef <- newIORef ([] :: [StreamEvent])
            res <-
              runExceptT $
                runConduit $
                  streamGraph compiled "n1" ("start" :: Text)
                    `fuseUpstream` CL.mapM_ (\ev -> liftIO $ modifyIORef' eventsRef (++ [ev]))
            res @?= Right "start->n1->n2"
            events <- readIORef eventsRef
            let tags =
                  [ (if isStart then "start:" else "end:") <> nid
                  | ev <- events
                  , (isStart, nid) <- case ev of
                      NodeStart _ n _ -> [(True, n)]
                      NodeEnd _ n _ -> [(False, n)]
                      _ -> []
                  ]
            tags @?= ["start:n1", "end:n1", "start:n2", "end:n2"]
    , testCase "addStreamingModelNode interleaves LLMStart, LLMChunk, LLMEnd between NodeStart and NodeEnd" $ do
        let model = MockStreamModel ["Hello ", "Graph"]
            g =
              addEdge "llm" endNodeId $
                addStreamingModelNode "llm" model (\t -> [userMessage t]) (\_ msg -> extractMessageText msg) $
                  emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compile failed: " ++ show err
          Right compiled -> do
            eventsRef <- newIORef ([] :: [StreamEvent])
            res <-
              runExceptT $
                runConduit $
                  streamGraph compiled "llm" ("hi" :: Text)
                    `fuseUpstream` CL.mapM_ (\ev -> liftIO $ modifyIORef' eventsRef (++ [ev]))
            res @?= Right "Hello Graph"
            events <- readIORef eventsRef
            let summary = map eventTag events
            summary
              @?= [ "NodeStart:llm"
                  , "LLMStart"
                  , "LLMChunk:Hello "
                  , "LLMChunk:Graph"
                  , "LLMEnd"
                  , "NodeEnd:llm"
                  ]
    , testCase "streamGraph throws AgentError after NodeStart when recursionLimit is exceeded" $ do
        let g =
              setRecursionLimit 1 $
                addEdge "loop" "loop" $
                  addTransformNode "loop" (+ (1 :: Int)) $
                    emptyStateGraph replaceFieldReducer
        case compileGraph g of
          Left err -> assertFailure $ "Compile failed: " ++ show err
          Right compiled -> do
            eventsRef <- newIORef ([] :: [StreamEvent])
            res <-
              runExceptT $
                runConduit $
                  streamGraph compiled "loop" (0 :: Int)
                    `fuseUpstream` CL.mapM_ (\ev -> liftIO $ modifyIORef' eventsRef (++ [ev]))
            case res of
              Left err ->
                assertBool "Mentions recursion limit" ("recursion limit exceeded" `T.isInfixOf` errorMessage err)
              Right val -> assertFailure $ "Expected recursion error, got: " ++ show val
            events <- readIORef eventsRef
            let starts = [n | NodeStart _ n _ <- events]
            starts @?= ["loop"]
    , testProperty "linear chain of n nodes emits strictly alternating NodeStart/NodeEnd pairs" $
        \(Positive rawN) -> ioProperty $ do
          let n = ((rawN - 1) `mod` 15) + 1
              nodeNames = ["node-" <> T.pack (show i) | i <- [1 .. n :: Int]]
              withNodes =
                foldr (\nid -> addTransformNode nid (+ (1 :: Int))) (emptyStateGraph replaceFieldReducer) nodeNames
              edges = zip nodeNames (drop 1 nodeNames ++ [endNodeId])
              withEdges = foldr (uncurry addEdge) withNodes edges
          case compileGraph withEdges of
            Left err -> pure $ counterexample (show err) False
            Right compiled -> do
              eventsRef <- newIORef ([] :: [StreamEvent])
              res <-
                runExceptT $
                  runConduit $
                    streamGraph compiled "node-1" (0 :: Int)
                      `fuseUpstream` CL.mapM_ (\ev -> liftIO $ modifyIORef' eventsRef (++ [ev]))
              events <- readIORef eventsRef
              let expected :: [(Text, Text)]
                  expected =
                    concat [[("start", nid), ("end", nid)] | nid <- nodeNames]
                  actual =
                    [ tag
                    | ev <- events
                    , tag <- case ev of
                        NodeStart _ nid _ -> [("start", nid)]
                        NodeEnd _ nid _ -> [("end", nid)]
                        _ -> []
                    ]
              pure $ res === Right n .&&. actual === expected
    ]
  where
    eventTag (NodeStart _ nid _) = "NodeStart:" <> nid
    eventTag (NodeEnd _ nid _) = "NodeEnd:" <> nid
    eventTag (LLMStart {}) = "LLMStart"
    eventTag (LLMChunk _ c _) = "LLMChunk:" <> c
    eventTag (LLMEnd {}) = "LLMEnd"
    eventTag _ = "Other"

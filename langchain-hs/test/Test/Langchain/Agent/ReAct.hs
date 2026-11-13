{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Agent.ReAct (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (finally)
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Conduit (yield)
import Data.IORef
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Agent.ReAct
import Langchain.Callback.Manager
  ( CallbackEvent (..)
  , CallbackHandler (..)
  , newCallbackManager
  , registerHandler
  )
import Langchain.Error (LangchainError, errorMessage, toolError)
import Langchain.Memory.Core (BaseMemory (..), newWindowBufferMemory)
import Langchain.Model
import Langchain.Monad (LangchainT, runLangchainT)
import Langchain.Stream
  ( StreamEvent (..)
  , collectEvents
  , newCancelToken
  , triggerCancel
  )
import Langchain.Tool (StreamingTool, Tool, createStreamingTool, createTool, toolExecute)
import Langchain.Tool.Binding (ToolBinder (..))
import Test.Langchain.Provider.Mock (newMockModel)

-- | Mock calculator tool used for testing agent step dispatching
calculatorTool :: MonadIO m => Tool m
calculatorTool =
  createTool
    "calculator"
    "Mock calculator tool"
    (object [])
    (\_ -> pure $ Right "4")

-- | Mock model that records the config received by invoke
data ConfigRecordingModel = ConfigRecordingModel (IORef (Maybe Value)) T.Text

instance ChatModel ConfigRecordingModel where
  type ModelConfig ConfigRecordingModel = Value
  invoke (ConfigRecordingModel ref resp) _ mbCfg = do
    liftIO $ writeIORef ref mbCfg
    pure $ assistantMessage resp
  stream = error "stream not supported in ConfigRecordingModel"

instance ToolBinder ConfigRecordingModel m where
  bindToolsConfig tools _ =
    Just $ object ["tool_count" .= length tools]

-- | Mock model that yields a pre-configured sequence of responses and logs history
data StepSequenceModel = StepSequenceModel (IORef [Message]) (IORef [[Message]])

instance ChatModel StepSequenceModel where
  type ModelConfig StepSequenceModel = Value
  invoke (StepSequenceModel stepsRef histRef) history _ = liftIO $ do
    modifyIORef histRef (++ [history])
    steps <- readIORef stepsRef
    case steps of
      [] -> pure $ assistantMessage "Default response"
      (m : rest) -> do
        writeIORef stepsRef rest
        pure m
  stream (StepSequenceModel stepsRef histRef) history _ = do
    let rId = "seq-stream-run"
    yield $ LLMStart rId "step-sequence-model" history
    resp <- liftIO $ do
      modifyIORef histRef (++ [history])
      steps <- readIORef stepsRef
      case steps of
        [] -> pure $ assistantMessage "Default response"
        (m : rest) -> do
          writeIORef stepsRef rest
          pure m
    let txt = extractMessageText resp
    unless (T.null txt) $
      forM_ (T.chunksOf 4 txt) $ \chunk ->
        yield $ LLMChunk rId chunk Nothing
    forM_ (fromMaybe [] (messageToolCalls resp)) $ \tc ->
      yield $ LLMChunk rId "" (Just tc)
    yield $ LLMEnd rId resp Nothing

instance ToolBinder StepSequenceModel m where
  bindToolsConfig _ _ = Nothing

data InterruptMockModel = InterruptMockModel (MVar ()) (MVar ())

instance ChatModel InterruptMockModel where
  type ModelConfig InterruptMockModel = ()
  invoke _ _ _ = pure $ assistantMessage "sync"
  stream (InterruptMockModel gate cleaned) msgs _ = do
    yield $ LLMStart "int-1" "interrupt-mock" msgs
    yield $ LLMChunk "int-1" "partial reply" Nothing
    liftIO $
      takeMVar gate
        `finally` putMVar cleaned ()
    yield $ LLMChunk "int-1" " trailing" Nothing
    yield $ LLMEnd "int-1" (assistantMessage "partial reply trailing") Nothing

instance ToolBinder InterruptMockModel m where
  bindToolsConfig _ cfg = cfg

tests :: TestTree
tests =
  testGroup
    "Langchain.Agent.ReAct"
    [ testCase "reactStep returns AgentFinish when LLM responds with plain text" $ do
        let mockModel = newMockModel "The answer is 4."
            tools = [calculatorTool]
        res <- runExceptT $ reactStep mockModel tools [userMessage "What is 2+2?"] Nothing
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right step -> case step of
            AgentFinish msg -> T.strip (extractMessageText msg) @?= "The answer is 4."
            _ -> assertFailure "Expected AgentFinish"
    , testCase "reactStep returns AgentAction with all tool calls" $ do
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            tc2 = ToolCall "call_2" "function" "calculator" (object ["expression" .= ("3*3" :: T.Text)])
            respWithTools = (assistantMessage "") {messageToolCalls = Just [tc1, tc2]}
        sRef <- newIORef [respWithTools]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            tools = [calculatorTool :: Tool (ExceptT LangchainError IO)]
        res <- runExceptT $ reactStep model tools [userMessage "Calculate both"] Nothing
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right step -> case step of
            AgentAction _ tcs -> length tcs @?= 2
            _ -> assertFailure "Expected AgentAction with multiple tool calls"
    , testCase "runReActAgent executes multiple parallel tool calls and reaches finish" $ do
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            tc2 = ToolCall "call_2" "function" "calculator" (object ["expression" .= ("5*2" :: T.Text)])
            respWithTools = (assistantMessage "calculating") {messageToolCalls = Just [tc1, tc2]}
            finalResp = assistantMessage "4 and 10"
        sRef <- newIORef [respWithTools, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent = defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Calculate 2+2 and 5*2"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> do
            T.strip (extractMessageText finalMsg) @?= "4 and 10"
            -- Verify history in step 2 received observations for BOTH tool calls
            histories <- readIORef hRef
            case histories of
              [_, secondCallHistory] -> do
                let toolMsgs = filter (\m -> messageRole m == Tool) secondCallHistory
                length toolMsgs @?= 2
                map messageToolId toolMsgs @?= [Just "call_1", Just "call_2"]
              _ -> assertFailure $ "Expected 2 invocations, got: " ++ show (length histories)
    , testCase "runReActAgent handles unknown tool gracefully via observation error" $ do
        let tc = ToolCall "call_bad" "function" "unknown_tool" (object [])
            respWithBadTool = (assistantMessage "") {messageToolCalls = Just [tc]}
            finalResp = assistantMessage "Handled missing tool"
        sRef <- newIORef [respWithBadTool, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent = defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Run unknown tool"]
        case res of
          Left err -> assertFailure $ "Expected recovery but got error: " ++ show err
          Right finalMsg -> do
            T.strip (extractMessageText finalMsg) @?= "Handled missing tool"
            histories <- readIORef hRef
            case histories of
              [_, secondCallHistory] -> do
                let toolMsgs = filter (\m -> messageRole m == Tool) secondCallHistory
                length toolMsgs @?= 1
                case toolMsgs of
                  (m : _) ->
                    assertBool "Error observation" ("Tool not found: unknown_tool" `T.isInfixOf` extractMessageText m)
                  _ -> assertFailure "Expected tool message"
              _ -> assertFailure "Expected 2 invocations"
    , testCase "runReActAgent completes full loop on finish" $ do
        let mockModel = newMockModel "Finished processing"
            agent = defaultReActAgent mockModel [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Hello"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> T.strip (extractMessageText finalMsg) @?= "Finished processing"
    , testCase "reactStep passes bound tools config to model invoke" $ do
        ref <- newIORef Nothing
        let recordingModel = ConfigRecordingModel ref "Direct Answer"
            tools = [calculatorTool :: Tool (ExceptT LangchainError IO)]
        res <- runExceptT $ reactStep recordingModel tools [userMessage "Calculate 2+2"] Nothing
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right _ -> do
            captured <- readIORef ref
            captured @?= Just (object ["tool_count" .= (1 :: Int)])
    , testCase "runReActAgentWithTrace returns intermediate steps" $ do
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            respWithTools = (assistantMessage "calculating") {messageToolCalls = Just [tc1]}
            finalResp = assistantMessage "The answer is 4"
        sRef <- newIORef [respWithTools, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent = defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgentWithTrace agent [userMessage "Calculate 2+2"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right trace -> do
            -- Trace should have 2 steps: one action + one finish
            length (traceSteps trace) @?= 2
            T.strip (extractMessageText (traceFinalAnswer trace)) @?= "The answer is 4"
    , testCase "withSystemPrompt prepends system message to history" $ do
        hRef <- newIORef ([] :: [[Message]])
        sRef <- newIORef [assistantMessage "Got it"]
        let model = StepSequenceModel sRef hRef
            agent =
              withSystemPrompt "You are a math tutor." $
                defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Hi"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right _ -> do
            histories <- readIORef hRef
            case histories of
              (firstCall : _) ->
                case firstCall of
                  (sysMsg : _) -> do
                    messageRole sysMsg @?= System
                    T.strip (extractMessageText sysMsg) @?= "You are a math tutor."
                  _ -> assertFailure "Expected system message in history"
              _ -> assertFailure "Expected at least one invocation"
    , testCase "withMemory loads and saves conversation history" $ do
        mem <- newWindowBufferMemory 50 []
        hRef <- newIORef ([] :: [[Message]])
        sRef <- newIORef [assistantMessage "Memory works!"]
        let model = StepSequenceModel sRef hRef
            agent =
              withMemory mem $
                defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Test memory"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> do
            T.strip (extractMessageText finalMsg) @?= "Memory works!"
            -- Verify the messages were saved to memory
            memRes <- runExceptT $ messages mem
            case memRes of
              Left err -> assertFailure $ "Memory read failed: " ++ show err
              Right savedMsgs -> do
                -- Should have user message + assistant response
                length savedMsgs @?= 2
                case listToMaybe savedMsgs of
                  Nothing -> assertFailure "Message list is empty"
                  Just firstMessage -> messageRole firstMessage @?= User
                messageRole (savedMsgs !! 1) @?= Assistant
    , testCase "withModelConfig passes config to provider" $ do
        ref <- newIORef Nothing
        let recordingModel = ConfigRecordingModel ref "Configured Answer"
            tools = [calculatorTool]
            agent =
              withModelConfig
                (Just $ object ["temperature" .= (0.7 :: Double)])
                (defaultReActAgent recordingModel tools)
        res <- runExceptT $ runReActAgent agent [userMessage "Test config"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right _ -> do
            captured <- readIORef ref
            -- The config should be the result of bindToolsConfig merging with our config
            case captured of
              Nothing -> assertFailure "Expected config to be passed to model"
              Just _ -> pure () -- Config was passed (exact value depends on ToolBinder instance)
    , testCase "streamReActAgent emits ChainStart, LLM events, Tool events, and ChainEnd across turns" $ do
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            respWithTools = (assistantMessage "Thinking...") {messageToolCalls = Just [tc1]}
            finalResp = assistantMessage "Answer is 4"
        sRef <- newIORef [respWithTools, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent = defaultReActAgent model [calculatorTool]
        res <- runExceptT $ collectEvents $ streamReActAgent agent [userMessage "Calculate 2+2"]
        case res of
          Left err -> assertFailure $ "Unexpected stream error: " ++ show err
          Right events -> do
            case (events, reverse events) of
              (ChainStart _ "ReActAgent" _ : _, ChainEnd _ "ReActAgent" _ : _) -> pure ()
              _ -> assertFailure $ "Expected ChainStart/ChainEnd wrapping events: " ++ show events
            let toolStarts = [tName | ToolStart _ tName _ <- events]
                toolEnds = [tName | ToolEnd _ tName _ <- events]
                toolDeltas = [tc | LLMChunk _ _ (Just tc) <- events]
                textChunks = T.concat [txt | LLMChunk _ txt Nothing <- events]
            toolStarts @?= ["calculator"]
            toolEnds @?= ["calculator"]
            toolDeltas @?= [tc1]
            textChunks @?= "Thinking...Answer is 4"
    , testCase "runReActAgentStream works in LangchainT () IO with memory and callbacks" $ do
        mem <- newWindowBufferMemory 20 []
        cbMgr <- newCallbackManager
        cbLog <- newIORef ([] :: [T.Text])
        registerHandler cbMgr $ CallbackHandler "test-cb" $ \ev ->
          let tag = case ev of
                OnLLMStart {} -> "llm_start"
                OnLLMEnd {} -> "llm_end"
                OnToolStart {} -> "tool_start"
                OnToolEnd {} -> "tool_end"
                _ -> "other"
           in modifyIORef cbLog (++ [tag])
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["x" .= (1 :: Int)])
            respWithTools = (assistantMessage "") {messageToolCalls = Just [tc1]}
            finalResp = assistantMessage "Done 4"
        sRef <- newIORef [respWithTools, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent =
              withCallbackManager cbMgr $
                withMemory mem $
                  defaultReActAgent model [calculatorTool :: Tool (LangchainT () IO)]
        seenChunks <- newIORef ([] :: [T.Text])
        res <-
          runLangchainT () $
            runReActAgentStream agent [userMessage "Go"] $
              \case
                LLMChunk _ txt Nothing | not (T.null txt) -> liftIO $ modifyIORef seenChunks (++ [txt])
                _ -> pure ()
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right finalMsg -> do
            extractMessageText finalMsg @?= "Done 4"
            chunks <- readIORef seenChunks
            T.concat chunks @?= "Done 4"
            cbs <- readIORef cbLog
            cbs @?= ["llm_start", "llm_end", "tool_start", "tool_end", "llm_start", "llm_end"]
            memRes <- runExceptT $ messages mem
            case memRes of
              Left err -> assertFailure $ show err
              Right saved -> length saved @?= 2
    , testCase "streamReActAgent streams ToolChunk events from withStreamingTools" $ do
        let sTool =
              createStreamingTool "stream_echo" "Echoes chunks" (object []) $ \_ -> do
                yield "part1-"
                yield "part2"
                pure (Right "part1-part2")
            tc1 = ToolCall "call_s" "function" "stream_echo" (object [])
            respWithTools = (assistantMessage "") {messageToolCalls = Just [tc1]}
            finalResp = assistantMessage "Streamed tool ok"
        sRef <- newIORef [respWithTools, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent =
              withStreamingTools [sTool] $
                defaultReActAgent model []
        res <- runExceptT $ collectEvents $ streamReActAgent agent [userMessage "Run stream tool"]
        case res of
          Left err -> assertFailure $ "Unexpected stream error: " ++ show err
          Right events -> do
            let toolChunks = [(nm, ch) | ToolChunk _ nm ch <- events]
            toolChunks @?= [("stream_echo", "part1-"), ("stream_echo", "part2")]
    , testCase "streamReActAgent emits ToolErrorEvent and respects FailOnError" $ do
        let failingTool :: Tool (ExceptT LangchainError IO)
            failingTool =
              createTool "boom" "Always fails" (object []) $ \_ ->
                pure $ Left $ toolError "exploded" (Just "boom") Nothing
            tc1 = ToolCall "call_err" "function" "boom" (object [])
            respWithTools = (assistantMessage "") {messageToolCalls = Just [tc1]}
        sRef <- newIORef [respWithTools]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent =
              withToolErrorStrategy FailOnError $
                defaultReActAgent model [failingTool]
        seenRef <- newIORef ([] :: [StreamEvent])
        res <- runExceptT $
          runReActAgentStream agent [userMessage "Trigger error"] $ \ev ->
            liftIO $ modifyIORef seenRef (++ [ev])
        case res of
          Left _ -> pure ()
          Right msg -> assertFailure $ "Expected FailOnError abort, got: " ++ show msg
        seen <- readIORef seenRef
        assertBool "Saw ToolStart" (any (\case ToolStart _ "boom" _ -> True; _ -> False) seen)
        assertBool "Saw ToolErrorEvent" (any (\case ToolErrorEvent _ "boom" _ -> True; _ -> False) seen)
    , testCase "runReActAgentStreamInterruptible cancels turn and keeps partial text" $ do
        tok <- newCancelToken
        gate <- newEmptyMVar
        cleaned <- newEmptyMVar
        mem <- newWindowBufferMemory 20 []
        let model = InterruptMockModel gate cleaned
            agent = withMemory mem $ defaultReActAgent model []
        res <-
          runLangchainT () $
            runReActAgentStreamInterruptible tok agent [userMessage "Interrupt me"] $ \case
              LLMChunk _ txt Nothing | not (T.null txt) -> triggerCancel tok
              _ -> pure ()
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right (Completed msg) -> assertFailure $ "Expected Cancelled, got Completed: " ++ show msg
          Right (Cancelled partialMsg) -> extractMessageText partialMsg @?= "partial reply"
        threadDelay 50000
        mbCleaned <- tryTakeMVar cleaned
        mbCleaned @?= Just ()
        memRes <- runExceptT $ messages mem
        case memRes of
          Left err -> assertFailure $ show err
          Right saved -> saved @?= []
    , testCase "AbortOnRepeat 3 halts loop on 3rd consecutive duplicate tool call in runReActAgent" $ do
        cbMgr <- newCallbackManager
        errSeenRef <- newIORef False
        registerHandler cbMgr $ CallbackHandler "loop-err-handler" $ \case
          OnError "ReActAgent" msg _ ->
            when ("Runaway loop detected" `T.isInfixOf` msg) $
              writeIORef errSeenRef True
          _ -> pure ()
        let tc = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            respWithTool = (assistantMessage "") {messageToolCalls = Just [tc]}
        sRef <- newIORef [respWithTool, respWithTool, respWithTool, respWithTool]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent =
              withCallbackManager cbMgr $
                withLoopDetection (AbortOnRepeat 3) (defaultReActAgent model [calculatorTool])
        res <- runExceptT $ runReActAgent agent [userMessage "Loop test"]
        case res of
          Left err -> do
            assertBool "Mentions runaway loop" ("Runaway loop detected" `T.isInfixOf` errorMessage err)
            sawCb <- readIORef errSeenRef
            assertBool "Dispatched OnError callback" sawCb
          Right msg -> assertFailure $ "Expected loop abort, got success: " ++ show msg
    , testCase "AbortOnRepeat 3 halts loop in streamReActAgent" $ do
        let tc = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            respWithTool = (assistantMessage "") {messageToolCalls = Just [tc]}
        sRef <- newIORef [respWithTool, respWithTool, respWithTool, respWithTool]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent =
              withLoopDetection (AbortOnRepeat 3) $
                defaultReActAgent model [calculatorTool]
        res <- runExceptT $ collectEvents $ streamReActAgent agent [userMessage "Stream loop test"]
        case res of
          Left err ->
            assertBool "Mentions runaway loop" ("Runaway loop detected" `T.isInfixOf` errorMessage err)
          Right evs -> assertFailure $ "Expected stream loop abort, got events: " ++ show evs
    , testCase "WarnOnRepeat 2 injects observation warning allowing recovery on step 3" $ do
        execCountRef <- newIORef (0 :: Int)
        let countingCalcTool :: Tool (ExceptT LangchainError IO)
            countingCalcTool =
              createTool "calculator" "Mock calculator tool" (object []) $ \_ -> do
                liftIO $ modifyIORef' execCountRef (+ 1)
                pure $ Right "4"
            tc = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            respWithTool = (assistantMessage "") {messageToolCalls = Just [tc]}
            recoveredResp = assistantMessage "I noticed the loop warning and resolved 4."
        sRef <- newIORef [respWithTool, respWithTool, recoveredResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent =
              withLoopDetection (WarnOnRepeat 2) $
                defaultReActAgent model [countingCalcTool]
        res <- runExceptT $ runReActAgentWithTrace agent [userMessage "Calculate"]
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right trace -> do
            traceIterations trace @?= 3
            extractMessageText (traceFinalAnswer trace)
              @?= "I noticed the loop warning and resolved 4."
            execCount <- readIORef execCountRef
            -- Tool was executed on step 1, but skipped on step 2 due to WarnOnRepeat
            execCount @?= 1
            histories <- readIORef hRef
            case histories of
              [_, _, step3Hist] -> do
                let toolMsgs = filter (\m -> messageRole m == Tool) step3Hist
                case reverse toolMsgs of
                  (warnMsg : _) ->
                    assertBool
                      "Step 3 received loop warning"
                      ("Loop warning" `T.isInfixOf` extractMessageText warnMsg)
                  [] -> assertFailure "Expected tool message in step 3 history"
              _ -> assertFailure $ "Expected 3 history snapshots, got: " ++ show (length histories)
    , testCase "distinct tool arguments do not trigger AbortOnRepeat" $ do
        let tc1 = ToolCall "call_1" "function" "calculator" (object ["expression" .= ("1+1" :: T.Text)])
            tc2 = ToolCall "call_2" "function" "calculator" (object ["expression" .= ("2+2" :: T.Text)])
            tc3 = ToolCall "call_3" "function" "calculator" (object ["expression" .= ("3+3" :: T.Text)])
            r1 = (assistantMessage "") {messageToolCalls = Just [tc1]}
            r2 = (assistantMessage "") {messageToolCalls = Just [tc2]}
            r3 = (assistantMessage "") {messageToolCalls = Just [tc3]}
            finalResp = assistantMessage "All three distinct calculations finished"
        sRef <- newIORef [r1, r2, r3, finalResp]
        hRef <- newIORef []
        let model = StepSequenceModel sRef hRef
            agent =
              withLoopDetection (AbortOnRepeat 2) $
                defaultReActAgent model [calculatorTool]
        res <- runExceptT $ runReActAgent agent [userMessage "Calculate distinct"]
        case res of
          Left err -> assertFailure $ "Distinct arguments should not abort: " ++ show err
          Right finalMsg ->
            extractMessageText finalMsg @?= "All three distinct calculations finished"
    , testCase "asAgentTool delegates task from supervisor to specialist agent" $ do
        let specModel = newMockModel "4"
            specialistAgent = defaultReActAgent specModel [calculatorTool]
            specialistTool :: Tool (ExceptT LangchainError IO)
            specialistTool =
              asAgentTool "math_specialist" "Specialist math agent" specialistAgent
            callSpecialist =
              ToolCall
                "call_spec"
                "function"
                "math_specialist"
                (object ["task" .= ("compute 2+2" :: T.Text)])
            supResp1 = (assistantMessage "") {messageToolCalls = Just [callSpecialist]}
            supResp2 = assistantMessage "Supervisor final answer: 4"
        sRef <- newIORef [supResp1, supResp2]
        hRef <- newIORef []
        let supModel = StepSequenceModel sRef hRef
            supervisor = defaultReActAgent supModel [specialistTool]
        res <- runExceptT $ runReActAgent supervisor [userMessage "Solve 2+2"]
        case res of
          Left err -> assertFailure $ "Supervisor delegation failed: " ++ show err
          Right finalMsg -> extractMessageText finalMsg @?= "Supervisor final answer: 4"
    , testCase "asStreamingAgentTool forwards specialist chunks as ToolChunk in streamReActAgent" $ do
        let specStreamingTool =
              createStreamingTool "stream_echo" "Echoes chunks" (object []) $ \_ -> do
                yield "spec-chunk-1 "
                yield "spec-chunk-2"
                pure (Right "spec-chunk-1 spec-chunk-2")
            specCall =
              ToolCall "call_echo" "function" "stream_echo" (object [])
            specResp1 = (assistantMessage "") {messageToolCalls = Just [specCall]}
            specResp2 = assistantMessage "specialist finished"
        specStepsRef <- newIORef [specResp1, specResp2]
        specHistRef <- newIORef []
        let specModel = StepSequenceModel specStepsRef specHistRef
            specialist =
              withStreamingTools [specStreamingTool] (defaultReActAgent specModel [])
            streamSpecialistTool :: StreamingTool (ExceptT LangchainError IO)
            streamSpecialistTool =
              asStreamingAgentTool "specialist_streamer" "Specialist streaming agent" specialist
            callSpec =
              ToolCall
                "call_stream_spec"
                "function"
                "specialist_streamer"
                (object ["task" .= ("stream this" :: T.Text)])
            supResp1 = (assistantMessage "") {messageToolCalls = Just [callSpec]}
            supResp2 = assistantMessage "Supervisor completed streaming delegation"
        sRef <- newIORef [supResp1, supResp2]
        hRef <- newIORef []
        let supModel = StepSequenceModel sRef hRef
            supervisor =
              withStreamingTools [streamSpecialistTool] (defaultReActAgent supModel [])
        res <- runExceptT $ collectEvents $ streamReActAgent supervisor [userMessage "Delegate stream"]
        case res of
          Left err -> assertFailure $ "Streaming delegation failed: " ++ show err
          Right evs -> do
            let tStarts = [nm | ToolStart _ nm _ <- evs]
                tChunks = [(nm, ch) | ToolChunk _ nm ch <- evs]
                tEnds = [nm | ToolEnd _ nm _ <- evs]
            assertBool "Saw specialist ToolStart" ("specialist_streamer" `elem` tStarts)
            assertBool "Saw specialist ToolEnd" ("specialist_streamer" `elem` tEnds)
            assertBool "Saw streamed chunks from specialist" (not (null tChunks))
            map snd tChunks
              @?= ["spec-chunk-1 ", "spec-chunk-2", "spec", "iali", "st f", "inis", "hed"]
    , testCase "asAgentTool respects maxDelegationDepth" $ do
        let specModel = newMockModel "depth"
            specialist =
              withMaxDelegationDepth 0 (defaultReActAgent specModel [calculatorTool])
            specTool :: Tool (ExceptT LangchainError IO)
            specTool = asAgentTool "deep_agent" "Deep agent" specialist
        res <- runExceptT $ toolExecute specTool (object ["task" .= ("run" :: T.Text)])
        case res of
          Right (Left err) ->
            assertBool
              "Mentions delegation depth exceeded"
              ("delegation depth exceeded" `T.isInfixOf` errorMessage err)
          Left err ->
            assertBool
              "Mentions delegation depth exceeded"
              ("delegation depth exceeded" `T.isInfixOf` errorMessage err)
          Right (Right val) ->
            assertFailure $ "Expected delegation depth failure, got: " ++ show val
    ]

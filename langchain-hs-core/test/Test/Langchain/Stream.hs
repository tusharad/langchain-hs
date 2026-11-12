{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Stream (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (finally)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (object, (.=))
import Data.Conduit (runConduit, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import Langchain.Error (llmError)
import Langchain.Model
import Langchain.Monad (runLangchainT)
import Langchain.Stream
import Test.Langchain.TestModel (TestChatModel (..))

tests :: TestTree
tests =
  testGroup
    "Langchain.Stream"
    [ testCase "stream emits LLMStart, LLMChunk, LLMEnd events" $ do
        let model = TestChatModel "Streamed content" "mock-gpt"
            input = [userMessage "Stream test"]
        res <- runResourceT $ runExceptT $ collectEvents (stream model input Nothing)
        case res of
          Left err -> assertFailure $ "Unexpected stream error: " ++ show err
          Right events -> do
            length events @?= 3
            case events of
              [s@LLMStart {}, c@LLMChunk {}, e@LLMEnd {}] -> do
                modelName s @?= "mock-gpt"
                chunkText c @?= "Streamed content"
                extractMessageText (finalMessage e) @?= "Streamed content"
              _ -> assertFailure $ "Unexpected event sequence: " ++ show events
    , testCase "liftChatStream forwards events into LangchainT () IO" $ do
        let model = TestChatModel "Bridged stream" "mock-gpt"
            input = [userMessage "Bridge test"]
        res <- runLangchainT () $ collectEvents $ liftChatStream (stream model input Nothing)
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right events -> do
            length events @?= 3
            case reverse events of
              LLMEnd _ msg _ : _ -> extractMessageText msg @?= "Bridged stream"
              _ -> assertFailure $ "Expected LLMEnd, got: " ++ show events
    , testCase "liftChatStream propagates LangchainError mid-stream" $ do
        let errStream :: ChatStream
            errStream = do
              yield $ LLMStart "r1" "mock" [userMessage "Hi"]
              yield $ LLMChunk "r1" "partial" Nothing
              throwError $ llmError "mid-stream boom" Nothing Nothing
        res <- runExceptT $ collectEvents $ liftChatStream errStream
        case res of
          Left _ -> pure ()
          Right events -> assertFailure $ "Expected failure, got: " ++ show events
    , testCase "runStreamWith invokes callback and resets across LLMStart turns" $ do
        let tc = ToolCall "call_1" "function" "calc" (object ["x" .= (1 :: Int)])
            multiTurnStream :: EventStream IO
            multiTurnStream = do
              yield $ LLMStart "r1" "mock" []
              yield $ LLMChunk "r1" "Calling tool..." Nothing
              yield $ LLMChunk "r1" "" (Just tc)
              yield $ LLMEnd "r1" ((assistantMessage "Calling tool...") {messageToolCalls = Just [tc]}) Nothing
              yield $ ToolStart "r1" "calc" (object ["x" .= (1 :: Int)])
              yield $ ToolEnd "r1" "calc" (object ["res" .= (2 :: Int)])
              yield $ LLMStart "r2" "mock" []
              yield $ LLMChunk "r2" "Final " Nothing
              yield $ LLMChunk "r2" "answer" Nothing
        seenRef <- newIORef ([] :: [StreamEvent])
        finalMsg <- runStreamWith (\ev -> liftIO $ modifyIORef' seenRef (ev :)) multiTurnStream
        seen <- readIORef seenRef
        length seen @?= 9
        extractMessageText finalMsg @?= "Final answer"
        messageToolCalls finalMsg @?= Nothing
    , testCase "liftChatStreamResource cancels upstream producer within 200ms on early take 1" $ do
        cleanedUp <- newEmptyMVar
        let slowUpstream :: ChatStream
            slowUpstream = do
              yield $ LLMStart "slow" "mock" []
              liftIO $
                threadDelay 5000000
                  `finally` putMVar cleanedUp ()
              yield $ LLMChunk "slow" "late" Nothing
        t0 <- getCurrentTime
        res <-
          runResourceT $
            runExceptT $
              runConduit $
                liftChatStreamResource slowUpstream .| CL.take 1
        t1 <- getCurrentTime
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right evs -> length evs @?= 1
        threadDelay 50000
        mbDone <- tryTakeMVar cleanedUp
        mbDone @?= Just ()
        let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
        assertBool ("Expected cancellation < 0.2s, got: " ++ show elapsed) (elapsed < 0.2)
    , testCase "liftChatStreamWithCancel stops upstream on CancelToken and preserves partial text" $ do
        tok <- newCancelToken
        cleanedUp <- newEmptyMVar
        gate <- newEmptyMVar :: IO (MVar ())
        let partialUpstream :: ChatStream
            partialUpstream = do
              yield $ LLMStart "c1" "mock" []
              yield $ LLMChunk "c1" "partial-" Nothing
              liftIO $
                takeMVar gate
                  `finally` putMVar cleanedUp ()
              yield $ LLMChunk "c1" "never" Nothing
        res <-
          runLangchainT () $
            runStreamWith
              ( \case
                  LLMChunk {} -> triggerCancel tok
                  _ -> pure ()
              )
              (liftChatStreamWithCancel tok partialUpstream)
        case res of
          Left err -> assertFailure $ "Unexpected error: " ++ show err
          Right msg -> extractMessageText msg @?= "partial-"
        threadDelay 50000
        mbDone <- tryTakeMVar cleanedUp
        mbDone @?= Just ()
    ]

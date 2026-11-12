{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
Module      : Langchain.Stream
Description : Standardized StreamEvent protocol and utilities
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Structured streaming event protocol for tracking LLM, tool, chain, and graph lifecycle events.
-}
module Langchain.Stream
  ( TokenUsage (..)
  , StreamEvent (..)
  , StreamM
  , EventStream
  , ChatStream
  , StreamCallback
  , StreamSource
  , callbackSource
  , liftChatStream
  , liftChatStreamResource
  , CancelToken
  , newCancelToken
  , triggerCancel
  , isCancelled
  , waitCancel
  , liftChatStreamWithCancel
  , tapStreamMessage
  , consumeStreamWith
  , runStreamWith
  , collectEvents
  , printEvents
  ) where

import Control.Concurrent.Async (Async, AsyncCancelled (..), async, cancel, wait)
import Control.Concurrent.STM
  ( STM
  , TBQueue
  , TMVar
  , atomically
  , newEmptyTMVarIO
  , newTBQueueIO
  , orElse
  , putTMVar
  , readTBQueue
  , readTMVar
  , tryPutTMVar
  , tryReadTMVar
  , writeTBQueue
  )
import Control.Exception
  ( Exception (displayException)
  , SomeException
  , catch
  , finally
  , fromException
  , onException
  , throwIO
  )
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Conduit (ConduitT, Void, await, bracketP, fuseUpstream, runConduit, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Langchain.Error (LangchainError, llmError)
import Langchain.Model.Types (Message (..), ToolCall, assistantMessage)

-- | Token usage accounting for LLM execution.
data TokenUsage = TokenUsage
  { promptTokens :: Int
  , completionTokens :: Int
  , totalTokens :: Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

{- | All streaming events emitted across the framework execution lifecycle.
Every event carries a 'runId' for correlation.
-}
data StreamEvent
  = -- | LLM lifecycle start
    LLMStart
      { runId :: Text
      , modelName :: Text
      , inputMessages :: [Message]
      }
  | -- | LLM incremental streaming chunk
    LLMChunk
      { runId :: Text
      , chunkText :: Text
      , toolCallDelta :: Maybe ToolCall
      }
  | -- | LLM lifecycle completion
    LLMEnd
      { runId :: Text
      , finalMessage :: Message
      , tokenUsage :: Maybe TokenUsage
      }
  | -- | Tool execution start
    ToolStart
      { runId :: Text
      , toolName :: Text
      , toolInput :: Value
      }
  | -- | Tool incremental streaming output chunk
    ToolChunk
      { runId :: Text
      , toolName :: Text
      , toolChunkText :: Text
      }
  | -- | Tool execution completion
    ToolEnd
      { runId :: Text
      , toolName :: Text
      , toolOutput :: Value
      }
  | -- | Tool execution failure
    ToolErrorEvent
      { runId :: Text
      , toolName :: Text
      , toolErrorPayload :: LangchainError
      }
  | -- | Chain execution start
    ChainStart
      { runId :: Text
      , chainName :: Text
      , chainInput :: Value
      }
  | -- | Chain execution completion
    ChainEnd
      { runId :: Text
      , chainName :: Text
      , chainOutput :: Value
      }
  | -- | Graph node execution start
    NodeStart
      { runId :: Text
      , nodeId :: Text
      , nodeState :: Value
      }
  | -- | Graph node execution completion
    NodeEnd
      { runId :: Text
      , nodeId :: Text
      , nodeState :: Value
      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Canonical event stream type using Conduit.
type EventStream m = ConduitT () StreamEvent m ()

-- | Effects used by resource-safe chat model streams.
type StreamM = ExceptT LangchainError (ResourceT IO)

-- | A resource-safe stream of chat model events.
type ChatStream = EventStream StreamM

-- | A callback function that produces values of type @a@.
type StreamCallback a = (a -> IO ()) -> IO ()

-- | A Conduit source that produces values of type @a@ in the 'StreamM' monad.
type StreamSource a = ConduitT () a StreamM ()

-- | Convert a callback-based streaming function into a Conduit source.
callbackSource :: StreamCallback a -> StreamSource a
callbackSource produce = bracketP start (cancel . third) consume
  where
    start = do
      queue <- newTBQueueIO 64
      finished <- newEmptyTMVarIO
      worker <-
        async $ produce (atomically . writeTBQueue queue) `finally` atomically (putTMVar finished ())
      pure (queue, finished, worker)

    consume (queue, finished, _worker) = loop
      where
        loop = do
          let waitForFinished = Nothing <$ readTMVar finished
              readEvent = Just <$> readTBQueue queue
          next <- liftIO . atomically $ readEvent `orElse` waitForFinished
          case next of
            Just item -> yield item >> loop
            Nothing -> pure ()

    third (_, _, worker) = worker

-- | Cooperative single-shot cancellation token backed by an STM 'TMVar'.
newtype CancelToken = CancelToken (TMVar ())
  deriving (Eq)

-- | Allocate a new untriggered 'CancelToken'.
newCancelToken :: MonadIO m => m CancelToken
newCancelToken = CancelToken <$> liftIO newEmptyTMVarIO

-- | Signal cancellation on a 'CancelToken' (idempotent).
triggerCancel :: MonadIO m => CancelToken -> m ()
triggerCancel (CancelToken var) = liftIO . atomically . void $ tryPutTMVar var ()

-- | Check whether a 'CancelToken' has been triggered.
isCancelled :: MonadIO m => CancelToken -> m Bool
isCancelled (CancelToken var) = liftIO . atomically $ isJust <$> tryReadTMVar var

waitCancel :: CancelToken -> STM ()
waitCancel (CancelToken var) = readTMVar var

startChatWorker ::
  ChatStream ->
  IO (TBQueue StreamEvent, TMVar (Maybe LangchainError), Async ())
startChatWorker chatStream = do
  q <- newTBQueueIO 64
  done <- newEmptyTMVarIO
  errRef <- newIORef Nothing
  w <-
    async $
      ( do
          eRes <-
            runResourceT $
              runExceptT $
                runConduit $
                  chatStream .| CL.mapM_ (liftIO . atomically . writeTBQueue q)
          case eRes of
            Right () -> pure ()
            Left err -> writeIORef errRef (Just err)
      )
        `catch` ( \ex -> case fromException ex of
                    Just AsyncCancelled -> throwIO (ex :: SomeException)
                    Nothing ->
                      writeIORef
                        errRef
                        (Just $ llmError (T.pack (displayException ex)) (Just "Stream") Nothing)
                )
        `finally` (readIORef errRef >>= atomically . void . tryPutTMVar done)
  pure (q, done, w)

consumeChatWorker ::
  (MonadIO m, MonadError LangchainError m) =>
  Maybe CancelToken ->
  (TBQueue StreamEvent, TMVar (Maybe LangchainError), Async ()) ->
  EventStream m
consumeChatWorker mbTok (queue, doneVar, worker) = loop
  where
    loop = do
      next <-
        liftIO $
          atomically
            ( case mbTok of
                Just tok ->
                  (Left Nothing <$ waitCancel tok)
                    `orElse` (Right <$> readTBQueue queue)
                    `orElse` (Left . Just <$> readTMVar doneVar)
                Nothing ->
                  (Right <$> readTBQueue queue)
                    `orElse` (Left . Just <$> readTMVar doneVar)
            )
            `onException` cancel worker
      case next of
        Right ev -> do
          yield ev
          loop
        Left Nothing ->
          liftIO $ cancel worker
        Left (Just Nothing) ->
          liftIO $ wait worker
        Left (Just (Just err)) -> do
          liftIO $ cancel worker
          throwError err

{- | Lift a resource-safe 'ChatStream' into any 'MonadIO' and 'MonadError LangchainError'
'EventStream'. Events are forwarded incrementally through a bounded STM queue,
and cancelling or throwing an exception cancels the upstream stream worker promptly.
-}
liftChatStream ::
  (MonadIO m, MonadError LangchainError m) =>
  ChatStream ->
  EventStream m
liftChatStream chatStream = do
  st <- liftIO $ startChatWorker chatStream
  consumeChatWorker Nothing st

{- | Lift a resource-safe 'ChatStream' into any 'MonadResource' and
'MonadError LangchainError' 'EventStream'. Uses 'bracketP' so stopping
consumption early (e.g. @src .| CL.take 1@) cancels the upstream producer
immediately.
-}
liftChatStreamResource ::
  (MonadResource m, MonadError LangchainError m) =>
  ChatStream ->
  EventStream m
liftChatStreamResource chatStream =
  bracketP (startChatWorker chatStream) (\(_, _, w) -> cancel w) (consumeChatWorker Nothing)

{- | Lift a resource-safe 'ChatStream' with cooperative cancellation via
'CancelToken'. When cancelled, the upstream stream worker is terminated promptly
and the Conduit exits cleanly so downstream accumulators preserve partial text.
-}
liftChatStreamWithCancel ::
  (MonadIO m, MonadError LangchainError m) =>
  CancelToken ->
  ChatStream ->
  EventStream m
liftChatStreamWithCancel tok chatStream = do
  st <- liftIO $ startChatWorker chatStream
  consumeChatWorker (Just tok) st

{- | Pass every 'StreamEvent' downstream unchanged while accumulating the final 'Message'
from the last 'LLMEnd' event (or synthesizing one from accumulated 'LLMChunk's if
no 'LLMEnd' event was emitted).
-}
tapStreamMessage :: Monad m => ConduitT StreamEvent StreamEvent m Message
tapStreamMessage = loop [] [] Nothing
  where
    loop accText accTools mbFinal = do
      mEv <- await
      case mEv of
        Nothing ->
          pure $ case mbFinal of
            Just msg ->
              case (messageToolCalls msg, reverse accTools) of
                (Nothing, ts@(_ : _)) -> msg {messageToolCalls = Just ts}
                _ -> msg
            Nothing ->
              let fullText = T.concat (reverse accText)
                  ts = reverse accTools
               in (assistantMessage fullText)
                    { messageToolCalls = if null ts then Nothing else Just ts
                    }
        Just ev -> do
          yield ev
          case ev of
            LLMStart {} -> loop [] [] Nothing
            LLMChunk {..} ->
              let nextText = if T.null chunkText then accText else chunkText : accText
                  nextTools = maybe accTools (: accTools) toolCallDelta
               in loop nextText nextTools mbFinal
            LLMEnd {..} ->
              loop accText accTools (Just finalMessage)
            _ -> loop accText accTools mbFinal

{- | Consume a 'StreamEvent' pipeline, invoking a handler for every event, and
return the final 'Message' from the last 'LLMEnd' event (or synthesize one from
accumulated 'LLMChunk's if no 'LLMEnd' event was emitted).
-}
consumeStreamWith ::
  Monad m =>
  (StreamEvent -> m ()) ->
  ConduitT StreamEvent Void m Message
consumeStreamWith onEvent = tapStreamMessage `fuseUpstream` CL.mapM_ onEvent

-- | Run an 'EventStream' with an event handler and return the resulting 'Message'.
runStreamWith ::
  Monad m =>
  (StreamEvent -> m ()) ->
  EventStream m ->
  m Message
runStreamWith onEvent src = runConduit (src .| consumeStreamWith onEvent)

-- | Collect all emitted events from a stream into a list.
collectEvents :: Monad m => EventStream m -> m [StreamEvent]
collectEvents streamSrc = runConduit (streamSrc .| CL.consume)

-- | Debug helper: print all stream events to stdout.
printEvents :: EventStream (ExceptT LangchainError (ResourceT IO)) -> IO (Either LangchainError ())
printEvents streamSrc = runResourceT $ runExceptT $ runConduit (streamSrc .| CL.mapM_ (liftIO . print))

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
Module      : Langchain.Core.Stream
Description : Standardized StreamEvent protocol and utilities
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Structured streaming event protocol for tracking LLM, tool, chain, and graph lifecycle events.
-}
module Langchain.Core.Stream
  ( TokenUsage (..)
  , StreamEvent (..)
  , StreamM
  , EventStream
  , ChatStream
  , StreamCallback
  , StreamSource
  , callbackSource
  , collectEvents
  , printEvents
  ) where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
  ( atomically
  , newEmptyTMVarIO
  , newTBQueueIO
  , orElse
  , putTMVar
  , readTBQueue
  , readTMVar
  , writeTBQueue
  )
import Control.Exception (finally)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Conduit (ConduitT, bracketP, runConduit, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import GHC.Generics (Generic)
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model.Types (Message, ToolCall)

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

-- | Collect all emitted events from a stream into a list.
collectEvents :: Monad m => EventStream m -> m [StreamEvent]
collectEvents streamSrc = runConduit (streamSrc .| CL.consume)

-- | Debug helper: print all stream events to stdout.
printEvents :: EventStream (ExceptT LangchainError (ResourceT IO)) -> IO (Either LangchainError ())
printEvents streamSrc = runResourceT $ runExceptT $ runConduit (streamSrc .| CL.mapM_ (liftIO . print))

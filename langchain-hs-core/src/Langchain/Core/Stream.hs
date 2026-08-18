{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , EventStream
  , collectEvents
  , printEvents
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Conduit (ConduitT, runConduit, (.|))
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

-- | Collect all emitted events from a stream into a list.
collectEvents :: Monad m => EventStream m -> m [StreamEvent]
collectEvents streamSrc = runConduit (streamSrc .| CL.consume)

-- | Debug helper: print all stream events to stdout.
printEvents :: EventStream IO -> IO ()
printEvents streamSrc = runConduit (streamSrc .| CL.mapM_ print)

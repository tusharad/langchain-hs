{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Tool
Description : Effect-polymorphic Tool specification and parameter validation
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides effect-polymorphic 'Tool m' representation, schema generation, and argument parsing.
-}
module Langchain.Tool
  ( Tool (..)
  , createTool
  , toolToValue
  , StreamingTool (..)
  , createStreamingTool
  , fromTool
  , streamToolCall
  , streamStreamingToolCall
  ) where

import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Conduit (ConduitT, await, fuseUpstream, mapOutput, runConduit, yield)
import Data.Text (Text)

import Langchain.Error (LangchainError)
import Langchain.Stream (StreamEvent (ToolChunk, ToolEnd, ToolErrorEvent, ToolStart))

-- | Effect-polymorphic Tool abstraction
data Tool m = Tool
  { toolName :: Text
  -- ^ Unique identifier for the tool
  , toolDescription :: Text
  -- ^ Description explaining when and how to use the tool
  , toolSchema :: Value
  -- ^ JSON Schema describing expected parameters
  , toolExecute :: Value -> m (Either LangchainError Text)
  -- ^ Monadic execution function accepting JSON arguments and returning text output
  }

instance Show (Tool m) where
  show t = "Tool { toolName = " ++ show (toolName t) ++ " }"

-- | Helper to create a Tool from a name, description, schema, and execution function
createTool ::
  Text ->
  Text ->
  Value ->
  (Value -> m (Either LangchainError Text)) ->
  Tool m
createTool name desc schema execFn =
  Tool
    { toolName = name
    , toolDescription = desc
    , toolSchema = schema
    , toolExecute = execFn
    }

-- | Convert Tool definition to OpenAI/Ollama compatible function definition JSON object
toolToValue :: Tool m -> Value
toolToValue Tool {..} =
  object
    [ "type" .= ("function" :: Text)
    , "function"
        .= object
          [ "name" .= toolName
          , "description" .= toolDescription
          , "parameters" .= toolSchema
          ]
    ]

-- | Effect-polymorphic streaming Tool abstraction supporting incremental 'Text' output chunks.
data StreamingTool m = StreamingTool
  { streamingToolDef :: Tool m
  -- ^ Underlying tool specification and synchronous fallback executor
  , streamingToolExecute :: Value -> ConduitT () Text m (Either LangchainError Text)
  -- ^ Monadic streaming execution yielding output chunks and returning final text output
  }

instance Show (StreamingTool m) where
  show st = "StreamingTool { toolName = " ++ show (toolName (streamingToolDef st)) ++ " }"

-- | Construct a 'StreamingTool' from a name, description, schema, and streaming executor.
createStreamingTool ::
  Monad m =>
  Text ->
  Text ->
  Value ->
  (Value -> ConduitT () Text m (Either LangchainError Text)) ->
  StreamingTool m
createStreamingTool name desc schema streamExec =
  StreamingTool
    { streamingToolDef =
        createTool name desc schema $ \args ->
          runConduit (streamExec args `fuseUpstream` drainSink)
    , streamingToolExecute = streamExec
    }
  where
    drainSink = do
      mItem <- await
      case mItem of
        Nothing -> pure ()
        Just _ -> drainSink

-- | Lift a standard 'Tool' into a 'StreamingTool' that emits its full output as a single chunk.
fromTool :: Monad m => Tool m -> StreamingTool m
fromTool t =
  StreamingTool
    { streamingToolDef = t
    , streamingToolExecute = \args -> do
        res <- lift $ toolExecute t args
        case res of
          Right out -> yield out >> pure (Right out)
          Left err -> pure (Left err)
    }

{- | Execute a 'Tool' within a 'StreamEvent' pipeline, emitting 'ToolStart'
followed by 'ToolEnd' or 'ToolErrorEvent'.
-}
streamToolCall ::
  Monad m =>
  Text ->
  Tool m ->
  Value ->
  ConduitT () StreamEvent m (Either LangchainError Text)
streamToolCall rId tool args = do
  let tName = toolName tool
  yield $ ToolStart rId tName args
  eRes <- lift $ toolExecute tool args
  case eRes of
    Left err -> do
      yield $ ToolErrorEvent rId tName err
      pure (Left err)
    Right out -> do
      yield $ ToolEnd rId tName (toJSON out)
      pure (Right out)

{- | Execute a 'StreamingTool' within a 'StreamEvent' pipeline, emitting
'ToolStart', 'ToolChunk' for each streamed text chunk, and 'ToolEnd' or 'ToolErrorEvent'.
-}
streamStreamingToolCall ::
  Monad m =>
  Text ->
  StreamingTool m ->
  Value ->
  ConduitT () StreamEvent m (Either LangchainError Text)
streamStreamingToolCall rId sTool args = do
  let tName = toolName (streamingToolDef sTool)
  yield $ ToolStart rId tName args
  eRes <- mapOutput (ToolChunk rId tName) (streamingToolExecute sTool args)
  case eRes of
    Left err -> do
      yield $ ToolErrorEvent rId tName err
      pure (Left err)
    Right out -> do
      yield $ ToolEnd rId tName (toJSON out)
      pure (Right out)

# Technical Design Document (TDD)

## Project: `langchain-hs` v2 — Architectural Blueprint

- **Status**: Technical Specification
- **Author**: Tushar Adhatrao (Maintainer)
- **TDD Version**: 2.0.0
- **Target GHC**: 9.4, 9.6, 9.8, 9.10, 9.12

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    langchain-hs (meta)                           │
├──────────────┬──────────────────┬─────────────┬─────────────────┤
│  core        │    providers     │    graph    │      rag         │
│  (no HTTP)   │  (LLM backends)  │  (agents)   │  (RAG pipeline) │
└──────┬───────┴────────┬─────────┴──────┬──────┴────────┬────────┘
       │                │                │               │
       └────────────────┴────────────────┴───────────────┘
                            depends on core
```

The dependency direction is strictly: `core <- providers`, `core <- graph`, `core <- rag`. The `core` package has **zero HTTP dependencies**. All network I/O lives in leaf packages.

---

## 2. Core Module Design (`langchain-hs-core`)

### 2.1 Effect-Polymorphic `ChatModel` Typeclass

**Problem with v1**: All methods return `IO (LangchainResult a)`, making testing impossible without live API calls.

**v2 Solution**: Parameterize over `m` with `MonadIO m, MonadError LangchainError m`.

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Langchain.Core.Model where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Conduit (ConduitT)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Langchain.Core.Error (LangchainError)
import Langchain.Core.Stream (StreamEvent)

-- | Unified typeclass for all chat models.
-- Parameterized over monad 'm' — no hardcoded IO.
class ChatModel model where
  type ModelConfig model :: *

  -- | Single synchronous invocation
  invoke
    :: (MonadIO m, MonadError LangchainError m)
    => model
    -> [Message]
    -> Maybe (ModelConfig model)
    -> m Message

  -- | Batch invocations (default: sequential)
  batch
    :: (MonadIO m, MonadError LangchainError m)
    => model
    -> [[Message]]
    -> Maybe (ModelConfig model)
    -> m [Message]
  batch model msgs cfg = mapM (\m -> invoke model m cfg) msgs

  -- | Streaming invocation via Conduit
  stream
    :: (MonadIO m, MonadError LangchainError m)
    => model
    -> [Message]
    -> Maybe (ModelConfig model)
    -> ConduitT () StreamEvent m ()
```

### 2.2 Multi-Modal Message Model

**Problem with v1**: `Message.content :: Text` — no multimodal support. `messageImages :: Maybe [Text]` is a bolted-on hack.

**v2 Solution**: Proper `ContentBlock` sum type.

```haskell
module Langchain.Core.Model where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | A single content block within a message.
-- Replaces the flat `content :: Text` field.
data ContentBlock
  = TextBlock  { blockText :: Text }
  | ImageBlock { blockMimeType :: Text, blockBase64 :: Text }
  | AudioBlock { blockMimeType :: Text, blockBase64 :: Text }
  | DataBlock  { blockBytes :: ByteString }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Message roles (complete set for all providers)
data Role
  = System
  | User
  | Assistant
  | Tool
  | Developer
  | Function
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Structured tool call from LLM response
data ToolCall = ToolCall
  { toolCallId       :: Text
  , toolCallType     :: Text    -- ^ always "function" for current providers
  , toolCallName     :: Text
  , toolCallArguments :: Value  -- ^ JSON Value, not Map Text Value
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A single message in a conversation
data Message = Message
  { messageRole      :: Role
  , messageContents  :: NonEmpty ContentBlock  -- ^ REPLACES content :: Text
  , messageName      :: Maybe Text
  , messageToolCalls :: Maybe [ToolCall]
  , messageToolId    :: Maybe Text  -- ^ For Tool role: associated call ID
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Convenience constructors
textMessage :: Role -> Text -> Message
textMessage r t = Message r (pure (TextBlock t)) Nothing Nothing Nothing

userMessage :: Text -> Message
userMessage = textMessage User

systemMessage :: Text -> Message
systemMessage = textMessage System

assistantMessage :: Text -> Message
assistantMessage = textMessage Assistant
```

### 2.3 Pure Pipeline GADT — `RunnableTree`

**Problem with v1**: `(|>>)` is `chain :: r1 -> r2 -> Input -> IO (Either Error Output)` — it executes immediately. Pipelines cannot be inspected, serialized, or reused.

**v2 Solution**: Pure GADT that builds an AST. `interpret` is the only execution point.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Langchain.Core.Runnable where

import Control.Monad.Except (MonadError)
import Control.Concurrent.Async (concurrently)
import Langchain.Core.Error (LangchainError)

-- | Pure GADT representing a composable pipeline.
-- 'i' = input type, 'o' = output type, 'm' = monad context.
-- Building this tree performs NO side effects.
data RunnableTree m i o where
  -- | Identity: passes input through unchanged
  Id      :: RunnableTree m a a

  -- | Lift a pure Runnable into the tree
  Prim    :: (Runnable r m, RunnableInput r ~ i, RunnableOutput r ~ o)
          => r -> RunnableTree m i o

  -- | Lift a monadic function (lambda) into the tree
  Lambda  :: (i -> m (Either LangchainError o)) -> RunnableTree m i o

  -- | Sequential composition: output of first is input to second
  Seq     :: RunnableTree m i mid -> RunnableTree m mid o -> RunnableTree m i o

  -- | Parallel composition: both branches receive same input
  Par     :: RunnableTree m i o1 -> RunnableTree m i o2 -> RunnableTree m i (o1, o2)

  -- | Conditional branching on input value
  Branch  :: (i -> m Bool)
          -> RunnableTree m i o   -- ^ branch if True
          -> RunnableTree m i o   -- ^ branch if False
          -> RunnableTree m i o

  -- | Fallback: if first fails, try second
  Fallback :: RunnableTree m i o -> RunnableTree m i o -> RunnableTree m i o

-- | Sequential composition operator — PURE, builds AST node
(|>>) :: RunnableTree m a b -> RunnableTree m b c -> RunnableTree m a c
(|>>) = Seq
infixl 1 |>>

-- | Parallel composition operator — PURE, builds AST node
(&>&) :: RunnableTree m a b -> RunnableTree m a c -> RunnableTree m a (b, c)
(&>&) = Par
infixl 2 &>&

-- | The ONLY function that executes the pipeline tree
interpret
  :: (MonadIO m, MonadError LangchainError m)
  => RunnableTree m i o
  -> i
  -> m o
interpret Id input = pure input
interpret (Prim r) input = invoke r input >>= liftEither
interpret (Lambda f) input = f input >>= liftEither
interpret (Seq t1 t2) input = interpret t1 input >>= interpret t2
interpret (Par t1 t2) input = do
  -- Parallel execution using async
  (r1, r2) <- liftIO $ concurrently
    (runExceptT $ interpret t1 input)
    (runExceptT $ interpret t2 input)
  o1 <- liftEither r1
  o2 <- liftEither r2
  pure (o1, o2)
interpret (Branch cond tTrue tFalse) input = do
  b <- cond input
  if b then interpret tTrue input else interpret tFalse input
interpret (Fallback t1 t2) input =
  catchError (interpret t1 input) (\_ -> interpret t2 input)

-- | Core Runnable typeclass (for Prim wrapping)
class Runnable r m where
  type RunnableInput  r :: *
  type RunnableOutput r :: *
  invoke :: r -> RunnableInput r -> m (Either LangchainError (RunnableOutput r))
```

### 2.4 Streaming Event Protocol

**Problem with v1**: Only 3 events (`LLMStart`, `LLMEnd`, `LLMError String`). No tool events, no chain events, no token usage, no run IDs.

**v2 Solution**: Full `StreamEvent` ADT backed by `Conduit`.

```haskell
module Langchain.Core.Stream where

import Data.Aeson (Value)
import Data.Conduit (ConduitT)
import Data.Text (Text)
import Data.Time (UTCTime)

data TokenUsage = TokenUsage
  { promptTokens     :: Int
  , completionTokens :: Int
  , totalTokens      :: Int
  } deriving (Eq, Show)

-- | All possible streaming events emitted by the framework.
-- Every event carries a 'runId' for correlation across a pipeline.
data StreamEvent
  -- LLM lifecycle
  = LLMStart
      { runId :: Text, modelName :: Text, inputMessages :: [Message] }
  | LLMChunk
      { runId :: Text, chunkText :: Text, toolCallDelta :: Maybe ToolCall }
  | LLMEnd
      { runId :: Text, finalMessage :: Message, tokenUsage :: Maybe TokenUsage }

  -- Tool lifecycle
  | ToolStart
      { runId :: Text, toolName :: Text, toolInput :: Value }
  | ToolEnd
      { runId :: Text, toolName :: Text, toolOutput :: Value }
  | ToolError
      { runId :: Text, toolName :: Text, toolError :: LangchainError }

  -- Chain lifecycle
  | ChainStart
      { runId :: Text, chainName :: Text, chainInput :: Value }
  | ChainEnd
      { runId :: Text, chainName :: Text, chainOutput :: Value }

  -- Graph node lifecycle
  | NodeStart
      { runId :: Text, nodeId :: Text, nodeState :: Value }
  | NodeEnd
      { runId :: Text, nodeId :: Text, nodeState :: Value }

  deriving (Eq, Show)

-- | Canonical stream type for the entire framework
type EventStream m = ConduitT () StreamEvent m ()
```

### 2.5 Type-Safe Tool System with Auto Schema Derivation

**Problem with v1**: `Tool` typeclass has no JSON Schema derivation. `ToolAcceptingToolCall` wrapper requiring `Input t ~ ToolCall` is a workaround that breaks type safety.

**v2 Solution**: `IsTool` with `toolSchema :: Value`, plus `DynamicTool` for heterogeneous dispatch.

```haskell
{-# LANGUAGE DefaultSignatures #-}

module Langchain.Core.Tool where

import Data.Aeson (ToJSON, FromJSON, Value, toJSON)
import Data.Aeson.Schema (schemaFor)  -- or Data.OpenApi / json-schema-typed
import Data.Text (Text)
import GHC.Generics (Generic)
import Langchain.Core.Error (LangchainError)

-- | Statically typed tool typeclass.
-- 'toolSchema' defaults to deriving from Generic + ToJSON.
class ( FromJSON (ToolInput t)
      , ToJSON   (ToolOutput t)
      ) => IsTool t where
  type ToolInput  t :: *
  type ToolOutput t :: *

  toolName        :: t -> Text
  toolDescription :: t -> Text

  -- | JSON Schema for ToolInput — MUST match what LLM receives
  toolSchema :: t -> Value
  default toolSchema :: (Generic (ToolInput t), ...) => t -> Value
  toolSchema _ = deriveJsonSchema @(ToolInput t)

  -- | Execute the tool with typed input
  executeTool
    :: (MonadIO m, MonadError LangchainError m)
    => t -> ToolInput t -> m (ToolOutput t)

-- | Runtime-erased tool wrapper for heterogeneous tool lists
data DynamicTool m = DynamicTool
  { dynName        :: Text
  , dynDescription :: Text
  , dynSchema      :: Value               -- ^ JSON Schema of input
  , dynExecute     :: Value -> m (Either LangchainError Value)
  }

-- | Smart constructor: wrap any IsTool instance into DynamicTool
mkTool :: (IsTool t, MonadIO m, MonadError LangchainError m, ...)
       => t -> DynamicTool m
mkTool t = DynamicTool
  { dynName        = toolName t
  , dynDescription = toolDescription t
  , dynSchema      = toolSchema t
  , dynExecute     = \v -> case fromJSON v of
      Error e   -> throwError (parsingError (pack e) ...)
      Success a -> toJSON <$> executeTool t a
  }

-- | Named registry of tools
newtype ToolRegistry m = ToolRegistry (Map Text (DynamicTool m))

lookupTool :: Text -> ToolRegistry m -> Maybe (DynamicTool m)
registerTool :: DynamicTool m -> ToolRegistry m -> ToolRegistry m
```

### 2.6 STM-Safe Memory

**Problem with v1**: `WindowBufferMemory` state is a plain record. Concurrent updates are unsafe. Memory typeclass is not monadic.

**v2 Solution**: All memory backed by `TVar`. `BaseMemory` parameterized over `m`.

```haskell
module Langchain.Core.Memory where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar')
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError)

-- | Typeclass for memory backends
class BaseMemory mem m where
  getMessages   :: mem -> m [Message]
  addMessage    :: mem -> Message -> m mem
  addUserMsg    :: mem -> Text -> m mem
  addAIMsg      :: mem -> Text -> m mem
  clear         :: mem -> m mem

-- | Thread-safe window buffer backed by TVar
data WindowBufferMemory = WindowBufferMemory
  { maxWindow  :: Int
  , msgBuffer  :: TVar [Message]  -- ^ CHANGED: was a plain [Message] field
  , systemMsg  :: Maybe Message
  }

newWindowBufferMemory :: MonadIO m => Int -> Maybe Text -> m WindowBufferMemory
newWindowBufferMemory n sys = liftIO $ do
  let sysMsg = systemMessage <$> sys
  buf <- newTVarIO (maybe [] pure sysMsg)
  pure $ WindowBufferMemory n buf sysMsg

instance (MonadIO m, MonadError LangchainError m) => BaseMemory WindowBufferMemory m where
  getMessages mem = liftIO $ readTVarIO (msgBuffer mem)

  addMessage mem msg = liftIO $ do
    modifyTVar' (msgBuffer mem) $ \msgs ->
      let withNew = msgs ++ [msg]
          trimmed = trimWindow (maxWindow mem) withNew
      in trimmed
    pure mem  -- mem is a handle, mutations in TVar
```

### 2.7 Error System — Fix Silent Parameter Dropping

**Problem with v1**: All error constructors ignore their `_model` and `_operation` parameters. This is a critical documentation and debugging regression.

**v2 Fix**: Store all context parameters.

```haskell
-- v1 (broken — ignores arguments):
llmError msg _model _operation =
  LangchainError { errorMessage = msg, errorContext = Nothing, ... }

-- v2 (correct — uses arguments):
llmError :: Text -> Maybe Text -> Maybe Text -> LangchainError
llmError msg model operation =
  LangchainError
    { errorMessage  = msg
    , errorSeverity = High
    , errorCategory = LLMError
    , errorContext  = buildContext model operation Nothing
    , errorCause    = Nothing
    , errorCode     = Nothing
    }
  where
    buildContext Nothing  Nothing  _ = Nothing
    buildContext model op input = Just ErrorContext
      { contextComponent  = model
      , contextOperation  = op
      , contextInput      = input
      , contextMetadata   = []
      , contextTimestamp  = unsafePerformIO getCurrentTime  -- only for convenience ctor
      }
```

---

## 3. Provider Architecture (`langchain-hs-providers`)

### 3.1 Provider Implementation Pattern

Each provider follows an identical pattern:

```haskell
module Langchain.Provider.OpenAI where

-- | Provider configuration
data OpenAIConfig = OpenAIConfig
  { apiKey      :: Text
  , baseUrl     :: Text         -- ^ for OpenAI-compatible endpoints
  , httpManager :: Manager      -- ^ reused HTTP connection manager
  , defaultModel :: Text
  }

-- | Provider handle (not configuration)
newtype OpenAI = OpenAI OpenAIConfig

-- | Smart constructor with connection manager reuse
newOpenAI :: MonadIO m => Text -> m OpenAI
newOpenAI key = liftIO $ do
  mgr <- newManager tlsManagerSettings
  pure $ OpenAI $ OpenAIConfig key "https://api.openai.com/v1" mgr "gpt-4o"

instance ChatModel OpenAI where
  type ModelConfig OpenAI = OpenAICallConfig

  invoke (OpenAI cfg) msgs callCfg = do
    -- Convert langchain-hs Messages -> OpenAI wire format
    let req = buildOpenAIRequest cfg msgs callCfg
    resp <- liftIO $ httpLbs req (httpManager cfg)
    parseOpenAIResponse resp

  stream (OpenAI cfg) msgs callCfg = do
    let req = buildOpenAIStreamRequest cfg msgs callCfg
    streamOpenAIResponse req
```

### 3.2 `Conduit`-Based Streaming Adapter

```haskell
-- Convert provider-specific SSE stream to EventStream
streamOpenAIResponse
  :: (MonadIO m, MonadError LangchainError m, MonadResource m)
  => Request -> Manager -> EventStream m
streamOpenAIResponse req mgr = do
  runId <- liftIO newRunId
  let msgs = [...]  -- captured input for LLMStart
  yield $ LLMStart runId "gpt-4o" msgs
  bracketP
    (openStream req mgr)
    closeStream
    (parseSSEChunks runId)
  where
    parseSSEChunks runId handle = do
      chunk <- liftIO $ readChunk handle
      unless (isDone chunk) $ do
        yield $ LLMChunk runId (extractText chunk) (extractToolDelta chunk)
        parseSSEChunks runId handle
      yield $ LLMEnd runId (assembleMessage ...) (extractUsage ...)
```

---

## 4. State-Graph Engine (`langchain-hs-graph`)

### 4.1 Core Graph Types

```haskell
module Langchain.Graph.StateGraph where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Langchain.Core.Error (LangchainError)

newtype NodeId = NodeId Text deriving (Eq, Ord, Show)

-- | Reserved node identifiers
startNode :: NodeId
startNode = NodeId "__start__"

endNode :: NodeId
endNode = NodeId "__end__"

-- | Node: a function that transforms state
data Node s m = Node
  { nodeId  :: NodeId
  , runNode :: s -> m (Either LangchainError s)
  }

-- | Edge: static or conditional routing
data Edge s m
  = StaticEdge NodeId
  | ConditionalEdge (s -> m (Either LangchainError NodeId))

-- | State reducer: pure merge of partial updates
type StateReducer s = s -> s -> s

-- | Full graph definition (uncompiled)
data StateGraph s m = StateGraph
  { graphNodes      :: Map NodeId (Node s m)
  , graphEdges      :: Map NodeId [Edge s m]
  , graphEntryPoint :: NodeId
  , graphExitPoint  :: NodeId
  , stateReducer    :: StateReducer s
  }
```

### 4.2 Graph Compilation and Validation

```haskell
-- | Compiled and validated graph. Construction is the only place
-- where structural errors can occur.
data CompiledGraph s m = CompiledGraph
  { compiledNodes :: Map NodeId (Node s m)
  , compiledEdges :: Map NodeId [Edge s m]
  , entryPoint    :: NodeId
  , exitPoint     :: NodeId
  , reducer       :: StateReducer s
  }

data GraphError
  = UnreachableNode NodeId
  | DanglingEdge NodeId NodeId
  | MissingEntryPoint
  | CyclicWithoutExit
  deriving (Show)

compileGraph :: StateGraph s m -> Either GraphError (CompiledGraph s m)
compileGraph sg = do
  validateAllNodesReachable sg
  validateNoDanglingEdges sg
  validateEntryExitExist sg
  pure $ CompiledGraph
    { compiledNodes = graphNodes sg
    , compiledEdges = graphEdges sg
    , entryPoint    = graphEntryPoint sg
    , exitPoint     = graphExitPoint sg
    , reducer       = stateReducer sg
    }

-- | Execute compiled graph from initial state
runGraph
  :: (MonadIO m, MonadError LangchainError m)
  => CompiledGraph s m
  -> s
  -> m s
runGraph g initialState = go (entryPoint g) initialState
  where
    go nodeId_ state_
      | nodeId_ == exitPoint g = pure state_
      | otherwise = do
          node <- lookupOrError nodeId_ (compiledNodes g)
          newState <- runNode node state_ >>= liftEither
          nextId <- routeEdge (compiledEdges g Map.! nodeId_) newState
          go nextId newState
```

### 4.3 Checkpointer Interface

```haskell
module Langchain.Graph.Checkpointer where

import Data.Aeson (Value, ToJSON, FromJSON)

class Checkpointer cp m where
  saveCheckpoint :: cp -> Text -> Text -> Value -> m ()
  -- ^ (checkpointer, threadId, runId, state)
  loadCheckpoint :: cp -> Text -> m (Maybe Value)
  -- ^ (checkpointer, threadId) -> latest state
  listCheckpoints :: cp -> Text -> m [(Text, Value)]
  -- ^ (checkpointer, threadId) -> [(runId, state)]

-- | In-memory checkpointer using STM
data MemoryCheckpointer = MemoryCheckpointer
  { store :: TVar (Map Text [(Text, Value)]) }

newMemoryCheckpointer :: MonadIO m => m MemoryCheckpointer
newMemoryCheckpointer = liftIO $ MemoryCheckpointer <$> newTVarIO mempty

-- | SQLite-backed persistent checkpointer
data SQLiteCheckpointer = SQLiteCheckpointer
  { dbPath :: FilePath }
```

### 4.4 Human-in-the-Loop (HITL)

```haskell
module Langchain.Graph.HITL where

-- | Interrupt signal — suspend execution at this point
data HITLSignal = HITLInterrupt | HITLResume

-- | Special node that saves checkpoint and halts execution
hitlNode
  :: (ToJSON s, Checkpointer cp m)
  => cp -> Text -> NodeId -> Node s m
hitlNode cp threadId nid = Node nid $ \state -> do
  saveCheckpoint cp threadId (unNodeId nid) (toJSON state)
  throwError $ hiTLInterruptError nid

-- | Resume graph execution from a saved checkpoint
resumeGraph
  :: ( FromJSON s, ToJSON s
     , Checkpointer cp m
     , MonadIO m, MonadError LangchainError m
     )
  => CompiledGraph s m
  -> cp
  -> Text        -- ^ threadId
  -> m s
resumeGraph graph cp threadId = do
  mState <- loadCheckpoint cp threadId
  case mState of
    Nothing -> throwError $ configurationError "No checkpoint found" ...
    Just v  -> case fromJSON v of
      Error e   -> throwError $ parsingError (pack e) ...
      Success s -> runGraph graph s
```

---

## 5. RAG Pipeline Architecture (`langchain-hs-rag`)

### 5.1 Document Loader Typeclass

```haskell
module Langchain.RAG.Loader where

-- | Document with content and structured metadata
data Document = Document
  { pageContent :: Text
  , metadata    :: Map Text Value
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

class DocumentLoader loader m where
  loadDocuments :: loader -> m (Either LangchainError [Document])

-- | Conduit-based streaming loader for large corpora
class StreamingLoader loader m where
  loadDocumentsStream :: loader -> ConduitT () Document m ()
```

### 5.2 `RecursiveCharacterTextSplitter`

The key missing splitter from v1 — splits by a priority list of separators.

```haskell
data RecursiveCharacterTextSplitter = RecursiveCharacterTextSplitter
  { chunkSize    :: Int
  , chunkOverlap :: Int
  , separators   :: [Text]  -- ^ Priority list, e.g. ["\n\n", "\n", " ", ""]
  , keepSeparator :: Bool
  }

defaultRecursiveSplitter :: RecursiveCharacterTextSplitter
defaultRecursiveSplitter = RecursiveCharacterTextSplitter
  { chunkSize    = 4000
  , chunkOverlap = 200
  , separators   = ["\n\n", "\n", " ", ""]
  , keepSeparator = True
  }

-- Language-specific presets
haskellSplitter :: RecursiveCharacterTextSplitter
haskellSplitter = defaultRecursiveSplitter
  { separators = ["\n\n\n", "\n\n", "\nmodule", "\nwhere", "\nlet", "\nin", " ", ""] }
```

### 5.3 Enhanced Vector Store with MMR

```haskell
class VectorStore vs m where
  addDocuments :: vs -> [(Document, [Float])] -> m (Either LangchainError vs)
  delete       :: vs -> [Int64] -> m (Either LangchainError vs)

  -- | Standard similarity search
  similaritySearch :: vs -> [Float] -> Int -> m (Either LangchainError [Document])

  -- | Max Marginal Relevance — balances relevance with diversity
  maxMarginalRelevanceSearch
    :: vs
    -> [Float]  -- ^ query vector
    -> Int      -- ^ k results
    -> Float    -- ^ lambda: 0=diversity, 1=relevance
    -> m (Either LangchainError [Document])
```

---

## 6. Observability Design

```haskell
module Langchain.Core.Telemetry where

import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)

data Span = Span
  { spanId         :: Text
  , parentSpanId   :: Maybe Text
  , traceId        :: Text
  , spanName       :: Text
  , startTime      :: UTCTime
  , endTime        :: Maybe UTCTime
  , spanAttributes :: Map Text Value
  , spanStatus     :: SpanStatus
  } deriving (Eq, Show)

data SpanStatus = Ok | Error Text | Unset deriving (Eq, Show)

-- | Effect typeclass for tracing — zero-cost if NoOpTracer
class Monad m => MonadTracer m where
  withSpan       :: Text -> m a -> m a
  addAttribute   :: Text -> Value -> m ()
  recordException :: LangchainError -> m ()
  currentSpan    :: m (Maybe Span)

-- | Exported tracer implementations
data NoOpTracer    -- zero overhead, default
data OTELTracer    -- OpenTelemetry OTLP export
data LangSmithTracer  -- LangSmith run tree export
```

---

## 7. Migration Map: v1 → v2

| v1 Location | v1 Problem | v2 Location | v2 Solution |
|:---|:---|:---|:---|
| `src/Langchain/LLM/Core.hs` | Hardcoded `IO`; `content :: Text` | `Langchain.Core.Model` | `ChatModel m`, `ContentBlock`, `NonEmpty ContentBlock` |
| `src/Langchain/Runnable/Core.hs` | Concrete `IO`; no AST | `Langchain.Core.Runnable` | `RunnableTree m i o` GADT; `interpret` executor |
| `src/Langchain/Runnable/Chain.hs` | `(|>>)` executes immediately | `Langchain.Core.Runnable` | `(|>>)` builds `Seq` AST node; pure |
| `src/Langchain/Tool/Core.hs` | No JSON Schema; `Input t ~ ToolCall` workaround | `Langchain.Core.Tool` | `IsTool`, `toolSchema`, `DynamicTool`, `ToolRegistry` |
| `src/Langchain/Agent/Core.hs` | Imperative loop, no graph | `Langchain.Graph.StateGraph` | Typed `StateGraph`, `Node`, `Edge`, `compileGraph` |
| `src/Langchain/Agent/Executor.hs` | Recursive `IO` with counter | `Langchain.Graph.StateGraph` | `runGraph`, `CompiledGraph`, checkpointing |
| `src/Langchain/Callback.hs` | 3 events, `String` error | `Langchain.Core.Stream` | Full `StreamEvent` ADT, `EventStream m` via `Conduit` |
| `src/Langchain/Memory/Core.hs` | Plain record, not thread-safe | `Langchain.Core.Memory` | `TVar`-backed `WindowBufferMemory`, STM-safe |
| `src/Langchain/Error.hs` | `_param` silently dropped | `Langchain.Core.Error` | All params stored in `ErrorContext` |
| `src/Langchain/PromptTemplate.hs` | `FewShotPromptTemplate` not `Runnable` | `Langchain.Core.Prompt` | Complete `Runnable` instance for `FewShotPromptTemplate` |
| `src/Langchain/TextSplitter/Character.hs` | Only character splitter | `Langchain.RAG.Splitter` | `RecursiveCharacter`, `Token`, `MarkdownHeader` splitters |
| `src/Langchain/VectorStore/InMemory.hs` | Basic cosine, no MMR | `Langchain.RAG.VectorStore` | HNSW + MMR, plus `PgvectorStore`, `QdrantStore` |
| MISSING | No observability | `Langchain.Core.Telemetry` | `MonadTracer`, OTEL, LangSmith |
| MISSING | No graph agents | `Langchain.Graph.*` | Full LangGraph-hs engine |
| MISSING | No multimodal | `Langchain.Core.Model` | `ContentBlock` sum type |

---

## 8. Package Split Strategy

### Phase 1: Extract `langchain-hs-core`

Split the current monolithic package. Move all type definitions and typeclasses — zero HTTP dependencies.

**`langchain-hs-core` dependencies:**
```
aeson, base, conduit, containers, mtl, stm, text, time, transformers, unliftio
```

### Phase 2: `langchain-hs-providers`

Each provider as a sub-module. Depends on `langchain-hs-core` + HTTP.

**`langchain-hs-providers` dependencies (per-provider):**
```
http-conduit, http-types, langchain-hs-core, ollama-haskell, openai, aeson
```

### Phase 3: `langchain-hs-graph`

The state-graph engine. No HTTP. Only STM and SQLite for checkpointers.

**`langchain-hs-graph` dependencies:**
```
langchain-hs-core, stm, sqlite-simple, containers, aeson
```

### Phase 4: `langchain-hs-rag`

Document processing pipeline. HTTP for remote loaders.

**`langchain-hs-rag` dependencies:**
```
langchain-hs-core, conduit, pdf-toolbox-document, tagsoup, aeson
```

---

## 9. Testing Strategy

### Unit Tests (no network required)

```haskell
-- Test pipeline composition is pure (no IO at definition)
testPipelineComposition :: TestTree
testPipelineComposition = testCase "pipeline builds without IO" $ do
  let pipeline = Prim mockLLM |>> Lambda (\t -> pure (Right (T.words t)))
  -- Construction is pure — no IO performed here
  assertEqual "is Seq" (isSeq pipeline) True

-- Test interpret executes correctly
testInterpret :: TestTree
testInterpret = testCase "interpret executes pipeline" $ do
  let pipeline = Lambda (\x -> pure (Right (x * 2)))
  result <- runExceptT $ interpret pipeline (5 :: Int)
  assertEqual "result" result (Right 10)
```

### Property Tests (QuickCheck)

```haskell
-- State reducer associativity
prop_reducerAssociative :: StateReducer s -> s -> s -> s -> Bool
prop_reducerAssociative r s1 s2 s3 =
  r s1 (r s2 s3) == r (r s1 s2) s3

-- Pipeline identity laws
prop_seqIdLeft :: ... => RunnableTree m a b -> Property
prop_seqIdLeft t = interpret (Id |>> t) x === interpret t x
```

### Integration Tests (gated behind env flags)

```haskell
-- Only runs when OPENAI_API_KEY is set
testOpenAIIntegration :: TestTree
testOpenAIIntegration = withEnv "OPENAI_API_KEY" $ \key -> do
  model <- newOpenAI key
  result <- runExceptT $ invoke model [userMessage "Say 'OK'"] Nothing
  assertRight result
```

---

## 10. GHC Extension Requirements

| Extension | Used In | Reason |
|-----------|---------|--------|
| `GADTs` | `RunnableTree`, `StateGraph` | Typed pipeline nodes with heterogeneous types |
| `TypeFamilies` | `ChatModel`, `IsTool`, `BaseMemory` | Associated type families |
| `RankNTypes` | Existential wrappers | Higher-rank polymorphism for `DynamicTool` |
| `FlexibleContexts` | All typeclasses | Complex constraints |
| `FlexibleInstances` | Provider instances | Non-trivial instance heads |
| `ScopedTypeVariables` | Type applications | Local type annotations |
| `OverloadedStrings` | All modules | `Text` literals |
| `RecordWildCards` | Provider modules | Record deconstruction |
| `DeriveGeneric` | All data types | JSON serialization |
| `DeriveAnyClass` | All data types | Automatic instances |
| `LambdaCase` | Pattern matching | Concise case expressions |
| `TupleSections` | Utilities | Concise tuple construction |
| `DataKinds` | Type-level node IDs | Phantom type parameters |

---

## 11. Versioning & Compatibility

| GHC Version | Stack LTS | Status |
|-------------|-----------|--------|
| 9.4.8 | LTS-21.x | Supported |
| 9.6.6 | LTS-22.x | Supported |
| 9.8.4 | LTS-23.x | Supported |
| 9.10.1 | LTS-24.x | Primary |
| 9.12.x | Nightly | Experimental |

Cabal bounds will use `>=` with tested upper bounds. All packages target Hackage publication.

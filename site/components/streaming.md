---
title: Streaming
description: Real-time token, tool-call, and streaming-tool event processing with Conduit and StreamEvent.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Model</span> <span class="badge badge-primary">Langchain.Stream</span> <span class="badge badge-primary">Langchain.Tool</span>
</div>

Real-time token, incremental tool-call delta, and streaming tool execution processing using `Conduit` and `StreamEvent`.

## Key Concepts

- **Incremental Token & Tool-Call Delivery**: Providers (`OpenAI`, `Gemini`, and `Ollama`) emit `LLMChunk runId chunkText (Just toolCall)` as tool calls complete or arrive over the wire alongside text deltas.
- **Structured Lifecycle Events**: `StreamEvent` models `LLMStart`, `LLMChunk`, `LLMEnd`, `ToolStart`, `ToolChunk`, `ToolEnd`, `ToolErrorEvent`, `ChainStart`, and `ChainEnd`.
- **Streaming Tools (`StreamingTool m`)**: Tools like `streamingShellTool` emit live `ToolChunk` output lines while executing, wrapped safely with `withStreamingSecurity`.
- **Lifting Streams (`liftChatStream`)**: Bridge resource-safe `ChatStream`s (`EventStream (ExceptT LangchainError (ResourceT IO))`) into arbitrary `(MonadIO m, MonadError LangchainError m)` pipelines.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="Ollama (Local)" data-exe="stack run streamollama"}
```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Stream (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as T
import Langchain.Prelude
import System.IO (hFlush, stdout)

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  let msgs = [userMessage "What is the meaning of life"]
  res <- runResourceT $ runExceptT $ runConduit $ stream o msgs Nothing .| CL.mapM_ onStreamEvent
  case res of
    Left err -> T.putStrLn $ "\nError: " <> errorMessage err
    Right () -> T.putStrLn "\n--- Stream Finished ---"
  where
    onStreamEvent = \case
      LLMChunk _ chunk _ -> liftIO $ do
        T.putStr chunk
        hFlush stdout
      _ -> pure ()
```
:::

::: {.provider-panel data-provider="openai" data-label="OpenAI / OpenRouter" data-exe="stack run streamopenai"}
```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Stream (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)
import System.IO (hFlush, stdout)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let msgs = [userMessage "What is the meaning of life"]
  res <- runResourceT $ runExceptT $ runConduit $ stream o msgs Nothing .| CL.mapM_ onStreamEvent
  case res of
    Left err -> T.putStrLn $ "\nError: " <> errorMessage err
    Right () -> T.putStrLn "\n--- Stream Finished ---"
  where
    onStreamEvent = \case
      LLMChunk _ chunk _ -> liftIO $ do
        T.putStr chunk
        hFlush stdout
      _ -> pure ()
```
::: {.provider-panel data-provider="gemini" data-label="Google Gemini" data-exe="cabal run streamgemini"}
```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Gemini.Stream (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as T
import Gemini.Common (defaultGeminiModelName, getGeminiModel)
import Langchain.Prelude
import System.IO (hFlush, stdout)

runApp :: IO ()
runApp = do
  T.putStrLn "--- Gemini Conduit Token Streaming ---"
  g <- getGeminiModel defaultGeminiModelName
  let msgs = [userMessage "Explain why Haskell has referential transparency in 2 short paragraphs"]
  res <- runResourceT $ runExceptT $ runConduit $ stream g msgs Nothing .| CL.mapM_ onStreamEvent
  case res of
    Left err -> T.putStrLn $ "\nError: " <> errorMessage err
    Right () -> T.putStrLn "\n--- Stream Finished ---"
  where
    onStreamEvent = \case
      LLMChunk _ chunk _ -> liftIO $ do
        T.putStr chunk
        hFlush stdout
      _ -> pure ()
```
:::
:::

## Core Types & Functions

```haskell
-- | Streaming invocation on any ChatModel provider (OpenAI, Gemini, Ollama)
stream :: ChatModel model => model -> [Message] -> Maybe (ModelConfig model) -> ChatStream

-- | Stream events with incremental text and tool-call chunks
data StreamEvent
  = LLMStart       { runId :: Text, modelName :: Text, inputMessages :: [Message] }
  | LLMChunk       { runId :: Text, chunkText :: Text, toolCallDelta :: Maybe ToolCall }
  | LLMEnd         { runId :: Text, finalMessage :: Message, tokenUsage :: Maybe TokenUsage }
  | ToolStart      { runId :: Text, toolName :: Text, toolInput :: Value }
  | ToolChunk      { runId :: Text, toolName :: Text, toolChunkText :: Text }
  | ToolEnd        { runId :: Text, toolName :: Text, toolOutput :: Value }
  | ToolErrorEvent { runId :: Text, toolName :: Text, toolErrorPayload :: LangchainError }
  | ChainStart     { runId :: Text, chainName :: Text, chainInput :: Value }
  | ChainEnd       { runId :: Text, chainName :: Text, chainOutput :: Value }
  | NodeStart      { runId :: Text, nodeId :: Text, nodeState :: Value }
  | NodeEnd        { runId :: Text, nodeId :: Text, nodeState :: Value }

-- | Lift a resource-safe ChatStream into an agent or application EventStream
liftChatStream :: (MonadIO m, MonadError LangchainError m) => ChatStream -> EventStream m

-- | Streaming tools yielding incremental Text chunks
data StreamingTool m = StreamingTool
  { streamingToolDef     :: Tool m
  , streamingToolExecute :: Value -> ConduitT () Text m (Either LangchainError Text)
  }

createStreamingTool :: Monad m => Text -> Text -> Value -> (Value -> ConduitT () Text m (Either LangchainError Text)) -> StreamingTool m
fromTool            :: Monad m => Tool m -> StreamingTool m
streamingShellTool  :: MonadIO m => SecurityPolicy -> StreamingTool m
```

## Tool-Call Streaming Across Providers

`OpenAI`, `Gemini`, and `Ollama` all emit incremental `LLMChunk` events during `ChatModel.stream`:
- **OpenAI (`Langchain.Provider.OpenAI`)**: Accumulates streamed `delta.tool_calls` fragments across SSE frames and emits completed `ToolCall` values via `LLMChunk "" (Just toolCall)` prior to `LLMEnd`.
- **Gemini (`Langchain.Provider.Gemini`)**: Emits `ToolCall` deltas incrementally from Interactions v1 `function_call` SSE events alongside text deltas.
- **Ollama (`Langchain.Provider.Ollama`)**: Forwards streamed `tool_calls` directly from `ollama-haskell` chunks as `LLMChunk "" (Just toolCall)`.

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run streamollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run streamopenai
```

### Google Gemini
Set your `GEMINI_API_KEY` in environment or store in `~/.gemini_api_key`:
```bash
export GEMINI_API_KEY="AIzaSy..."
cabal run streamgemini
```

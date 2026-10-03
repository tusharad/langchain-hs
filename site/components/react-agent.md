---
title: ReAct Agent
description: Reasoning and acting loop with dynamic tool selection, memory integration, and observation feedback.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Agent.ReAct</span> <span class="badge badge-primary">Langchain.Core.Tool</span> <span class="badge badge-primary">Langchain.Memory.Core</span>
</div>

Reasoning and acting loop with dynamic tool selection, effect-polymorphic tools, persistent conversation memory, and observation feedback.

## Key Concepts

- **Thought-Action-Observation**: Iterative cycle where the LLM reasons about the problem, chooses a tool, executes it, and reflects on the observation.
- **Effect-Polymorphic Tools**: Tools run natively in the agent's monad `m` (`Tool m`), supporting `ExceptT`, `IO`, or custom application transformer stacks.
- **Conversation Memory**: Attach any `BaseMemory` instance (such as `WindowBufferMemory` or `TokenBufferMemory`) via `withMemory`.
- **Builder-Style Configuration**: Configure system prompts, model configs, iteration limits, error recovery strategies, and callbacks using composable `with*` combinators.
- **Full Reasoning Traces**: Inspect intermediate reasoning steps, tool actions, and observations via `runReActAgentWithTrace`.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run reactollama"}
```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ollama.ReAct (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Provider.Ollama
import Langchain.Tool.Calculator (calculatorTool)
import Langchain.Tool.FileSystem (readFileTool)

runApp :: IO ()
runApp = do
  writeFile "/tmp/budget.txt" "120 + 85"
  o <- newOllama "qwen3.5:2b" defaultConfig
  let tools = [readFileTool, calculatorTool]
      agent = defaultReActAgent o tools
      prompt =
        [ userMessage
            "Read the expression inside /tmp/budget.txt using read_file, then evaluate it using calculator."
        ]

  res <- runExceptT $ runReActAgent agent prompt
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right msg -> T.putStrLn $ extractMessageText msg
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run reactopenai"}
```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenAI.ReAct (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Tool.Calculator (calculatorTool)
import Langchain.Tool.FileSystem (readFileTool)
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  writeFile "/tmp/budget.txt" "120 + 85"
  o <- getOpenRouterModel defaultModelName
  let tools = [readFileTool, calculatorTool]
      agent = defaultReActAgent o tools
      prompt =
        [ userMessage
            "Read the expression inside /tmp/budget.txt using read_file, then evaluate it using calculator."
        ]

  res <- runExceptT $ runReActAgent agent prompt
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right msg -> T.putStrLn $ extractMessageText msg
```
:::
:::

## Multi-Turn Conversations with Memory

Attach conversation memory to persist dialogue history across multiple agent invocations:

```haskell
-- Create sliding window memory
mem <- newWindowBufferMemory 20 []

let agent =
      withMemory mem $
        withSystemPrompt "You are an expert financial calculation assistant." $
          defaultReActAgent model tools

-- First turn: Agent remembers user query and its final answer
res1 <- runExceptT $ runReActAgent agent [userMessage "What is 15% tip on $85?"]

-- Second turn: Agent recalls the previous answer from memory
res2 <- runExceptT $ runReActAgent agent [userMessage "Add that tip to the total bill."]
```

## Inspecting Intermediate Traces

Use `runReActAgentWithTrace` to inspect every tool action and observation in the reasoning chain:

```haskell
res <- runExceptT $ runReActAgentWithTrace agent [userMessage "Calculate 45 * 12"]
case res of
  Left err -> putStrLn $ "Agent error: " ++ show err
  Right trace -> do
    putStrLn $ "Final Answer: " ++ show (extractMessageText (traceFinalAnswer trace))
    putStrLn $ "Total Iterations: " ++ show (length (traceSteps trace))
    forM_ (traceSteps trace) $ \(step, obs) -> do
      case step of
        AgentAction msg calls -> putStrLn $ "Called: " ++ show (map toolCallName calls)
        AgentFinish msg       -> putStrLn "Finished reasoning"
```

## Core Types & API

```haskell
-- | Construct agent with sensible defaults
defaultReActAgent :: model -> [Tool m] -> ReActAgent model m

-- | Builder configuration functions
withMemory            :: (BaseMemory mem) => mem -> ReActAgent model m -> ReActAgent model m
withoutMemory         :: ReActAgent model m -> ReActAgent model m
withSystemPrompt      :: Text -> ReActAgent model m -> ReActAgent model m
withMaxIterations     :: Int -> ReActAgent model m -> ReActAgent model m
withModelConfig       :: Maybe (ModelConfig model) -> ReActAgent model m -> ReActAgent model m
withCallbackManager   :: CallbackManager -> ReActAgent model m -> ReActAgent model m
withStopCondition     :: (Message -> Bool) -> ReActAgent model m -> ReActAgent model m
withToolErrorStrategy :: ToolErrorStrategy -> ReActAgent model m -> ReActAgent model m

-- | Agent execution
runReActAgent          :: (ToolBinder model m, MonadIO m, MonadError LangchainError m) => ReActAgent model m -> [Message] -> m Message
runReActAgentWithTrace :: (ToolBinder model m, MonadIO m, MonadError LangchainError m) => ReActAgent model m -> [Message] -> m AgentTrace
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run qwen3.5:2b # or your desired model
stack run reactollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run reactopenai
```

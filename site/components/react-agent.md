---
title: ReAct Agent
description: Reasoning and acting loop with dynamic tool selection and observation feedback.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Agent.ReAct</span> <span class="badge badge-primary">Langchain.Core.Tool</span>
</div>

Reasoning and acting loop with dynamic tool selection and observation feedback.

## Key Concepts

- **Thought-Action-Observation**: Iterative cycle where the LLM reasons about the problem, chooses a tool, executes it, and reflects on the observation.
- **Stop Sequences**: Halt generation cleanly after tool actions to allow the runtime to execute Haskell functions.
- **Step Bounds**: Prevent infinite loops with max iteration limits and error recovery.

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
import Langchain.Tool.Calculator (calculatorTool)
import Langchain.Tool.FileSystem (readFileTool)

runApp :: IO ()
runApp = do
  writeFile "/tmp/budget.txt" "120 + 85"
  o <- newOllama "qwen3.5:2b" defaultConfig
  let tools = [readFileTool, calculatorTool]
      agent = createReActAgent o tools
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
      agent = createReActAgent o tools
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

## Core Types & Functions

```haskell
ChatModel m => m -> [Tool] -> Text -> IO (Either LangchainError Text)
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run reactollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run reactopenai
```

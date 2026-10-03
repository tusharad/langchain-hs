---
title: Plan-and-Execute Agent
description: Macro-level planning decoupled from execution with dynamic replanning.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Agent.PlanAndExecute</span>
</div>

Macro-level planning decoupled from execution with dynamic replanning.

## Key Concepts

- **Explicit Plan Generation**: Initial planning step produces an ordered list of tasks required to achieve a complex objective.
- **Specialized Execution**: Executors handle sub-tasks independently without losing global focus.
- **Replanning Phase**: Review completed tasks and current state to update remaining steps dynamically.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run planandexecuteollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.PlanAndExecute (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "qwen3.5:2b" defaultConfig
  let tools = [shellTool]
      executor = defaultReActAgent (bindTools tools o) tools
      agent = newPlanAndExecuteAgent o executor Nothing
      goal =
        "Use shell commands to check the operating system name (uname -s) and architecture (uname -m), then summarize the host platform."
  res <- runExceptT $ runPlanAndExecute agent goal
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run planandexecuteopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.PlanAndExecute (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let tools = [shellTool]
      executor = defaultReActAgent o tools
      agent = newPlanAndExecuteAgent o executor Nothing
      goal =
        "Use shell commands to check the operating system name (uname -s) and architecture (uname -m), then summarize the host platform."
  res <- runExceptT $ runPlanAndExecute agent goal
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
```
:::
:::

## Core Types & Functions

```haskell
data Plan = Plan { steps :: ![PlanStep] }
data PlanStep = PlanStep { stepNumber :: !Int, stepDescription :: !Text }

class StepExecutor e m where
  executeStep :: e -> Text -> m Text

newPlanAndExecuteAgent
  :: (ChatModel planner m, StepExecutor executor m)
  => planner
  -> executor
  -> Maybe (ModelConfig planner)
  -> PlanAndExecuteAgent planner executor

newPlanAndExecuteAgentWithTools
  :: (ChatModel planner m, ToolBinder executorModel m)
  => planner
  -> executorModel
  -> [Tool m]
  -> Maybe (ModelConfig planner)
  -> PlanAndExecuteAgent planner (ReActAgent executorModel m)

runPlanAndExecute
  :: (ChatModel planner m, StepExecutor executor m, MonadIO m, MonadError LangchainError m)
  => PlanAndExecuteAgent planner executor
  -> Text
  -> m Text
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run planandexecuteollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run planandexecuteopenai
```
